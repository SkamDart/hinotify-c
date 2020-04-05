module System.HINotifyC
(
)
where

import           Control.Concurrent.STM

import qualified Data.ByteString.Char8  as C

import           System.INotify

type EventStream = TQueue Event

data INotifyClient = INotifyClient { inotifyEventStream     :: EventStream
                                   , inotify                :: INotify
                                   , inotifyWatchDescriptor :: WatchDescriptor
                                   }

-- | Create our INotifyClient
mkINotifyClient :: FilePath -> IO INotifyClient
mkINotifyClient fp = do
  i <- initINotify
  es <- atomically newTQueue
  wd <- addWatch i [Delete, Modify] (C.pack fp) (\e -> do
                                                   atomically (writeTQueue es e))
  return (INotifyClient es i wd)

-- | Release resources associated to our client.
destroyINotifyClient :: INotifyClient -> IO ()
destroyINotifyClient c = do
  removeWatch $ inotifyWatchDescriptor c
  killINotify $ inotify c

-- | Read a single file system event.
-- This is a blocking call.
readEvent :: INotifyClient -> IO Event
readEvent c = atomically $ readTQueue $ inotifyEventStream c

-- | Read all file system events from queue into a list.
flushEvents :: INotifyClient -> IO [Event]
flushEvents c = atomically $ flushTQueue $ inotifyEventStream c

-- | Attempt to read an event from the queue returning `Nothing` on failure.
-- This is not a blocking call.
tryReadEvent :: INotifyClient -> IO (Maybe Event)
tryReadEvent c = atomically $ tryReadTQueue $ inotifyEventStream c

-- | Whether or not there are any events to be read from the queue.
-- If this returns true, then the queue contains atleast one element.
hasEvents :: INotifyClient -> IO Bool
hasEvents c = atomically $ isEmptyTQueue $ inotifyEventStream c

-- | Get the next event off the queue without removing it.
peekEvents :: INotifyClient -> IO Event
peekEvents c = atomically $ peekTQueue $ inotifyEventStream c

-- | Attempt to read next event off the queue without removing it.
tryPeekEvents :: INotifyClient -> IO (Maybe Event)
tryPeekEvents c = atomically $ tryPeekTQueue $ inotifyEventStream c
