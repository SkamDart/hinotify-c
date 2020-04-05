#ifndef __HINOTIFY_H__
#define __HINOTIFY_H__

// C Respresentation of the Haskell HINotifyClient data structure.
typedef struct
{
  void* client;
} hinotify_client;

int hinotify_init();

void hinotify_destroy();

void hinotify_read_event();

void hinotify_flush_events();

void hinotify_try_read_event();

bool hinotify_has_events();

void hinotify_peek_event();

// Attempts to read an event off the queue without actually removing it.
// On success, the pointer is initialized and 0 is returned.
// On failure, a non zero value is returned and the pointer is left null.
int hinotify_try_peek_event();

#endif // __HINOTIFY_H__
