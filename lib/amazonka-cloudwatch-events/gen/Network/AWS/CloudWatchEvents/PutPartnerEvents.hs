{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutPartnerEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is used by SaaS partners to write events to a customer's partner event bus. AWS customers do not use this operation.
module Network.AWS.CloudWatchEvents.PutPartnerEvents
  ( -- * Creating a request
    PutPartnerEvents (..),
    mkPutPartnerEvents,

    -- ** Request lenses
    ppeEntries,

    -- * Destructuring the response
    PutPartnerEventsResponse (..),
    mkPutPartnerEventsResponse,

    -- ** Response lenses
    ppersFailedEntryCount,
    ppersEntries,
    ppersResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutPartnerEvents' smart constructor.
newtype PutPartnerEvents = PutPartnerEvents'
  { entries ::
      Lude.NonEmpty PutPartnerEventsRequestEntry
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPartnerEvents' with the minimum fields required to make a request.
--
-- * 'entries' - The list of events to write to the event bus.
mkPutPartnerEvents ::
  -- | 'entries'
  Lude.NonEmpty PutPartnerEventsRequestEntry ->
  PutPartnerEvents
mkPutPartnerEvents pEntries_ =
  PutPartnerEvents' {entries = pEntries_}

-- | The list of events to write to the event bus.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppeEntries :: Lens.Lens' PutPartnerEvents (Lude.NonEmpty PutPartnerEventsRequestEntry)
ppeEntries = Lens.lens (entries :: PutPartnerEvents -> Lude.NonEmpty PutPartnerEventsRequestEntry) (\s a -> s {entries = a} :: PutPartnerEvents)
{-# DEPRECATED ppeEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

instance Lude.AWSRequest PutPartnerEvents where
  type Rs PutPartnerEvents = PutPartnerEventsResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutPartnerEventsResponse'
            Lude.<$> (x Lude..?> "FailedEntryCount")
            Lude.<*> (x Lude..?> "Entries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutPartnerEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.PutPartnerEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutPartnerEvents where
  toJSON PutPartnerEvents' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Entries" Lude..= entries)])

instance Lude.ToPath PutPartnerEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery PutPartnerEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutPartnerEventsResponse' smart constructor.
data PutPartnerEventsResponse = PutPartnerEventsResponse'
  { failedEntryCount ::
      Lude.Maybe Lude.Int,
    entries ::
      Lude.Maybe [PutPartnerEventsResultEntry],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPartnerEventsResponse' with the minimum fields required to make a request.
--
-- * 'entries' - The list of events from this operation that were successfully written to the partner event bus.
-- * 'failedEntryCount' - The number of events from this operation that could not be written to the partner event bus.
-- * 'responseStatus' - The response status code.
mkPutPartnerEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutPartnerEventsResponse
mkPutPartnerEventsResponse pResponseStatus_ =
  PutPartnerEventsResponse'
    { failedEntryCount = Lude.Nothing,
      entries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of events from this operation that could not be written to the partner event bus.
--
-- /Note:/ Consider using 'failedEntryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppersFailedEntryCount :: Lens.Lens' PutPartnerEventsResponse (Lude.Maybe Lude.Int)
ppersFailedEntryCount = Lens.lens (failedEntryCount :: PutPartnerEventsResponse -> Lude.Maybe Lude.Int) (\s a -> s {failedEntryCount = a} :: PutPartnerEventsResponse)
{-# DEPRECATED ppersFailedEntryCount "Use generic-lens or generic-optics with 'failedEntryCount' instead." #-}

-- | The list of events from this operation that were successfully written to the partner event bus.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppersEntries :: Lens.Lens' PutPartnerEventsResponse (Lude.Maybe [PutPartnerEventsResultEntry])
ppersEntries = Lens.lens (entries :: PutPartnerEventsResponse -> Lude.Maybe [PutPartnerEventsResultEntry]) (\s a -> s {entries = a} :: PutPartnerEventsResponse)
{-# DEPRECATED ppersEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppersResponseStatus :: Lens.Lens' PutPartnerEventsResponse Lude.Int
ppersResponseStatus = Lens.lens (responseStatus :: PutPartnerEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutPartnerEventsResponse)
{-# DEPRECATED ppersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
