{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends custom events to Amazon EventBridge so that they can be matched to rules.
module Network.AWS.CloudWatchEvents.PutEvents
  ( -- * Creating a request
    PutEvents (..),
    mkPutEvents,

    -- ** Request lenses
    peEntries,

    -- * Destructuring the response
    PutEventsResponse (..),
    mkPutEventsResponse,

    -- ** Response lenses
    persFailedEntryCount,
    persEntries,
    persResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutEvents' smart constructor.
newtype PutEvents = PutEvents'
  { -- | The entry that defines an event in your system. You can specify several parameters for the entry such as the source and type of the event, resources associated with the event, and so on.
    entries :: Lude.NonEmpty PutEventsRequestEntry
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEvents' with the minimum fields required to make a request.
--
-- * 'entries' - The entry that defines an event in your system. You can specify several parameters for the entry such as the source and type of the event, resources associated with the event, and so on.
mkPutEvents ::
  -- | 'entries'
  Lude.NonEmpty PutEventsRequestEntry ->
  PutEvents
mkPutEvents pEntries_ = PutEvents' {entries = pEntries_}

-- | The entry that defines an event in your system. You can specify several parameters for the entry such as the source and type of the event, resources associated with the event, and so on.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEntries :: Lens.Lens' PutEvents (Lude.NonEmpty PutEventsRequestEntry)
peEntries = Lens.lens (entries :: PutEvents -> Lude.NonEmpty PutEventsRequestEntry) (\s a -> s {entries = a} :: PutEvents)
{-# DEPRECATED peEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

instance Lude.AWSRequest PutEvents where
  type Rs PutEvents = PutEventsResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutEventsResponse'
            Lude.<$> (x Lude..?> "FailedEntryCount")
            Lude.<*> (x Lude..?> "Entries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSEvents.PutEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutEvents where
  toJSON PutEvents' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Entries" Lude..= entries)])

instance Lude.ToPath PutEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery PutEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutEventsResponse' smart constructor.
data PutEventsResponse = PutEventsResponse'
  { -- | The number of failed entries.
    failedEntryCount :: Lude.Maybe Lude.Int,
    -- | The successfully and unsuccessfully ingested events results. If the ingestion was successful, the entry has the event ID in it. Otherwise, you can use the error code and error message to identify the problem with the entry.
    entries :: Lude.Maybe [PutEventsResultEntry],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEventsResponse' with the minimum fields required to make a request.
--
-- * 'failedEntryCount' - The number of failed entries.
-- * 'entries' - The successfully and unsuccessfully ingested events results. If the ingestion was successful, the entry has the event ID in it. Otherwise, you can use the error code and error message to identify the problem with the entry.
-- * 'responseStatus' - The response status code.
mkPutEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutEventsResponse
mkPutEventsResponse pResponseStatus_ =
  PutEventsResponse'
    { failedEntryCount = Lude.Nothing,
      entries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of failed entries.
--
-- /Note:/ Consider using 'failedEntryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
persFailedEntryCount :: Lens.Lens' PutEventsResponse (Lude.Maybe Lude.Int)
persFailedEntryCount = Lens.lens (failedEntryCount :: PutEventsResponse -> Lude.Maybe Lude.Int) (\s a -> s {failedEntryCount = a} :: PutEventsResponse)
{-# DEPRECATED persFailedEntryCount "Use generic-lens or generic-optics with 'failedEntryCount' instead." #-}

-- | The successfully and unsuccessfully ingested events results. If the ingestion was successful, the entry has the event ID in it. Otherwise, you can use the error code and error message to identify the problem with the entry.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
persEntries :: Lens.Lens' PutEventsResponse (Lude.Maybe [PutEventsResultEntry])
persEntries = Lens.lens (entries :: PutEventsResponse -> Lude.Maybe [PutEventsResultEntry]) (\s a -> s {entries = a} :: PutEventsResponse)
{-# DEPRECATED persEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
persResponseStatus :: Lens.Lens' PutEventsResponse Lude.Int
persResponseStatus = Lens.lens (responseStatus :: PutEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutEventsResponse)
{-# DEPRECATED persResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
