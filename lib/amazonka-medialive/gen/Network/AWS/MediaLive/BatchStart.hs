{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.BatchStart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts existing resources
module Network.AWS.MediaLive.BatchStart
  ( -- * Creating a request
    BatchStart (..),
    mkBatchStart,

    -- ** Request lenses
    bsChannelIds,
    bsMultiplexIds,

    -- * Destructuring the response
    BatchStartResponse (..),
    mkBatchStartResponse,

    -- ** Response lenses
    bsrsSuccessful,
    bsrsFailed,
    bsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to start resources
--
-- /See:/ 'mkBatchStart' smart constructor.
data BatchStart = BatchStart'
  { channelIds :: Lude.Maybe [Lude.Text],
    multiplexIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStart' with the minimum fields required to make a request.
--
-- * 'channelIds' - List of channel IDs
-- * 'multiplexIds' - List of multiplex IDs
mkBatchStart ::
  BatchStart
mkBatchStart =
  BatchStart'
    { channelIds = Lude.Nothing,
      multiplexIds = Lude.Nothing
    }

-- | List of channel IDs
--
-- /Note:/ Consider using 'channelIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsChannelIds :: Lens.Lens' BatchStart (Lude.Maybe [Lude.Text])
bsChannelIds = Lens.lens (channelIds :: BatchStart -> Lude.Maybe [Lude.Text]) (\s a -> s {channelIds = a} :: BatchStart)
{-# DEPRECATED bsChannelIds "Use generic-lens or generic-optics with 'channelIds' instead." #-}

-- | List of multiplex IDs
--
-- /Note:/ Consider using 'multiplexIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsMultiplexIds :: Lens.Lens' BatchStart (Lude.Maybe [Lude.Text])
bsMultiplexIds = Lens.lens (multiplexIds :: BatchStart -> Lude.Maybe [Lude.Text]) (\s a -> s {multiplexIds = a} :: BatchStart)
{-# DEPRECATED bsMultiplexIds "Use generic-lens or generic-optics with 'multiplexIds' instead." #-}

instance Lude.AWSRequest BatchStart where
  type Rs BatchStart = BatchStartResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchStartResponse'
            Lude.<$> (x Lude..?> "successful" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failed" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchStart where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchStart where
  toJSON BatchStart' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("channelIds" Lude..=) Lude.<$> channelIds,
            ("multiplexIds" Lude..=) Lude.<$> multiplexIds
          ]
      )

instance Lude.ToPath BatchStart where
  toPath = Lude.const "/prod/batch/start"

instance Lude.ToQuery BatchStart where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for BatchStartResponse
--
-- /See:/ 'mkBatchStartResponse' smart constructor.
data BatchStartResponse = BatchStartResponse'
  { successful ::
      Lude.Maybe [BatchSuccessfulResultModel],
    failed :: Lude.Maybe [BatchFailedResultModel],
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

-- | Creates a value of 'BatchStartResponse' with the minimum fields required to make a request.
--
-- * 'failed' - List of failed operations
-- * 'responseStatus' - The response status code.
-- * 'successful' - List of successful operations
mkBatchStartResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchStartResponse
mkBatchStartResponse pResponseStatus_ =
  BatchStartResponse'
    { successful = Lude.Nothing,
      failed = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of successful operations
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrsSuccessful :: Lens.Lens' BatchStartResponse (Lude.Maybe [BatchSuccessfulResultModel])
bsrsSuccessful = Lens.lens (successful :: BatchStartResponse -> Lude.Maybe [BatchSuccessfulResultModel]) (\s a -> s {successful = a} :: BatchStartResponse)
{-# DEPRECATED bsrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | List of failed operations
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrsFailed :: Lens.Lens' BatchStartResponse (Lude.Maybe [BatchFailedResultModel])
bsrsFailed = Lens.lens (failed :: BatchStartResponse -> Lude.Maybe [BatchFailedResultModel]) (\s a -> s {failed = a} :: BatchStartResponse)
{-# DEPRECATED bsrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrsResponseStatus :: Lens.Lens' BatchStartResponse Lude.Int
bsrsResponseStatus = Lens.lens (responseStatus :: BatchStartResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchStartResponse)
{-# DEPRECATED bsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
