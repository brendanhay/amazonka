{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.BatchStop
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops running resources
module Network.AWS.MediaLive.BatchStop
  ( -- * Creating a request
    BatchStop (..),
    mkBatchStop,

    -- ** Request lenses
    bsChannelIds,
    bsMultiplexIds,

    -- * Destructuring the response
    BatchStopResponse (..),
    mkBatchStopResponse,

    -- ** Response lenses
    brsSuccessful,
    brsFailed,
    brsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to stop resources
--
-- /See:/ 'mkBatchStop' smart constructor.
data BatchStop = BatchStop'
  { -- | List of channel IDs
    channelIds :: Lude.Maybe [Lude.Text],
    -- | List of multiplex IDs
    multiplexIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStop' with the minimum fields required to make a request.
--
-- * 'channelIds' - List of channel IDs
-- * 'multiplexIds' - List of multiplex IDs
mkBatchStop ::
  BatchStop
mkBatchStop =
  BatchStop'
    { channelIds = Lude.Nothing,
      multiplexIds = Lude.Nothing
    }

-- | List of channel IDs
--
-- /Note:/ Consider using 'channelIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsChannelIds :: Lens.Lens' BatchStop (Lude.Maybe [Lude.Text])
bsChannelIds = Lens.lens (channelIds :: BatchStop -> Lude.Maybe [Lude.Text]) (\s a -> s {channelIds = a} :: BatchStop)
{-# DEPRECATED bsChannelIds "Use generic-lens or generic-optics with 'channelIds' instead." #-}

-- | List of multiplex IDs
--
-- /Note:/ Consider using 'multiplexIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsMultiplexIds :: Lens.Lens' BatchStop (Lude.Maybe [Lude.Text])
bsMultiplexIds = Lens.lens (multiplexIds :: BatchStop -> Lude.Maybe [Lude.Text]) (\s a -> s {multiplexIds = a} :: BatchStop)
{-# DEPRECATED bsMultiplexIds "Use generic-lens or generic-optics with 'multiplexIds' instead." #-}

instance Lude.AWSRequest BatchStop where
  type Rs BatchStop = BatchStopResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchStopResponse'
            Lude.<$> (x Lude..?> "successful" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failed" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchStop where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchStop where
  toJSON BatchStop' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("channelIds" Lude..=) Lude.<$> channelIds,
            ("multiplexIds" Lude..=) Lude.<$> multiplexIds
          ]
      )

instance Lude.ToPath BatchStop where
  toPath = Lude.const "/prod/batch/stop"

instance Lude.ToQuery BatchStop where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for BatchStopResponse
--
-- /See:/ 'mkBatchStopResponse' smart constructor.
data BatchStopResponse = BatchStopResponse'
  { -- | List of successful operations
    successful :: Lude.Maybe [BatchSuccessfulResultModel],
    -- | List of failed operations
    failed :: Lude.Maybe [BatchFailedResultModel],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStopResponse' with the minimum fields required to make a request.
--
-- * 'successful' - List of successful operations
-- * 'failed' - List of failed operations
-- * 'responseStatus' - The response status code.
mkBatchStopResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchStopResponse
mkBatchStopResponse pResponseStatus_ =
  BatchStopResponse'
    { successful = Lude.Nothing,
      failed = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of successful operations
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsSuccessful :: Lens.Lens' BatchStopResponse (Lude.Maybe [BatchSuccessfulResultModel])
brsSuccessful = Lens.lens (successful :: BatchStopResponse -> Lude.Maybe [BatchSuccessfulResultModel]) (\s a -> s {successful = a} :: BatchStopResponse)
{-# DEPRECATED brsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | List of failed operations
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsFailed :: Lens.Lens' BatchStopResponse (Lude.Maybe [BatchFailedResultModel])
brsFailed = Lens.lens (failed :: BatchStopResponse -> Lude.Maybe [BatchFailedResultModel]) (\s a -> s {failed = a} :: BatchStopResponse)
{-# DEPRECATED brsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsResponseStatus :: Lens.Lens' BatchStopResponse Lude.Int
brsResponseStatus = Lens.lens (responseStatus :: BatchStopResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchStopResponse)
{-# DEPRECATED brsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
