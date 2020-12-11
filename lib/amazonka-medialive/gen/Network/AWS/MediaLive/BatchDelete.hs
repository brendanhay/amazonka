{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.BatchDelete
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts delete of resources.
module Network.AWS.MediaLive.BatchDelete
  ( -- * Creating a request
    BatchDelete (..),
    mkBatchDelete,

    -- ** Request lenses
    bdChannelIds,
    bdInputIds,
    bdMultiplexIds,
    bdInputSecurityGroupIds,

    -- * Destructuring the response
    BatchDeleteResponse (..),
    mkBatchDeleteResponse,

    -- ** Response lenses
    bdrsSuccessful,
    bdrsFailed,
    bdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to delete resources
--
-- /See:/ 'mkBatchDelete' smart constructor.
data BatchDelete = BatchDelete'
  { channelIds ::
      Lude.Maybe [Lude.Text],
    inputIds :: Lude.Maybe [Lude.Text],
    multiplexIds :: Lude.Maybe [Lude.Text],
    inputSecurityGroupIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDelete' with the minimum fields required to make a request.
--
-- * 'channelIds' - List of channel IDs
-- * 'inputIds' - List of input IDs
-- * 'inputSecurityGroupIds' - List of input security group IDs
-- * 'multiplexIds' - List of multiplex IDs
mkBatchDelete ::
  BatchDelete
mkBatchDelete =
  BatchDelete'
    { channelIds = Lude.Nothing,
      inputIds = Lude.Nothing,
      multiplexIds = Lude.Nothing,
      inputSecurityGroupIds = Lude.Nothing
    }

-- | List of channel IDs
--
-- /Note:/ Consider using 'channelIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdChannelIds :: Lens.Lens' BatchDelete (Lude.Maybe [Lude.Text])
bdChannelIds = Lens.lens (channelIds :: BatchDelete -> Lude.Maybe [Lude.Text]) (\s a -> s {channelIds = a} :: BatchDelete)
{-# DEPRECATED bdChannelIds "Use generic-lens or generic-optics with 'channelIds' instead." #-}

-- | List of input IDs
--
-- /Note:/ Consider using 'inputIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdInputIds :: Lens.Lens' BatchDelete (Lude.Maybe [Lude.Text])
bdInputIds = Lens.lens (inputIds :: BatchDelete -> Lude.Maybe [Lude.Text]) (\s a -> s {inputIds = a} :: BatchDelete)
{-# DEPRECATED bdInputIds "Use generic-lens or generic-optics with 'inputIds' instead." #-}

-- | List of multiplex IDs
--
-- /Note:/ Consider using 'multiplexIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdMultiplexIds :: Lens.Lens' BatchDelete (Lude.Maybe [Lude.Text])
bdMultiplexIds = Lens.lens (multiplexIds :: BatchDelete -> Lude.Maybe [Lude.Text]) (\s a -> s {multiplexIds = a} :: BatchDelete)
{-# DEPRECATED bdMultiplexIds "Use generic-lens or generic-optics with 'multiplexIds' instead." #-}

-- | List of input security group IDs
--
-- /Note:/ Consider using 'inputSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdInputSecurityGroupIds :: Lens.Lens' BatchDelete (Lude.Maybe [Lude.Text])
bdInputSecurityGroupIds = Lens.lens (inputSecurityGroupIds :: BatchDelete -> Lude.Maybe [Lude.Text]) (\s a -> s {inputSecurityGroupIds = a} :: BatchDelete)
{-# DEPRECATED bdInputSecurityGroupIds "Use generic-lens or generic-optics with 'inputSecurityGroupIds' instead." #-}

instance Lude.AWSRequest BatchDelete where
  type Rs BatchDelete = BatchDeleteResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDeleteResponse'
            Lude.<$> (x Lude..?> "successful" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failed" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDelete where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDelete where
  toJSON BatchDelete' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("channelIds" Lude..=) Lude.<$> channelIds,
            ("inputIds" Lude..=) Lude.<$> inputIds,
            ("multiplexIds" Lude..=) Lude.<$> multiplexIds,
            ("inputSecurityGroupIds" Lude..=) Lude.<$> inputSecurityGroupIds
          ]
      )

instance Lude.ToPath BatchDelete where
  toPath = Lude.const "/prod/batch/delete"

instance Lude.ToQuery BatchDelete where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for BatchDeleteResponse
--
-- /See:/ 'mkBatchDeleteResponse' smart constructor.
data BatchDeleteResponse = BatchDeleteResponse'
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

-- | Creates a value of 'BatchDeleteResponse' with the minimum fields required to make a request.
--
-- * 'failed' - List of failed operations
-- * 'responseStatus' - The response status code.
-- * 'successful' - List of successful operations
mkBatchDeleteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeleteResponse
mkBatchDeleteResponse pResponseStatus_ =
  BatchDeleteResponse'
    { successful = Lude.Nothing,
      failed = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of successful operations
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrsSuccessful :: Lens.Lens' BatchDeleteResponse (Lude.Maybe [BatchSuccessfulResultModel])
bdrsSuccessful = Lens.lens (successful :: BatchDeleteResponse -> Lude.Maybe [BatchSuccessfulResultModel]) (\s a -> s {successful = a} :: BatchDeleteResponse)
{-# DEPRECATED bdrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | List of failed operations
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrsFailed :: Lens.Lens' BatchDeleteResponse (Lude.Maybe [BatchFailedResultModel])
bdrsFailed = Lens.lens (failed :: BatchDeleteResponse -> Lude.Maybe [BatchFailedResultModel]) (\s a -> s {failed = a} :: BatchDeleteResponse)
{-# DEPRECATED bdrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrsResponseStatus :: Lens.Lens' BatchDeleteResponse Lude.Int
bdrsResponseStatus = Lens.lens (responseStatus :: BatchDeleteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeleteResponse)
{-# DEPRECATED bdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
