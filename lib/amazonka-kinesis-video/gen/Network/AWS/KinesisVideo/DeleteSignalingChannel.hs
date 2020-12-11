{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.DeleteSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified signaling channel. @DeleteSignalingChannel@ is an asynchronous operation. If you don't specify the channel's current version, the most recent version is deleted.
module Network.AWS.KinesisVideo.DeleteSignalingChannel
  ( -- * Creating a request
    DeleteSignalingChannel (..),
    mkDeleteSignalingChannel,

    -- ** Request lenses
    dscCurrentVersion,
    dscChannelARN,

    -- * Destructuring the response
    DeleteSignalingChannelResponse (..),
    mkDeleteSignalingChannelResponse,

    -- ** Response lenses
    dscrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSignalingChannel' smart constructor.
data DeleteSignalingChannel = DeleteSignalingChannel'
  { currentVersion ::
      Lude.Maybe Lude.Text,
    channelARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSignalingChannel' with the minimum fields required to make a request.
--
-- * 'channelARN' - The Amazon Resource Name (ARN) of the signaling channel that you want to delete.
-- * 'currentVersion' - The current version of the signaling channel that you want to delete. You can obtain the current version by invoking the @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
mkDeleteSignalingChannel ::
  -- | 'channelARN'
  Lude.Text ->
  DeleteSignalingChannel
mkDeleteSignalingChannel pChannelARN_ =
  DeleteSignalingChannel'
    { currentVersion = Lude.Nothing,
      channelARN = pChannelARN_
    }

-- | The current version of the signaling channel that you want to delete. You can obtain the current version by invoking the @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscCurrentVersion :: Lens.Lens' DeleteSignalingChannel (Lude.Maybe Lude.Text)
dscCurrentVersion = Lens.lens (currentVersion :: DeleteSignalingChannel -> Lude.Maybe Lude.Text) (\s a -> s {currentVersion = a} :: DeleteSignalingChannel)
{-# DEPRECATED dscCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the signaling channel that you want to delete.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscChannelARN :: Lens.Lens' DeleteSignalingChannel Lude.Text
dscChannelARN = Lens.lens (channelARN :: DeleteSignalingChannel -> Lude.Text) (\s a -> s {channelARN = a} :: DeleteSignalingChannel)
{-# DEPRECATED dscChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

instance Lude.AWSRequest DeleteSignalingChannel where
  type Rs DeleteSignalingChannel = DeleteSignalingChannelResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteSignalingChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSignalingChannel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DeleteSignalingChannel where
  toJSON DeleteSignalingChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CurrentVersion" Lude..=) Lude.<$> currentVersion,
            Lude.Just ("ChannelARN" Lude..= channelARN)
          ]
      )

instance Lude.ToPath DeleteSignalingChannel where
  toPath = Lude.const "/deleteSignalingChannel"

instance Lude.ToQuery DeleteSignalingChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSignalingChannelResponse' smart constructor.
newtype DeleteSignalingChannelResponse = DeleteSignalingChannelResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSignalingChannelResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteSignalingChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSignalingChannelResponse
mkDeleteSignalingChannelResponse pResponseStatus_ =
  DeleteSignalingChannelResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsResponseStatus :: Lens.Lens' DeleteSignalingChannelResponse Lude.Int
dscrsResponseStatus = Lens.lens (responseStatus :: DeleteSignalingChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSignalingChannelResponse)
{-# DEPRECATED dscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
