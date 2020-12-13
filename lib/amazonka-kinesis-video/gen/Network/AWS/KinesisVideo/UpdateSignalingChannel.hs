{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.UpdateSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the existing signaling channel. This is an asynchronous operation and takes time to complete.
--
-- If the @MessageTtlSeconds@ value is updated (either increased or reduced), it only applies to new messages sent via this channel after it's been updated. Existing messages are still expired as per the previous @MessageTtlSeconds@ value.
module Network.AWS.KinesisVideo.UpdateSignalingChannel
  ( -- * Creating a request
    UpdateSignalingChannel (..),
    mkUpdateSignalingChannel,

    -- ** Request lenses
    uscCurrentVersion,
    uscChannelARN,
    uscSingleMasterConfiguration,

    -- * Destructuring the response
    UpdateSignalingChannelResponse (..),
    mkUpdateSignalingChannelResponse,

    -- ** Response lenses
    uscrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSignalingChannel' smart constructor.
data UpdateSignalingChannel = UpdateSignalingChannel'
  { -- | The current version of the signaling channel that you want to update.
    currentVersion :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the signaling channel that you want to update.
    channelARN :: Lude.Text,
    -- | The structure containing the configuration for the @SINGLE_MASTER@ type of the signaling channel that you want to update.
    singleMasterConfiguration :: Lude.Maybe SingleMasterConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSignalingChannel' with the minimum fields required to make a request.
--
-- * 'currentVersion' - The current version of the signaling channel that you want to update.
-- * 'channelARN' - The Amazon Resource Name (ARN) of the signaling channel that you want to update.
-- * 'singleMasterConfiguration' - The structure containing the configuration for the @SINGLE_MASTER@ type of the signaling channel that you want to update.
mkUpdateSignalingChannel ::
  -- | 'currentVersion'
  Lude.Text ->
  -- | 'channelARN'
  Lude.Text ->
  UpdateSignalingChannel
mkUpdateSignalingChannel pCurrentVersion_ pChannelARN_ =
  UpdateSignalingChannel'
    { currentVersion = pCurrentVersion_,
      channelARN = pChannelARN_,
      singleMasterConfiguration = Lude.Nothing
    }

-- | The current version of the signaling channel that you want to update.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscCurrentVersion :: Lens.Lens' UpdateSignalingChannel Lude.Text
uscCurrentVersion = Lens.lens (currentVersion :: UpdateSignalingChannel -> Lude.Text) (\s a -> s {currentVersion = a} :: UpdateSignalingChannel)
{-# DEPRECATED uscCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the signaling channel that you want to update.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscChannelARN :: Lens.Lens' UpdateSignalingChannel Lude.Text
uscChannelARN = Lens.lens (channelARN :: UpdateSignalingChannel -> Lude.Text) (\s a -> s {channelARN = a} :: UpdateSignalingChannel)
{-# DEPRECATED uscChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | The structure containing the configuration for the @SINGLE_MASTER@ type of the signaling channel that you want to update.
--
-- /Note:/ Consider using 'singleMasterConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscSingleMasterConfiguration :: Lens.Lens' UpdateSignalingChannel (Lude.Maybe SingleMasterConfiguration)
uscSingleMasterConfiguration = Lens.lens (singleMasterConfiguration :: UpdateSignalingChannel -> Lude.Maybe SingleMasterConfiguration) (\s a -> s {singleMasterConfiguration = a} :: UpdateSignalingChannel)
{-# DEPRECATED uscSingleMasterConfiguration "Use generic-lens or generic-optics with 'singleMasterConfiguration' instead." #-}

instance Lude.AWSRequest UpdateSignalingChannel where
  type Rs UpdateSignalingChannel = UpdateSignalingChannelResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateSignalingChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSignalingChannel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateSignalingChannel where
  toJSON UpdateSignalingChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CurrentVersion" Lude..= currentVersion),
            Lude.Just ("ChannelARN" Lude..= channelARN),
            ("SingleMasterConfiguration" Lude..=)
              Lude.<$> singleMasterConfiguration
          ]
      )

instance Lude.ToPath UpdateSignalingChannel where
  toPath = Lude.const "/updateSignalingChannel"

instance Lude.ToQuery UpdateSignalingChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSignalingChannelResponse' smart constructor.
newtype UpdateSignalingChannelResponse = UpdateSignalingChannelResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSignalingChannelResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateSignalingChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSignalingChannelResponse
mkUpdateSignalingChannelResponse pResponseStatus_ =
  UpdateSignalingChannelResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrsResponseStatus :: Lens.Lens' UpdateSignalingChannelResponse Lude.Int
uscrsResponseStatus = Lens.lens (responseStatus :: UpdateSignalingChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSignalingChannelResponse)
{-# DEPRECATED uscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
