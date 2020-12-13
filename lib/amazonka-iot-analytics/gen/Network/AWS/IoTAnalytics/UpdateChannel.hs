{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.UpdateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a channel.
module Network.AWS.IoTAnalytics.UpdateChannel
  ( -- * Creating a request
    UpdateChannel (..),
    mkUpdateChannel,

    -- ** Request lenses
    ucRetentionPeriod,
    ucChannelName,
    ucChannelStorage,

    -- * Destructuring the response
    UpdateChannelResponse (..),
    mkUpdateChannelResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | How long, in days, message data is kept for the channel. The retention period cannot be updated if the channel's S3 storage is customer-managed.
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    -- | The name of the channel to be updated.
    channelName :: Lude.Text,
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
    channelStorage :: Lude.Maybe ChannelStorage
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateChannel' with the minimum fields required to make a request.
--
-- * 'retentionPeriod' - How long, in days, message data is kept for the channel. The retention period cannot be updated if the channel's S3 storage is customer-managed.
-- * 'channelName' - The name of the channel to be updated.
-- * 'channelStorage' - Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
mkUpdateChannel ::
  -- | 'channelName'
  Lude.Text ->
  UpdateChannel
mkUpdateChannel pChannelName_ =
  UpdateChannel'
    { retentionPeriod = Lude.Nothing,
      channelName = pChannelName_,
      channelStorage = Lude.Nothing
    }

-- | How long, in days, message data is kept for the channel. The retention period cannot be updated if the channel's S3 storage is customer-managed.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucRetentionPeriod :: Lens.Lens' UpdateChannel (Lude.Maybe RetentionPeriod)
ucRetentionPeriod = Lens.lens (retentionPeriod :: UpdateChannel -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: UpdateChannel)
{-# DEPRECATED ucRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The name of the channel to be updated.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucChannelName :: Lens.Lens' UpdateChannel Lude.Text
ucChannelName = Lens.lens (channelName :: UpdateChannel -> Lude.Text) (\s a -> s {channelName = a} :: UpdateChannel)
{-# DEPRECATED ucChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- /Note:/ Consider using 'channelStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucChannelStorage :: Lens.Lens' UpdateChannel (Lude.Maybe ChannelStorage)
ucChannelStorage = Lens.lens (channelStorage :: UpdateChannel -> Lude.Maybe ChannelStorage) (\s a -> s {channelStorage = a} :: UpdateChannel)
{-# DEPRECATED ucChannelStorage "Use generic-lens or generic-optics with 'channelStorage' instead." #-}

instance Lude.AWSRequest UpdateChannel where
  type Rs UpdateChannel = UpdateChannelResponse
  request = Req.putJSON ioTAnalyticsService
  response = Res.receiveNull UpdateChannelResponse'

instance Lude.ToHeaders UpdateChannel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("retentionPeriod" Lude..=) Lude.<$> retentionPeriod,
            ("channelStorage" Lude..=) Lude.<$> channelStorage
          ]
      )

instance Lude.ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    Lude.mconcat ["/channels/", Lude.toBS channelName]

instance Lude.ToQuery UpdateChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateChannelResponse' with the minimum fields required to make a request.
mkUpdateChannelResponse ::
  UpdateChannelResponse
mkUpdateChannelResponse = UpdateChannelResponse'
