{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeleteChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified channel.
module Network.AWS.IoTAnalytics.DeleteChannel
  ( -- * Creating a request
    DeleteChannel (..),
    mkDeleteChannel,

    -- ** Request lenses
    dChannelName,

    -- * Destructuring the response
    DeleteChannelResponse (..),
    mkDeleteChannelResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteChannel' smart constructor.
newtype DeleteChannel = DeleteChannel'
  { -- | The name of the channel to delete.
    channelName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteChannel' with the minimum fields required to make a request.
--
-- * 'channelName' - The name of the channel to delete.
mkDeleteChannel ::
  -- | 'channelName'
  Lude.Text ->
  DeleteChannel
mkDeleteChannel pChannelName_ =
  DeleteChannel' {channelName = pChannelName_}

-- | The name of the channel to delete.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannelName :: Lens.Lens' DeleteChannel Lude.Text
dChannelName = Lens.lens (channelName :: DeleteChannel -> Lude.Text) (\s a -> s {channelName = a} :: DeleteChannel)
{-# DEPRECATED dChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

instance Lude.AWSRequest DeleteChannel where
  type Rs DeleteChannel = DeleteChannelResponse
  request = Req.delete ioTAnalyticsService
  response = Res.receiveNull DeleteChannelResponse'

instance Lude.ToHeaders DeleteChannel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    Lude.mconcat ["/channels/", Lude.toBS channelName]

instance Lude.ToQuery DeleteChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteChannelResponse' with the minimum fields required to make a request.
mkDeleteChannelResponse ::
  DeleteChannelResponse
mkDeleteChannelResponse = DeleteChannelResponse'
