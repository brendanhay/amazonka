{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateChannelClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the class of the channel.
module Network.AWS.MediaLive.UpdateChannelClass
  ( -- * Creating a request
    UpdateChannelClass (..),
    mkUpdateChannelClass,

    -- ** Request lenses
    uccDestinations,
    uccChannelId,
    uccChannelClass,

    -- * Destructuring the response
    UpdateChannelClassResponse (..),
    mkUpdateChannelClassResponse,

    -- ** Response lenses
    uccrsChannel,
    uccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Channel class that the channel should be updated to.
--
-- /See:/ 'mkUpdateChannelClass' smart constructor.
data UpdateChannelClass = UpdateChannelClass'
  { destinations ::
      Lude.Maybe [OutputDestination],
    channelId :: Lude.Text,
    channelClass :: ChannelClass
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateChannelClass' with the minimum fields required to make a request.
--
-- * 'channelClass' - The channel class that you wish to update this channel to use.
-- * 'channelId' - Channel Id of the channel whose class should be updated.
-- * 'destinations' - A list of output destinations for this channel.
mkUpdateChannelClass ::
  -- | 'channelId'
  Lude.Text ->
  -- | 'channelClass'
  ChannelClass ->
  UpdateChannelClass
mkUpdateChannelClass pChannelId_ pChannelClass_ =
  UpdateChannelClass'
    { destinations = Lude.Nothing,
      channelId = pChannelId_,
      channelClass = pChannelClass_
    }

-- | A list of output destinations for this channel.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccDestinations :: Lens.Lens' UpdateChannelClass (Lude.Maybe [OutputDestination])
uccDestinations = Lens.lens (destinations :: UpdateChannelClass -> Lude.Maybe [OutputDestination]) (\s a -> s {destinations = a} :: UpdateChannelClass)
{-# DEPRECATED uccDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | Channel Id of the channel whose class should be updated.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccChannelId :: Lens.Lens' UpdateChannelClass Lude.Text
uccChannelId = Lens.lens (channelId :: UpdateChannelClass -> Lude.Text) (\s a -> s {channelId = a} :: UpdateChannelClass)
{-# DEPRECATED uccChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The channel class that you wish to update this channel to use.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccChannelClass :: Lens.Lens' UpdateChannelClass ChannelClass
uccChannelClass = Lens.lens (channelClass :: UpdateChannelClass -> ChannelClass) (\s a -> s {channelClass = a} :: UpdateChannelClass)
{-# DEPRECATED uccChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

instance Lude.AWSRequest UpdateChannelClass where
  type Rs UpdateChannelClass = UpdateChannelClassResponse
  request = Req.putJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateChannelClassResponse'
            Lude.<$> (x Lude..?> "channel") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateChannelClass where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateChannelClass where
  toJSON UpdateChannelClass' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("destinations" Lude..=) Lude.<$> destinations,
            Lude.Just ("channelClass" Lude..= channelClass)
          ]
      )

instance Lude.ToPath UpdateChannelClass where
  toPath UpdateChannelClass' {..} =
    Lude.mconcat
      ["/prod/channels/", Lude.toBS channelId, "/channelClass"]

instance Lude.ToQuery UpdateChannelClass where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for UpdateChannelClassResponse
--
-- /See:/ 'mkUpdateChannelClassResponse' smart constructor.
data UpdateChannelClassResponse = UpdateChannelClassResponse'
  { channel ::
      Lude.Maybe Channel,
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

-- | Creates a value of 'UpdateChannelClassResponse' with the minimum fields required to make a request.
--
-- * 'channel' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateChannelClassResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateChannelClassResponse
mkUpdateChannelClassResponse pResponseStatus_ =
  UpdateChannelClassResponse'
    { channel = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrsChannel :: Lens.Lens' UpdateChannelClassResponse (Lude.Maybe Channel)
uccrsChannel = Lens.lens (channel :: UpdateChannelClassResponse -> Lude.Maybe Channel) (\s a -> s {channel = a} :: UpdateChannelClassResponse)
{-# DEPRECATED uccrsChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrsResponseStatus :: Lens.Lens' UpdateChannelClassResponse Lude.Int
uccrsResponseStatus = Lens.lens (responseStatus :: UpdateChannelClassResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateChannelClassResponse)
{-# DEPRECATED uccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
