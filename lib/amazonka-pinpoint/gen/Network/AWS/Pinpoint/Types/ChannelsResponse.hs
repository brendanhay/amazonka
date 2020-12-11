-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ChannelsResponse
  ( ChannelsResponse (..),

    -- * Smart constructor
    mkChannelsResponse,

    -- * Lenses
    cChannels,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the general settings and status of all channels for an application, including channels that aren't enabled for the application.
--
-- /See:/ 'mkChannelsResponse' smart constructor.
newtype ChannelsResponse = ChannelsResponse'
  { channels ::
      Lude.HashMap Lude.Text (ChannelResponse)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelsResponse' with the minimum fields required to make a request.
--
-- * 'channels' - A map that contains a multipart response for each channel. For each item in this object, the ChannelType is the key and the Channel is the value.
mkChannelsResponse ::
  ChannelsResponse
mkChannelsResponse = ChannelsResponse' {channels = Lude.mempty}

-- | A map that contains a multipart response for each channel. For each item in this object, the ChannelType is the key and the Channel is the value.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cChannels :: Lens.Lens' ChannelsResponse (Lude.HashMap Lude.Text (ChannelResponse))
cChannels = Lens.lens (channels :: ChannelsResponse -> Lude.HashMap Lude.Text (ChannelResponse)) (\s a -> s {channels = a} :: ChannelsResponse)
{-# DEPRECATED cChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

instance Lude.FromJSON ChannelsResponse where
  parseJSON =
    Lude.withObject
      "ChannelsResponse"
      ( \x ->
          ChannelsResponse'
            Lude.<$> (x Lude..:? "Channels" Lude..!= Lude.mempty)
      )
