{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ChannelsResponse
  ( ChannelsResponse (..)
  -- * Smart constructor
  , mkChannelsResponse
  -- * Lenses
  , crChannels
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ChannelResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the general settings and status of all channels for an application, including channels that aren't enabled for the application.
--
-- /See:/ 'mkChannelsResponse' smart constructor.
newtype ChannelsResponse = ChannelsResponse'
  { channels :: Core.HashMap Core.Text Types.ChannelResponse
    -- ^ A map that contains a multipart response for each channel. For each item in this object, the ChannelType is the key and the Channel is the value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelsResponse' value with any optional fields omitted.
mkChannelsResponse
    :: ChannelsResponse
mkChannelsResponse = ChannelsResponse'{channels = Core.mempty}

-- | A map that contains a multipart response for each channel. For each item in this object, the ChannelType is the key and the Channel is the value.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crChannels :: Lens.Lens' ChannelsResponse (Core.HashMap Core.Text Types.ChannelResponse)
crChannels = Lens.field @"channels"
{-# INLINEABLE crChannels #-}
{-# DEPRECATED channels "Use generic-lens or generic-optics with 'channels' instead"  #-}

instance Core.FromJSON ChannelsResponse where
        parseJSON
          = Core.withObject "ChannelsResponse" Core.$
              \ x ->
                ChannelsResponse' Core.<$>
                  (x Core..:? "Channels" Core..!= Core.mempty)
