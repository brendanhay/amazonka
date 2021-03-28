{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelEgressEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ChannelEgressEndpoint
  ( ChannelEgressEndpoint (..)
  -- * Smart constructor
  , mkChannelEgressEndpoint
  -- * Lenses
  , ceeSourceIp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for ChannelEgressEndpoint
--
-- /See:/ 'mkChannelEgressEndpoint' smart constructor.
newtype ChannelEgressEndpoint = ChannelEgressEndpoint'
  { sourceIp :: Core.Maybe Core.Text
    -- ^ Public IP of where a channel's output comes from
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelEgressEndpoint' value with any optional fields omitted.
mkChannelEgressEndpoint
    :: ChannelEgressEndpoint
mkChannelEgressEndpoint
  = ChannelEgressEndpoint'{sourceIp = Core.Nothing}

-- | Public IP of where a channel's output comes from
--
-- /Note:/ Consider using 'sourceIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceeSourceIp :: Lens.Lens' ChannelEgressEndpoint (Core.Maybe Core.Text)
ceeSourceIp = Lens.field @"sourceIp"
{-# INLINEABLE ceeSourceIp #-}
{-# DEPRECATED sourceIp "Use generic-lens or generic-optics with 'sourceIp' instead"  #-}

instance Core.FromJSON ChannelEgressEndpoint where
        parseJSON
          = Core.withObject "ChannelEgressEndpoint" Core.$
              \ x -> ChannelEgressEndpoint' Core.<$> (x Core..:? "sourceIp")
