{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.ChannelStatistics
  ( ChannelStatistics (..)
  -- * Smart constructor
  , mkChannelStatistics
  -- * Lenses
  , csSize
  ) where

import qualified Network.AWS.IoTAnalytics.Types.EstimatedResourceSize as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Statistics information about the channel.
--
-- /See:/ 'mkChannelStatistics' smart constructor.
newtype ChannelStatistics = ChannelStatistics'
  { size :: Core.Maybe Types.EstimatedResourceSize
    -- ^ The estimated size of the channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'ChannelStatistics' value with any optional fields omitted.
mkChannelStatistics
    :: ChannelStatistics
mkChannelStatistics = ChannelStatistics'{size = Core.Nothing}

-- | The estimated size of the channel.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSize :: Lens.Lens' ChannelStatistics (Core.Maybe Types.EstimatedResourceSize)
csSize = Lens.field @"size"
{-# INLINEABLE csSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

instance Core.FromJSON ChannelStatistics where
        parseJSON
          = Core.withObject "ChannelStatistics" Core.$
              \ x -> ChannelStatistics' Core.<$> (x Core..:? "size")
