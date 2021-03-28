{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
  ( LaunchTemplatesMonitoringRequest (..)
  -- * Smart constructor
  , mkLaunchTemplatesMonitoringRequest
  -- * Lenses
  , ltmrEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the monitoring for the instance.
--
-- /See:/ 'mkLaunchTemplatesMonitoringRequest' smart constructor.
newtype LaunchTemplatesMonitoringRequest = LaunchTemplatesMonitoringRequest'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Specify @true@ to enable detailed monitoring. Otherwise, basic monitoring is enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplatesMonitoringRequest' value with any optional fields omitted.
mkLaunchTemplatesMonitoringRequest
    :: LaunchTemplatesMonitoringRequest
mkLaunchTemplatesMonitoringRequest
  = LaunchTemplatesMonitoringRequest'{enabled = Core.Nothing}

-- | Specify @true@ to enable detailed monitoring. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrEnabled :: Lens.Lens' LaunchTemplatesMonitoringRequest (Core.Maybe Core.Bool)
ltmrEnabled = Lens.field @"enabled"
{-# INLINEABLE ltmrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.ToQuery LaunchTemplatesMonitoringRequest where
        toQuery LaunchTemplatesMonitoringRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled
