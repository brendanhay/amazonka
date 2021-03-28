{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatesMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplatesMonitoring
  ( LaunchTemplatesMonitoring (..)
  -- * Smart constructor
  , mkLaunchTemplatesMonitoring
  -- * Lenses
  , ltmEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the monitoring for the instance.
--
-- /See:/ 'mkLaunchTemplatesMonitoring' smart constructor.
newtype LaunchTemplatesMonitoring = LaunchTemplatesMonitoring'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplatesMonitoring' value with any optional fields omitted.
mkLaunchTemplatesMonitoring
    :: LaunchTemplatesMonitoring
mkLaunchTemplatesMonitoring
  = LaunchTemplatesMonitoring'{enabled = Core.Nothing}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmEnabled :: Lens.Lens' LaunchTemplatesMonitoring (Core.Maybe Core.Bool)
ltmEnabled = Lens.field @"enabled"
{-# INLINEABLE ltmEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.FromXML LaunchTemplatesMonitoring where
        parseXML x
          = LaunchTemplatesMonitoring' Core.<$> (x Core..@? "enabled")
