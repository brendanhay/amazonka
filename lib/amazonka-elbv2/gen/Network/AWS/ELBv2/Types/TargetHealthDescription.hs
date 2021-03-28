{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealthDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TargetHealthDescription
  ( TargetHealthDescription (..)
  -- * Smart constructor
  , mkTargetHealthDescription
  -- * Lenses
  , thdHealthCheckPort
  , thdTarget
  , thdTargetHealth
  ) where

import qualified Network.AWS.ELBv2.Types.HealthCheckPort as Types
import qualified Network.AWS.ELBv2.Types.TargetDescription as Types
import qualified Network.AWS.ELBv2.Types.TargetHealth as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the health of a target.
--
-- /See:/ 'mkTargetHealthDescription' smart constructor.
data TargetHealthDescription = TargetHealthDescription'
  { healthCheckPort :: Core.Maybe Types.HealthCheckPort
    -- ^ The port to use to connect with the target.
  , target :: Core.Maybe Types.TargetDescription
    -- ^ The description of the target.
  , targetHealth :: Core.Maybe Types.TargetHealth
    -- ^ The health information for the target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetHealthDescription' value with any optional fields omitted.
mkTargetHealthDescription
    :: TargetHealthDescription
mkTargetHealthDescription
  = TargetHealthDescription'{healthCheckPort = Core.Nothing,
                             target = Core.Nothing, targetHealth = Core.Nothing}

-- | The port to use to connect with the target.
--
-- /Note:/ Consider using 'healthCheckPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thdHealthCheckPort :: Lens.Lens' TargetHealthDescription (Core.Maybe Types.HealthCheckPort)
thdHealthCheckPort = Lens.field @"healthCheckPort"
{-# INLINEABLE thdHealthCheckPort #-}
{-# DEPRECATED healthCheckPort "Use generic-lens or generic-optics with 'healthCheckPort' instead"  #-}

-- | The description of the target.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thdTarget :: Lens.Lens' TargetHealthDescription (Core.Maybe Types.TargetDescription)
thdTarget = Lens.field @"target"
{-# INLINEABLE thdTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

-- | The health information for the target.
--
-- /Note:/ Consider using 'targetHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thdTargetHealth :: Lens.Lens' TargetHealthDescription (Core.Maybe Types.TargetHealth)
thdTargetHealth = Lens.field @"targetHealth"
{-# INLINEABLE thdTargetHealth #-}
{-# DEPRECATED targetHealth "Use generic-lens or generic-optics with 'targetHealth' instead"  #-}

instance Core.FromXML TargetHealthDescription where
        parseXML x
          = TargetHealthDescription' Core.<$>
              (x Core..@? "HealthCheckPort") Core.<*> x Core..@? "Target"
                Core.<*> x Core..@? "TargetHealth"
