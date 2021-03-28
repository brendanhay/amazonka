{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceMonitoring
  ( InstanceMonitoring (..)
  -- * Smart constructor
  , mkInstanceMonitoring
  -- * Lenses
  , imInstanceId
  , imMonitoring
  ) where

import qualified Network.AWS.EC2.Types.Monitoring as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the monitoring of an instance.
--
-- /See:/ 'mkInstanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance.
  , monitoring :: Core.Maybe Types.Monitoring
    -- ^ The monitoring for the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceMonitoring' value with any optional fields omitted.
mkInstanceMonitoring
    :: InstanceMonitoring
mkInstanceMonitoring
  = InstanceMonitoring'{instanceId = Core.Nothing,
                        monitoring = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imInstanceId :: Lens.Lens' InstanceMonitoring (Core.Maybe Core.Text)
imInstanceId = Lens.field @"instanceId"
{-# INLINEABLE imInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imMonitoring :: Lens.Lens' InstanceMonitoring (Core.Maybe Types.Monitoring)
imMonitoring = Lens.field @"monitoring"
{-# INLINEABLE imMonitoring #-}
{-# DEPRECATED monitoring "Use generic-lens or generic-optics with 'monitoring' instead"  #-}

instance Core.FromXML InstanceMonitoring where
        parseXML x
          = InstanceMonitoring' Core.<$>
              (x Core..@? "instanceId") Core.<*> x Core..@? "monitoring"
