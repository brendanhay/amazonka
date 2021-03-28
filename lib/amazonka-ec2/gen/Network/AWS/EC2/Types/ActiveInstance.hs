{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ActiveInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ActiveInstance
  ( ActiveInstance (..)
  -- * Smart constructor
  , mkActiveInstance
  -- * Lenses
  , aiInstanceHealth
  , aiInstanceId
  , aiInstanceType
  , aiSpotInstanceRequestId
  ) where

import qualified Network.AWS.EC2.Types.InstanceHealthStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a running instance in a Spot Fleet.
--
-- /See:/ 'mkActiveInstance' smart constructor.
data ActiveInstance = ActiveInstance'
  { instanceHealth :: Core.Maybe Types.InstanceHealthStatus
    -- ^ The health status of the instance. If the status of either the instance status check or the system status check is @impaired@ , the health status of the instance is @unhealthy@ . Otherwise, the health status is @healthy@ .
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance.
  , instanceType :: Core.Maybe Core.Text
    -- ^ The instance type.
  , spotInstanceRequestId :: Core.Maybe Core.Text
    -- ^ The ID of the Spot Instance request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActiveInstance' value with any optional fields omitted.
mkActiveInstance
    :: ActiveInstance
mkActiveInstance
  = ActiveInstance'{instanceHealth = Core.Nothing,
                    instanceId = Core.Nothing, instanceType = Core.Nothing,
                    spotInstanceRequestId = Core.Nothing}

-- | The health status of the instance. If the status of either the instance status check or the system status check is @impaired@ , the health status of the instance is @unhealthy@ . Otherwise, the health status is @healthy@ .
--
-- /Note:/ Consider using 'instanceHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceHealth :: Lens.Lens' ActiveInstance (Core.Maybe Types.InstanceHealthStatus)
aiInstanceHealth = Lens.field @"instanceHealth"
{-# INLINEABLE aiInstanceHealth #-}
{-# DEPRECATED instanceHealth "Use generic-lens or generic-optics with 'instanceHealth' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceId :: Lens.Lens' ActiveInstance (Core.Maybe Core.Text)
aiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE aiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceType :: Lens.Lens' ActiveInstance (Core.Maybe Core.Text)
aiInstanceType = Lens.field @"instanceType"
{-# INLINEABLE aiInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The ID of the Spot Instance request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiSpotInstanceRequestId :: Lens.Lens' ActiveInstance (Core.Maybe Core.Text)
aiSpotInstanceRequestId = Lens.field @"spotInstanceRequestId"
{-# INLINEABLE aiSpotInstanceRequestId #-}
{-# DEPRECATED spotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead"  #-}

instance Core.FromXML ActiveInstance where
        parseXML x
          = ActiveInstance' Core.<$>
              (x Core..@? "instanceHealth") Core.<*> x Core..@? "instanceId"
                Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "spotInstanceRequestId"
