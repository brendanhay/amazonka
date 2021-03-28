{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceStatus
  ( InstanceStatus (..)
  -- * Smart constructor
  , mkInstanceStatus
  -- * Lenses
  , issAvailabilityZone
  , issEvents
  , issInstanceId
  , issInstanceState
  , issInstanceStatus
  , issOutpostArn
  , issSystemStatus
  ) where

import qualified Network.AWS.EC2.Types.InstanceState as Types
import qualified Network.AWS.EC2.Types.InstanceStatusEvent as Types
import qualified Network.AWS.EC2.Types.InstanceStatusSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the status of an instance.
--
-- /See:/ 'mkInstanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone of the instance.
  , events :: Core.Maybe [Types.InstanceStatusEvent]
    -- ^ Any scheduled events associated with the instance.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance.
  , instanceState :: Core.Maybe Types.InstanceState
    -- ^ The intended state of the instance. 'DescribeInstanceStatus' requires that an instance be in the @running@ state.
  , instanceStatus :: Core.Maybe Types.InstanceStatusSummary
    -- ^ Reports impaired functionality that stems from issues internal to the instance, such as impaired reachability.
  , outpostArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Outpost.
  , systemStatus :: Core.Maybe Types.InstanceStatusSummary
    -- ^ Reports impaired functionality that stems from issues related to the systems that support an instance, such as hardware failures and network connectivity problems.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceStatus' value with any optional fields omitted.
mkInstanceStatus
    :: InstanceStatus
mkInstanceStatus
  = InstanceStatus'{availabilityZone = Core.Nothing,
                    events = Core.Nothing, instanceId = Core.Nothing,
                    instanceState = Core.Nothing, instanceStatus = Core.Nothing,
                    outpostArn = Core.Nothing, systemStatus = Core.Nothing}

-- | The Availability Zone of the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issAvailabilityZone :: Lens.Lens' InstanceStatus (Core.Maybe Core.Text)
issAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE issAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | Any scheduled events associated with the instance.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issEvents :: Lens.Lens' InstanceStatus (Core.Maybe [Types.InstanceStatusEvent])
issEvents = Lens.field @"events"
{-# INLINEABLE issEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issInstanceId :: Lens.Lens' InstanceStatus (Core.Maybe Core.Text)
issInstanceId = Lens.field @"instanceId"
{-# INLINEABLE issInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The intended state of the instance. 'DescribeInstanceStatus' requires that an instance be in the @running@ state.
--
-- /Note:/ Consider using 'instanceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issInstanceState :: Lens.Lens' InstanceStatus (Core.Maybe Types.InstanceState)
issInstanceState = Lens.field @"instanceState"
{-# INLINEABLE issInstanceState #-}
{-# DEPRECATED instanceState "Use generic-lens or generic-optics with 'instanceState' instead"  #-}

-- | Reports impaired functionality that stems from issues internal to the instance, such as impaired reachability.
--
-- /Note:/ Consider using 'instanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issInstanceStatus :: Lens.Lens' InstanceStatus (Core.Maybe Types.InstanceStatusSummary)
issInstanceStatus = Lens.field @"instanceStatus"
{-# INLINEABLE issInstanceStatus #-}
{-# DEPRECATED instanceStatus "Use generic-lens or generic-optics with 'instanceStatus' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issOutpostArn :: Lens.Lens' InstanceStatus (Core.Maybe Core.Text)
issOutpostArn = Lens.field @"outpostArn"
{-# INLINEABLE issOutpostArn #-}
{-# DEPRECATED outpostArn "Use generic-lens or generic-optics with 'outpostArn' instead"  #-}

-- | Reports impaired functionality that stems from issues related to the systems that support an instance, such as hardware failures and network connectivity problems.
--
-- /Note:/ Consider using 'systemStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issSystemStatus :: Lens.Lens' InstanceStatus (Core.Maybe Types.InstanceStatusSummary)
issSystemStatus = Lens.field @"systemStatus"
{-# INLINEABLE issSystemStatus #-}
{-# DEPRECATED systemStatus "Use generic-lens or generic-optics with 'systemStatus' instead"  #-}

instance Core.FromXML InstanceStatus where
        parseXML x
          = InstanceStatus' Core.<$>
              (x Core..@? "availabilityZone") Core.<*>
                x Core..@? "eventsSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "instanceId"
                Core.<*> x Core..@? "instanceState"
                Core.<*> x Core..@? "instanceStatus"
                Core.<*> x Core..@? "outpostArn"
                Core.<*> x Core..@? "systemStatus"
