{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ViolationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ViolationEvent
  ( ViolationEvent (..),

    -- * Smart constructor
    mkViolationEvent,

    -- * Lenses
    veBehavior,
    veMetricValue,
    veSecurityProfileName,
    veThingName,
    veViolationEventTime,
    veViolationEventType,
    veViolationId,
  )
where

import qualified Network.AWS.IoT.Types.Behavior as Types
import qualified Network.AWS.IoT.Types.DeviceDefenderThingName as Types
import qualified Network.AWS.IoT.Types.MetricValue as Types
import qualified Network.AWS.IoT.Types.SecurityProfileName as Types
import qualified Network.AWS.IoT.Types.ViolationEventType as Types
import qualified Network.AWS.IoT.Types.ViolationId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a Device Defender security profile behavior violation.
--
-- /See:/ 'mkViolationEvent' smart constructor.
data ViolationEvent = ViolationEvent'
  { -- | The behavior which was violated.
    behavior :: Core.Maybe Types.Behavior,
    -- | The value of the metric (the measurement).
    metricValue :: Core.Maybe Types.MetricValue,
    -- | The name of the security profile whose behavior was violated.
    securityProfileName :: Core.Maybe Types.SecurityProfileName,
    -- | The name of the thing responsible for the violation event.
    thingName :: Core.Maybe Types.DeviceDefenderThingName,
    -- | The time the violation event occurred.
    violationEventTime :: Core.Maybe Core.NominalDiffTime,
    -- | The type of violation event.
    violationEventType :: Core.Maybe Types.ViolationEventType,
    -- | The ID of the violation event.
    violationId :: Core.Maybe Types.ViolationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ViolationEvent' value with any optional fields omitted.
mkViolationEvent ::
  ViolationEvent
mkViolationEvent =
  ViolationEvent'
    { behavior = Core.Nothing,
      metricValue = Core.Nothing,
      securityProfileName = Core.Nothing,
      thingName = Core.Nothing,
      violationEventTime = Core.Nothing,
      violationEventType = Core.Nothing,
      violationId = Core.Nothing
    }

-- | The behavior which was violated.
--
-- /Note:/ Consider using 'behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veBehavior :: Lens.Lens' ViolationEvent (Core.Maybe Types.Behavior)
veBehavior = Lens.field @"behavior"
{-# DEPRECATED veBehavior "Use generic-lens or generic-optics with 'behavior' instead." #-}

-- | The value of the metric (the measurement).
--
-- /Note:/ Consider using 'metricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veMetricValue :: Lens.Lens' ViolationEvent (Core.Maybe Types.MetricValue)
veMetricValue = Lens.field @"metricValue"
{-# DEPRECATED veMetricValue "Use generic-lens or generic-optics with 'metricValue' instead." #-}

-- | The name of the security profile whose behavior was violated.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veSecurityProfileName :: Lens.Lens' ViolationEvent (Core.Maybe Types.SecurityProfileName)
veSecurityProfileName = Lens.field @"securityProfileName"
{-# DEPRECATED veSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The name of the thing responsible for the violation event.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veThingName :: Lens.Lens' ViolationEvent (Core.Maybe Types.DeviceDefenderThingName)
veThingName = Lens.field @"thingName"
{-# DEPRECATED veThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The time the violation event occurred.
--
-- /Note:/ Consider using 'violationEventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veViolationEventTime :: Lens.Lens' ViolationEvent (Core.Maybe Core.NominalDiffTime)
veViolationEventTime = Lens.field @"violationEventTime"
{-# DEPRECATED veViolationEventTime "Use generic-lens or generic-optics with 'violationEventTime' instead." #-}

-- | The type of violation event.
--
-- /Note:/ Consider using 'violationEventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veViolationEventType :: Lens.Lens' ViolationEvent (Core.Maybe Types.ViolationEventType)
veViolationEventType = Lens.field @"violationEventType"
{-# DEPRECATED veViolationEventType "Use generic-lens or generic-optics with 'violationEventType' instead." #-}

-- | The ID of the violation event.
--
-- /Note:/ Consider using 'violationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veViolationId :: Lens.Lens' ViolationEvent (Core.Maybe Types.ViolationId)
veViolationId = Lens.field @"violationId"
{-# DEPRECATED veViolationId "Use generic-lens or generic-optics with 'violationId' instead." #-}

instance Core.FromJSON ViolationEvent where
  parseJSON =
    Core.withObject "ViolationEvent" Core.$
      \x ->
        ViolationEvent'
          Core.<$> (x Core..:? "behavior")
          Core.<*> (x Core..:? "metricValue")
          Core.<*> (x Core..:? "securityProfileName")
          Core.<*> (x Core..:? "thingName")
          Core.<*> (x Core..:? "violationEventTime")
          Core.<*> (x Core..:? "violationEventType")
          Core.<*> (x Core..:? "violationId")
