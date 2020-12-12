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
    veViolationEventType,
    veViolationId,
    veBehavior,
    veMetricValue,
    veSecurityProfileName,
    veViolationEventTime,
    veThingName,
  )
where

import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.ViolationEventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a Device Defender security profile behavior violation.
--
-- /See:/ 'mkViolationEvent' smart constructor.
data ViolationEvent = ViolationEvent'
  { violationEventType ::
      Lude.Maybe ViolationEventType,
    violationId :: Lude.Maybe Lude.Text,
    behavior :: Lude.Maybe Behavior,
    metricValue :: Lude.Maybe MetricValue,
    securityProfileName :: Lude.Maybe Lude.Text,
    violationEventTime :: Lude.Maybe Lude.Timestamp,
    thingName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ViolationEvent' with the minimum fields required to make a request.
--
-- * 'behavior' - The behavior which was violated.
-- * 'metricValue' - The value of the metric (the measurement).
-- * 'securityProfileName' - The name of the security profile whose behavior was violated.
-- * 'thingName' - The name of the thing responsible for the violation event.
-- * 'violationEventTime' - The time the violation event occurred.
-- * 'violationEventType' - The type of violation event.
-- * 'violationId' - The ID of the violation event.
mkViolationEvent ::
  ViolationEvent
mkViolationEvent =
  ViolationEvent'
    { violationEventType = Lude.Nothing,
      violationId = Lude.Nothing,
      behavior = Lude.Nothing,
      metricValue = Lude.Nothing,
      securityProfileName = Lude.Nothing,
      violationEventTime = Lude.Nothing,
      thingName = Lude.Nothing
    }

-- | The type of violation event.
--
-- /Note:/ Consider using 'violationEventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veViolationEventType :: Lens.Lens' ViolationEvent (Lude.Maybe ViolationEventType)
veViolationEventType = Lens.lens (violationEventType :: ViolationEvent -> Lude.Maybe ViolationEventType) (\s a -> s {violationEventType = a} :: ViolationEvent)
{-# DEPRECATED veViolationEventType "Use generic-lens or generic-optics with 'violationEventType' instead." #-}

-- | The ID of the violation event.
--
-- /Note:/ Consider using 'violationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veViolationId :: Lens.Lens' ViolationEvent (Lude.Maybe Lude.Text)
veViolationId = Lens.lens (violationId :: ViolationEvent -> Lude.Maybe Lude.Text) (\s a -> s {violationId = a} :: ViolationEvent)
{-# DEPRECATED veViolationId "Use generic-lens or generic-optics with 'violationId' instead." #-}

-- | The behavior which was violated.
--
-- /Note:/ Consider using 'behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veBehavior :: Lens.Lens' ViolationEvent (Lude.Maybe Behavior)
veBehavior = Lens.lens (behavior :: ViolationEvent -> Lude.Maybe Behavior) (\s a -> s {behavior = a} :: ViolationEvent)
{-# DEPRECATED veBehavior "Use generic-lens or generic-optics with 'behavior' instead." #-}

-- | The value of the metric (the measurement).
--
-- /Note:/ Consider using 'metricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veMetricValue :: Lens.Lens' ViolationEvent (Lude.Maybe MetricValue)
veMetricValue = Lens.lens (metricValue :: ViolationEvent -> Lude.Maybe MetricValue) (\s a -> s {metricValue = a} :: ViolationEvent)
{-# DEPRECATED veMetricValue "Use generic-lens or generic-optics with 'metricValue' instead." #-}

-- | The name of the security profile whose behavior was violated.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veSecurityProfileName :: Lens.Lens' ViolationEvent (Lude.Maybe Lude.Text)
veSecurityProfileName = Lens.lens (securityProfileName :: ViolationEvent -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileName = a} :: ViolationEvent)
{-# DEPRECATED veSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The time the violation event occurred.
--
-- /Note:/ Consider using 'violationEventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veViolationEventTime :: Lens.Lens' ViolationEvent (Lude.Maybe Lude.Timestamp)
veViolationEventTime = Lens.lens (violationEventTime :: ViolationEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {violationEventTime = a} :: ViolationEvent)
{-# DEPRECATED veViolationEventTime "Use generic-lens or generic-optics with 'violationEventTime' instead." #-}

-- | The name of the thing responsible for the violation event.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veThingName :: Lens.Lens' ViolationEvent (Lude.Maybe Lude.Text)
veThingName = Lens.lens (thingName :: ViolationEvent -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: ViolationEvent)
{-# DEPRECATED veThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.FromJSON ViolationEvent where
  parseJSON =
    Lude.withObject
      "ViolationEvent"
      ( \x ->
          ViolationEvent'
            Lude.<$> (x Lude..:? "violationEventType")
            Lude.<*> (x Lude..:? "violationId")
            Lude.<*> (x Lude..:? "behavior")
            Lude.<*> (x Lude..:? "metricValue")
            Lude.<*> (x Lude..:? "securityProfileName")
            Lude.<*> (x Lude..:? "violationEventTime")
            Lude.<*> (x Lude..:? "thingName")
      )
