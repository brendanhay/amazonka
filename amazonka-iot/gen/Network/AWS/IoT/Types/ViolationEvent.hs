{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ViolationEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ViolationEvent where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.ViolationEventAdditionalInfo
import Network.AWS.IoT.Types.ViolationEventType
import qualified Network.AWS.Lens as Lens

-- | Information about a Device Defender security profile behavior violation.
--
-- /See:/ 'newViolationEvent' smart constructor.
data ViolationEvent = ViolationEvent'
  { -- | The value of the metric (the measurement).
    metricValue :: Core.Maybe MetricValue,
    -- | The ID of the violation event.
    violationId :: Core.Maybe Core.Text,
    -- | The name of the thing responsible for the violation event.
    thingName :: Core.Maybe Core.Text,
    -- | The name of the security profile whose behavior was violated.
    securityProfileName :: Core.Maybe Core.Text,
    -- | The behavior that was violated.
    behavior :: Core.Maybe Behavior,
    -- | The time the violation event occurred.
    violationEventTime :: Core.Maybe Core.POSIX,
    -- | The type of violation event.
    violationEventType :: Core.Maybe ViolationEventType,
    -- | The details of a violation event.
    violationEventAdditionalInfo :: Core.Maybe ViolationEventAdditionalInfo
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ViolationEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricValue', 'violationEvent_metricValue' - The value of the metric (the measurement).
--
-- 'violationId', 'violationEvent_violationId' - The ID of the violation event.
--
-- 'thingName', 'violationEvent_thingName' - The name of the thing responsible for the violation event.
--
-- 'securityProfileName', 'violationEvent_securityProfileName' - The name of the security profile whose behavior was violated.
--
-- 'behavior', 'violationEvent_behavior' - The behavior that was violated.
--
-- 'violationEventTime', 'violationEvent_violationEventTime' - The time the violation event occurred.
--
-- 'violationEventType', 'violationEvent_violationEventType' - The type of violation event.
--
-- 'violationEventAdditionalInfo', 'violationEvent_violationEventAdditionalInfo' - The details of a violation event.
newViolationEvent ::
  ViolationEvent
newViolationEvent =
  ViolationEvent'
    { metricValue = Core.Nothing,
      violationId = Core.Nothing,
      thingName = Core.Nothing,
      securityProfileName = Core.Nothing,
      behavior = Core.Nothing,
      violationEventTime = Core.Nothing,
      violationEventType = Core.Nothing,
      violationEventAdditionalInfo = Core.Nothing
    }

-- | The value of the metric (the measurement).
violationEvent_metricValue :: Lens.Lens' ViolationEvent (Core.Maybe MetricValue)
violationEvent_metricValue = Lens.lens (\ViolationEvent' {metricValue} -> metricValue) (\s@ViolationEvent' {} a -> s {metricValue = a} :: ViolationEvent)

-- | The ID of the violation event.
violationEvent_violationId :: Lens.Lens' ViolationEvent (Core.Maybe Core.Text)
violationEvent_violationId = Lens.lens (\ViolationEvent' {violationId} -> violationId) (\s@ViolationEvent' {} a -> s {violationId = a} :: ViolationEvent)

-- | The name of the thing responsible for the violation event.
violationEvent_thingName :: Lens.Lens' ViolationEvent (Core.Maybe Core.Text)
violationEvent_thingName = Lens.lens (\ViolationEvent' {thingName} -> thingName) (\s@ViolationEvent' {} a -> s {thingName = a} :: ViolationEvent)

-- | The name of the security profile whose behavior was violated.
violationEvent_securityProfileName :: Lens.Lens' ViolationEvent (Core.Maybe Core.Text)
violationEvent_securityProfileName = Lens.lens (\ViolationEvent' {securityProfileName} -> securityProfileName) (\s@ViolationEvent' {} a -> s {securityProfileName = a} :: ViolationEvent)

-- | The behavior that was violated.
violationEvent_behavior :: Lens.Lens' ViolationEvent (Core.Maybe Behavior)
violationEvent_behavior = Lens.lens (\ViolationEvent' {behavior} -> behavior) (\s@ViolationEvent' {} a -> s {behavior = a} :: ViolationEvent)

-- | The time the violation event occurred.
violationEvent_violationEventTime :: Lens.Lens' ViolationEvent (Core.Maybe Core.UTCTime)
violationEvent_violationEventTime = Lens.lens (\ViolationEvent' {violationEventTime} -> violationEventTime) (\s@ViolationEvent' {} a -> s {violationEventTime = a} :: ViolationEvent) Core.. Lens.mapping Core._Time

-- | The type of violation event.
violationEvent_violationEventType :: Lens.Lens' ViolationEvent (Core.Maybe ViolationEventType)
violationEvent_violationEventType = Lens.lens (\ViolationEvent' {violationEventType} -> violationEventType) (\s@ViolationEvent' {} a -> s {violationEventType = a} :: ViolationEvent)

-- | The details of a violation event.
violationEvent_violationEventAdditionalInfo :: Lens.Lens' ViolationEvent (Core.Maybe ViolationEventAdditionalInfo)
violationEvent_violationEventAdditionalInfo = Lens.lens (\ViolationEvent' {violationEventAdditionalInfo} -> violationEventAdditionalInfo) (\s@ViolationEvent' {} a -> s {violationEventAdditionalInfo = a} :: ViolationEvent)

instance Core.FromJSON ViolationEvent where
  parseJSON =
    Core.withObject
      "ViolationEvent"
      ( \x ->
          ViolationEvent'
            Core.<$> (x Core..:? "metricValue")
            Core.<*> (x Core..:? "violationId")
            Core.<*> (x Core..:? "thingName")
            Core.<*> (x Core..:? "securityProfileName")
            Core.<*> (x Core..:? "behavior")
            Core.<*> (x Core..:? "violationEventTime")
            Core.<*> (x Core..:? "violationEventType")
            Core.<*> (x Core..:? "violationEventAdditionalInfo")
      )

instance Core.Hashable ViolationEvent

instance Core.NFData ViolationEvent
