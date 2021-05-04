{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.ViolationEventAdditionalInfo
import Network.AWS.IoT.Types.ViolationEventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a Device Defender security profile behavior violation.
--
-- /See:/ 'newViolationEvent' smart constructor.
data ViolationEvent = ViolationEvent'
  { -- | The value of the metric (the measurement).
    metricValue :: Prelude.Maybe MetricValue,
    -- | The ID of the violation event.
    violationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing responsible for the violation event.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The name of the security profile whose behavior was violated.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The behavior that was violated.
    behavior :: Prelude.Maybe Behavior,
    -- | The time the violation event occurred.
    violationEventTime :: Prelude.Maybe Prelude.POSIX,
    -- | The type of violation event.
    violationEventType :: Prelude.Maybe ViolationEventType,
    -- | The details of a violation event.
    violationEventAdditionalInfo :: Prelude.Maybe ViolationEventAdditionalInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { metricValue = Prelude.Nothing,
      violationId = Prelude.Nothing,
      thingName = Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      behavior = Prelude.Nothing,
      violationEventTime = Prelude.Nothing,
      violationEventType = Prelude.Nothing,
      violationEventAdditionalInfo = Prelude.Nothing
    }

-- | The value of the metric (the measurement).
violationEvent_metricValue :: Lens.Lens' ViolationEvent (Prelude.Maybe MetricValue)
violationEvent_metricValue = Lens.lens (\ViolationEvent' {metricValue} -> metricValue) (\s@ViolationEvent' {} a -> s {metricValue = a} :: ViolationEvent)

-- | The ID of the violation event.
violationEvent_violationId :: Lens.Lens' ViolationEvent (Prelude.Maybe Prelude.Text)
violationEvent_violationId = Lens.lens (\ViolationEvent' {violationId} -> violationId) (\s@ViolationEvent' {} a -> s {violationId = a} :: ViolationEvent)

-- | The name of the thing responsible for the violation event.
violationEvent_thingName :: Lens.Lens' ViolationEvent (Prelude.Maybe Prelude.Text)
violationEvent_thingName = Lens.lens (\ViolationEvent' {thingName} -> thingName) (\s@ViolationEvent' {} a -> s {thingName = a} :: ViolationEvent)

-- | The name of the security profile whose behavior was violated.
violationEvent_securityProfileName :: Lens.Lens' ViolationEvent (Prelude.Maybe Prelude.Text)
violationEvent_securityProfileName = Lens.lens (\ViolationEvent' {securityProfileName} -> securityProfileName) (\s@ViolationEvent' {} a -> s {securityProfileName = a} :: ViolationEvent)

-- | The behavior that was violated.
violationEvent_behavior :: Lens.Lens' ViolationEvent (Prelude.Maybe Behavior)
violationEvent_behavior = Lens.lens (\ViolationEvent' {behavior} -> behavior) (\s@ViolationEvent' {} a -> s {behavior = a} :: ViolationEvent)

-- | The time the violation event occurred.
violationEvent_violationEventTime :: Lens.Lens' ViolationEvent (Prelude.Maybe Prelude.UTCTime)
violationEvent_violationEventTime = Lens.lens (\ViolationEvent' {violationEventTime} -> violationEventTime) (\s@ViolationEvent' {} a -> s {violationEventTime = a} :: ViolationEvent) Prelude.. Lens.mapping Prelude._Time

-- | The type of violation event.
violationEvent_violationEventType :: Lens.Lens' ViolationEvent (Prelude.Maybe ViolationEventType)
violationEvent_violationEventType = Lens.lens (\ViolationEvent' {violationEventType} -> violationEventType) (\s@ViolationEvent' {} a -> s {violationEventType = a} :: ViolationEvent)

-- | The details of a violation event.
violationEvent_violationEventAdditionalInfo :: Lens.Lens' ViolationEvent (Prelude.Maybe ViolationEventAdditionalInfo)
violationEvent_violationEventAdditionalInfo = Lens.lens (\ViolationEvent' {violationEventAdditionalInfo} -> violationEventAdditionalInfo) (\s@ViolationEvent' {} a -> s {violationEventAdditionalInfo = a} :: ViolationEvent)

instance Prelude.FromJSON ViolationEvent where
  parseJSON =
    Prelude.withObject
      "ViolationEvent"
      ( \x ->
          ViolationEvent'
            Prelude.<$> (x Prelude..:? "metricValue")
            Prelude.<*> (x Prelude..:? "violationId")
            Prelude.<*> (x Prelude..:? "thingName")
            Prelude.<*> (x Prelude..:? "securityProfileName")
            Prelude.<*> (x Prelude..:? "behavior")
            Prelude.<*> (x Prelude..:? "violationEventTime")
            Prelude.<*> (x Prelude..:? "violationEventType")
            Prelude.<*> (x Prelude..:? "violationEventAdditionalInfo")
      )

instance Prelude.Hashable ViolationEvent

instance Prelude.NFData ViolationEvent
