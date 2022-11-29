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
-- Module      : Amazonka.IoT.Types.ViolationEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ViolationEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.Behavior
import Amazonka.IoT.Types.MetricValue
import Amazonka.IoT.Types.VerificationState
import Amazonka.IoT.Types.ViolationEventAdditionalInfo
import Amazonka.IoT.Types.ViolationEventType
import qualified Amazonka.Prelude as Prelude

-- | Information about a Device Defender security profile behavior violation.
--
-- /See:/ 'newViolationEvent' smart constructor.
data ViolationEvent = ViolationEvent'
  { -- | The name of the thing responsible for the violation event.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The time the violation event occurred.
    violationEventTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the violation event.
    violationId :: Prelude.Maybe Prelude.Text,
    -- | The value of the metric (the measurement).
    metricValue :: Prelude.Maybe MetricValue,
    -- | The details of a violation event.
    violationEventAdditionalInfo :: Prelude.Maybe ViolationEventAdditionalInfo,
    -- | The name of the security profile whose behavior was violated.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The description of the verification state of the violation.
    verificationStateDescription :: Prelude.Maybe Prelude.Text,
    -- | The verification state of the violation (detect alarm).
    verificationState :: Prelude.Maybe VerificationState,
    -- | The type of violation event.
    violationEventType :: Prelude.Maybe ViolationEventType,
    -- | The behavior that was violated.
    behavior :: Prelude.Maybe Behavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViolationEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'violationEvent_thingName' - The name of the thing responsible for the violation event.
--
-- 'violationEventTime', 'violationEvent_violationEventTime' - The time the violation event occurred.
--
-- 'violationId', 'violationEvent_violationId' - The ID of the violation event.
--
-- 'metricValue', 'violationEvent_metricValue' - The value of the metric (the measurement).
--
-- 'violationEventAdditionalInfo', 'violationEvent_violationEventAdditionalInfo' - The details of a violation event.
--
-- 'securityProfileName', 'violationEvent_securityProfileName' - The name of the security profile whose behavior was violated.
--
-- 'verificationStateDescription', 'violationEvent_verificationStateDescription' - The description of the verification state of the violation.
--
-- 'verificationState', 'violationEvent_verificationState' - The verification state of the violation (detect alarm).
--
-- 'violationEventType', 'violationEvent_violationEventType' - The type of violation event.
--
-- 'behavior', 'violationEvent_behavior' - The behavior that was violated.
newViolationEvent ::
  ViolationEvent
newViolationEvent =
  ViolationEvent'
    { thingName = Prelude.Nothing,
      violationEventTime = Prelude.Nothing,
      violationId = Prelude.Nothing,
      metricValue = Prelude.Nothing,
      violationEventAdditionalInfo = Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      verificationStateDescription = Prelude.Nothing,
      verificationState = Prelude.Nothing,
      violationEventType = Prelude.Nothing,
      behavior = Prelude.Nothing
    }

-- | The name of the thing responsible for the violation event.
violationEvent_thingName :: Lens.Lens' ViolationEvent (Prelude.Maybe Prelude.Text)
violationEvent_thingName = Lens.lens (\ViolationEvent' {thingName} -> thingName) (\s@ViolationEvent' {} a -> s {thingName = a} :: ViolationEvent)

-- | The time the violation event occurred.
violationEvent_violationEventTime :: Lens.Lens' ViolationEvent (Prelude.Maybe Prelude.UTCTime)
violationEvent_violationEventTime = Lens.lens (\ViolationEvent' {violationEventTime} -> violationEventTime) (\s@ViolationEvent' {} a -> s {violationEventTime = a} :: ViolationEvent) Prelude.. Lens.mapping Core._Time

-- | The ID of the violation event.
violationEvent_violationId :: Lens.Lens' ViolationEvent (Prelude.Maybe Prelude.Text)
violationEvent_violationId = Lens.lens (\ViolationEvent' {violationId} -> violationId) (\s@ViolationEvent' {} a -> s {violationId = a} :: ViolationEvent)

-- | The value of the metric (the measurement).
violationEvent_metricValue :: Lens.Lens' ViolationEvent (Prelude.Maybe MetricValue)
violationEvent_metricValue = Lens.lens (\ViolationEvent' {metricValue} -> metricValue) (\s@ViolationEvent' {} a -> s {metricValue = a} :: ViolationEvent)

-- | The details of a violation event.
violationEvent_violationEventAdditionalInfo :: Lens.Lens' ViolationEvent (Prelude.Maybe ViolationEventAdditionalInfo)
violationEvent_violationEventAdditionalInfo = Lens.lens (\ViolationEvent' {violationEventAdditionalInfo} -> violationEventAdditionalInfo) (\s@ViolationEvent' {} a -> s {violationEventAdditionalInfo = a} :: ViolationEvent)

-- | The name of the security profile whose behavior was violated.
violationEvent_securityProfileName :: Lens.Lens' ViolationEvent (Prelude.Maybe Prelude.Text)
violationEvent_securityProfileName = Lens.lens (\ViolationEvent' {securityProfileName} -> securityProfileName) (\s@ViolationEvent' {} a -> s {securityProfileName = a} :: ViolationEvent)

-- | The description of the verification state of the violation.
violationEvent_verificationStateDescription :: Lens.Lens' ViolationEvent (Prelude.Maybe Prelude.Text)
violationEvent_verificationStateDescription = Lens.lens (\ViolationEvent' {verificationStateDescription} -> verificationStateDescription) (\s@ViolationEvent' {} a -> s {verificationStateDescription = a} :: ViolationEvent)

-- | The verification state of the violation (detect alarm).
violationEvent_verificationState :: Lens.Lens' ViolationEvent (Prelude.Maybe VerificationState)
violationEvent_verificationState = Lens.lens (\ViolationEvent' {verificationState} -> verificationState) (\s@ViolationEvent' {} a -> s {verificationState = a} :: ViolationEvent)

-- | The type of violation event.
violationEvent_violationEventType :: Lens.Lens' ViolationEvent (Prelude.Maybe ViolationEventType)
violationEvent_violationEventType = Lens.lens (\ViolationEvent' {violationEventType} -> violationEventType) (\s@ViolationEvent' {} a -> s {violationEventType = a} :: ViolationEvent)

-- | The behavior that was violated.
violationEvent_behavior :: Lens.Lens' ViolationEvent (Prelude.Maybe Behavior)
violationEvent_behavior = Lens.lens (\ViolationEvent' {behavior} -> behavior) (\s@ViolationEvent' {} a -> s {behavior = a} :: ViolationEvent)

instance Core.FromJSON ViolationEvent where
  parseJSON =
    Core.withObject
      "ViolationEvent"
      ( \x ->
          ViolationEvent'
            Prelude.<$> (x Core..:? "thingName")
            Prelude.<*> (x Core..:? "violationEventTime")
            Prelude.<*> (x Core..:? "violationId")
            Prelude.<*> (x Core..:? "metricValue")
            Prelude.<*> (x Core..:? "violationEventAdditionalInfo")
            Prelude.<*> (x Core..:? "securityProfileName")
            Prelude.<*> (x Core..:? "verificationStateDescription")
            Prelude.<*> (x Core..:? "verificationState")
            Prelude.<*> (x Core..:? "violationEventType")
            Prelude.<*> (x Core..:? "behavior")
      )

instance Prelude.Hashable ViolationEvent where
  hashWithSalt _salt ViolationEvent' {..} =
    _salt `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` violationEventTime
      `Prelude.hashWithSalt` violationId
      `Prelude.hashWithSalt` metricValue
      `Prelude.hashWithSalt` violationEventAdditionalInfo
      `Prelude.hashWithSalt` securityProfileName
      `Prelude.hashWithSalt` verificationStateDescription
      `Prelude.hashWithSalt` verificationState
      `Prelude.hashWithSalt` violationEventType
      `Prelude.hashWithSalt` behavior

instance Prelude.NFData ViolationEvent where
  rnf ViolationEvent' {..} =
    Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf violationEventTime
      `Prelude.seq` Prelude.rnf violationId
      `Prelude.seq` Prelude.rnf metricValue
      `Prelude.seq` Prelude.rnf violationEventAdditionalInfo
      `Prelude.seq` Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf verificationStateDescription
      `Prelude.seq` Prelude.rnf verificationState
      `Prelude.seq` Prelude.rnf violationEventType
      `Prelude.seq` Prelude.rnf behavior
