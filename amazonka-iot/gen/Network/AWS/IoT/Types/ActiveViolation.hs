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
-- Module      : Network.AWS.IoT.Types.ActiveViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ActiveViolation where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.VerificationState
import Network.AWS.IoT.Types.ViolationEventAdditionalInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an active Device Defender security profile behavior
-- violation.
--
-- /See:/ 'newActiveViolation' smart constructor.
data ActiveViolation = ActiveViolation'
  { -- | The ID of the active violation.
    violationId :: Prelude.Maybe Prelude.Text,
    -- | The time the most recent violation occurred.
    lastViolationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the thing responsible for the active violation.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The value of the metric (the measurement) that caused the most recent
    -- violation.
    lastViolationValue :: Prelude.Maybe MetricValue,
    -- | The description of the verification state of the violation.
    verificationStateDescription :: Prelude.Maybe Prelude.Text,
    -- | The security profile with the behavior is in violation.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The behavior that is being violated.
    behavior :: Prelude.Maybe Behavior,
    -- | The time the violation started.
    violationStartTime :: Prelude.Maybe Core.POSIX,
    -- | The verification state of the violation (detect alarm).
    verificationState :: Prelude.Maybe VerificationState,
    -- | The details of a violation event.
    violationEventAdditionalInfo :: Prelude.Maybe ViolationEventAdditionalInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationId', 'activeViolation_violationId' - The ID of the active violation.
--
-- 'lastViolationTime', 'activeViolation_lastViolationTime' - The time the most recent violation occurred.
--
-- 'thingName', 'activeViolation_thingName' - The name of the thing responsible for the active violation.
--
-- 'lastViolationValue', 'activeViolation_lastViolationValue' - The value of the metric (the measurement) that caused the most recent
-- violation.
--
-- 'verificationStateDescription', 'activeViolation_verificationStateDescription' - The description of the verification state of the violation.
--
-- 'securityProfileName', 'activeViolation_securityProfileName' - The security profile with the behavior is in violation.
--
-- 'behavior', 'activeViolation_behavior' - The behavior that is being violated.
--
-- 'violationStartTime', 'activeViolation_violationStartTime' - The time the violation started.
--
-- 'verificationState', 'activeViolation_verificationState' - The verification state of the violation (detect alarm).
--
-- 'violationEventAdditionalInfo', 'activeViolation_violationEventAdditionalInfo' - The details of a violation event.
newActiveViolation ::
  ActiveViolation
newActiveViolation =
  ActiveViolation'
    { violationId = Prelude.Nothing,
      lastViolationTime = Prelude.Nothing,
      thingName = Prelude.Nothing,
      lastViolationValue = Prelude.Nothing,
      verificationStateDescription = Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      behavior = Prelude.Nothing,
      violationStartTime = Prelude.Nothing,
      verificationState = Prelude.Nothing,
      violationEventAdditionalInfo = Prelude.Nothing
    }

-- | The ID of the active violation.
activeViolation_violationId :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.Text)
activeViolation_violationId = Lens.lens (\ActiveViolation' {violationId} -> violationId) (\s@ActiveViolation' {} a -> s {violationId = a} :: ActiveViolation)

-- | The time the most recent violation occurred.
activeViolation_lastViolationTime :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.UTCTime)
activeViolation_lastViolationTime = Lens.lens (\ActiveViolation' {lastViolationTime} -> lastViolationTime) (\s@ActiveViolation' {} a -> s {lastViolationTime = a} :: ActiveViolation) Prelude.. Lens.mapping Core._Time

-- | The name of the thing responsible for the active violation.
activeViolation_thingName :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.Text)
activeViolation_thingName = Lens.lens (\ActiveViolation' {thingName} -> thingName) (\s@ActiveViolation' {} a -> s {thingName = a} :: ActiveViolation)

-- | The value of the metric (the measurement) that caused the most recent
-- violation.
activeViolation_lastViolationValue :: Lens.Lens' ActiveViolation (Prelude.Maybe MetricValue)
activeViolation_lastViolationValue = Lens.lens (\ActiveViolation' {lastViolationValue} -> lastViolationValue) (\s@ActiveViolation' {} a -> s {lastViolationValue = a} :: ActiveViolation)

-- | The description of the verification state of the violation.
activeViolation_verificationStateDescription :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.Text)
activeViolation_verificationStateDescription = Lens.lens (\ActiveViolation' {verificationStateDescription} -> verificationStateDescription) (\s@ActiveViolation' {} a -> s {verificationStateDescription = a} :: ActiveViolation)

-- | The security profile with the behavior is in violation.
activeViolation_securityProfileName :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.Text)
activeViolation_securityProfileName = Lens.lens (\ActiveViolation' {securityProfileName} -> securityProfileName) (\s@ActiveViolation' {} a -> s {securityProfileName = a} :: ActiveViolation)

-- | The behavior that is being violated.
activeViolation_behavior :: Lens.Lens' ActiveViolation (Prelude.Maybe Behavior)
activeViolation_behavior = Lens.lens (\ActiveViolation' {behavior} -> behavior) (\s@ActiveViolation' {} a -> s {behavior = a} :: ActiveViolation)

-- | The time the violation started.
activeViolation_violationStartTime :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.UTCTime)
activeViolation_violationStartTime = Lens.lens (\ActiveViolation' {violationStartTime} -> violationStartTime) (\s@ActiveViolation' {} a -> s {violationStartTime = a} :: ActiveViolation) Prelude.. Lens.mapping Core._Time

-- | The verification state of the violation (detect alarm).
activeViolation_verificationState :: Lens.Lens' ActiveViolation (Prelude.Maybe VerificationState)
activeViolation_verificationState = Lens.lens (\ActiveViolation' {verificationState} -> verificationState) (\s@ActiveViolation' {} a -> s {verificationState = a} :: ActiveViolation)

-- | The details of a violation event.
activeViolation_violationEventAdditionalInfo :: Lens.Lens' ActiveViolation (Prelude.Maybe ViolationEventAdditionalInfo)
activeViolation_violationEventAdditionalInfo = Lens.lens (\ActiveViolation' {violationEventAdditionalInfo} -> violationEventAdditionalInfo) (\s@ActiveViolation' {} a -> s {violationEventAdditionalInfo = a} :: ActiveViolation)

instance Core.FromJSON ActiveViolation where
  parseJSON =
    Core.withObject
      "ActiveViolation"
      ( \x ->
          ActiveViolation'
            Prelude.<$> (x Core..:? "violationId")
            Prelude.<*> (x Core..:? "lastViolationTime")
            Prelude.<*> (x Core..:? "thingName")
            Prelude.<*> (x Core..:? "lastViolationValue")
            Prelude.<*> (x Core..:? "verificationStateDescription")
            Prelude.<*> (x Core..:? "securityProfileName")
            Prelude.<*> (x Core..:? "behavior")
            Prelude.<*> (x Core..:? "violationStartTime")
            Prelude.<*> (x Core..:? "verificationState")
            Prelude.<*> (x Core..:? "violationEventAdditionalInfo")
      )

instance Prelude.Hashable ActiveViolation

instance Prelude.NFData ActiveViolation
