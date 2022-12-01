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
-- Module      : Amazonka.IoT.Types.ActiveViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ActiveViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.Behavior
import Amazonka.IoT.Types.MetricValue
import Amazonka.IoT.Types.VerificationState
import Amazonka.IoT.Types.ViolationEventAdditionalInfo
import qualified Amazonka.Prelude as Prelude

-- | Information about an active Device Defender security profile behavior
-- violation.
--
-- /See:/ 'newActiveViolation' smart constructor.
data ActiveViolation = ActiveViolation'
  { -- | The name of the thing responsible for the active violation.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the active violation.
    violationId :: Prelude.Maybe Prelude.Text,
    -- | The details of a violation event.
    violationEventAdditionalInfo :: Prelude.Maybe ViolationEventAdditionalInfo,
    -- | The time the most recent violation occurred.
    lastViolationTime :: Prelude.Maybe Core.POSIX,
    -- | The value of the metric (the measurement) that caused the most recent
    -- violation.
    lastViolationValue :: Prelude.Maybe MetricValue,
    -- | The time the violation started.
    violationStartTime :: Prelude.Maybe Core.POSIX,
    -- | The security profile with the behavior is in violation.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The description of the verification state of the violation.
    verificationStateDescription :: Prelude.Maybe Prelude.Text,
    -- | The verification state of the violation (detect alarm).
    verificationState :: Prelude.Maybe VerificationState,
    -- | The behavior that is being violated.
    behavior :: Prelude.Maybe Behavior
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
-- 'thingName', 'activeViolation_thingName' - The name of the thing responsible for the active violation.
--
-- 'violationId', 'activeViolation_violationId' - The ID of the active violation.
--
-- 'violationEventAdditionalInfo', 'activeViolation_violationEventAdditionalInfo' - The details of a violation event.
--
-- 'lastViolationTime', 'activeViolation_lastViolationTime' - The time the most recent violation occurred.
--
-- 'lastViolationValue', 'activeViolation_lastViolationValue' - The value of the metric (the measurement) that caused the most recent
-- violation.
--
-- 'violationStartTime', 'activeViolation_violationStartTime' - The time the violation started.
--
-- 'securityProfileName', 'activeViolation_securityProfileName' - The security profile with the behavior is in violation.
--
-- 'verificationStateDescription', 'activeViolation_verificationStateDescription' - The description of the verification state of the violation.
--
-- 'verificationState', 'activeViolation_verificationState' - The verification state of the violation (detect alarm).
--
-- 'behavior', 'activeViolation_behavior' - The behavior that is being violated.
newActiveViolation ::
  ActiveViolation
newActiveViolation =
  ActiveViolation'
    { thingName = Prelude.Nothing,
      violationId = Prelude.Nothing,
      violationEventAdditionalInfo = Prelude.Nothing,
      lastViolationTime = Prelude.Nothing,
      lastViolationValue = Prelude.Nothing,
      violationStartTime = Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      verificationStateDescription = Prelude.Nothing,
      verificationState = Prelude.Nothing,
      behavior = Prelude.Nothing
    }

-- | The name of the thing responsible for the active violation.
activeViolation_thingName :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.Text)
activeViolation_thingName = Lens.lens (\ActiveViolation' {thingName} -> thingName) (\s@ActiveViolation' {} a -> s {thingName = a} :: ActiveViolation)

-- | The ID of the active violation.
activeViolation_violationId :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.Text)
activeViolation_violationId = Lens.lens (\ActiveViolation' {violationId} -> violationId) (\s@ActiveViolation' {} a -> s {violationId = a} :: ActiveViolation)

-- | The details of a violation event.
activeViolation_violationEventAdditionalInfo :: Lens.Lens' ActiveViolation (Prelude.Maybe ViolationEventAdditionalInfo)
activeViolation_violationEventAdditionalInfo = Lens.lens (\ActiveViolation' {violationEventAdditionalInfo} -> violationEventAdditionalInfo) (\s@ActiveViolation' {} a -> s {violationEventAdditionalInfo = a} :: ActiveViolation)

-- | The time the most recent violation occurred.
activeViolation_lastViolationTime :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.UTCTime)
activeViolation_lastViolationTime = Lens.lens (\ActiveViolation' {lastViolationTime} -> lastViolationTime) (\s@ActiveViolation' {} a -> s {lastViolationTime = a} :: ActiveViolation) Prelude.. Lens.mapping Core._Time

-- | The value of the metric (the measurement) that caused the most recent
-- violation.
activeViolation_lastViolationValue :: Lens.Lens' ActiveViolation (Prelude.Maybe MetricValue)
activeViolation_lastViolationValue = Lens.lens (\ActiveViolation' {lastViolationValue} -> lastViolationValue) (\s@ActiveViolation' {} a -> s {lastViolationValue = a} :: ActiveViolation)

-- | The time the violation started.
activeViolation_violationStartTime :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.UTCTime)
activeViolation_violationStartTime = Lens.lens (\ActiveViolation' {violationStartTime} -> violationStartTime) (\s@ActiveViolation' {} a -> s {violationStartTime = a} :: ActiveViolation) Prelude.. Lens.mapping Core._Time

-- | The security profile with the behavior is in violation.
activeViolation_securityProfileName :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.Text)
activeViolation_securityProfileName = Lens.lens (\ActiveViolation' {securityProfileName} -> securityProfileName) (\s@ActiveViolation' {} a -> s {securityProfileName = a} :: ActiveViolation)

-- | The description of the verification state of the violation.
activeViolation_verificationStateDescription :: Lens.Lens' ActiveViolation (Prelude.Maybe Prelude.Text)
activeViolation_verificationStateDescription = Lens.lens (\ActiveViolation' {verificationStateDescription} -> verificationStateDescription) (\s@ActiveViolation' {} a -> s {verificationStateDescription = a} :: ActiveViolation)

-- | The verification state of the violation (detect alarm).
activeViolation_verificationState :: Lens.Lens' ActiveViolation (Prelude.Maybe VerificationState)
activeViolation_verificationState = Lens.lens (\ActiveViolation' {verificationState} -> verificationState) (\s@ActiveViolation' {} a -> s {verificationState = a} :: ActiveViolation)

-- | The behavior that is being violated.
activeViolation_behavior :: Lens.Lens' ActiveViolation (Prelude.Maybe Behavior)
activeViolation_behavior = Lens.lens (\ActiveViolation' {behavior} -> behavior) (\s@ActiveViolation' {} a -> s {behavior = a} :: ActiveViolation)

instance Core.FromJSON ActiveViolation where
  parseJSON =
    Core.withObject
      "ActiveViolation"
      ( \x ->
          ActiveViolation'
            Prelude.<$> (x Core..:? "thingName")
            Prelude.<*> (x Core..:? "violationId")
            Prelude.<*> (x Core..:? "violationEventAdditionalInfo")
            Prelude.<*> (x Core..:? "lastViolationTime")
            Prelude.<*> (x Core..:? "lastViolationValue")
            Prelude.<*> (x Core..:? "violationStartTime")
            Prelude.<*> (x Core..:? "securityProfileName")
            Prelude.<*> (x Core..:? "verificationStateDescription")
            Prelude.<*> (x Core..:? "verificationState")
            Prelude.<*> (x Core..:? "behavior")
      )

instance Prelude.Hashable ActiveViolation where
  hashWithSalt _salt ActiveViolation' {..} =
    _salt `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` violationId
      `Prelude.hashWithSalt` violationEventAdditionalInfo
      `Prelude.hashWithSalt` lastViolationTime
      `Prelude.hashWithSalt` lastViolationValue
      `Prelude.hashWithSalt` violationStartTime
      `Prelude.hashWithSalt` securityProfileName
      `Prelude.hashWithSalt` verificationStateDescription
      `Prelude.hashWithSalt` verificationState
      `Prelude.hashWithSalt` behavior

instance Prelude.NFData ActiveViolation where
  rnf ActiveViolation' {..} =
    Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf violationId
      `Prelude.seq` Prelude.rnf violationEventAdditionalInfo
      `Prelude.seq` Prelude.rnf lastViolationTime
      `Prelude.seq` Prelude.rnf lastViolationValue
      `Prelude.seq` Prelude.rnf violationStartTime
      `Prelude.seq` Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf verificationStateDescription
      `Prelude.seq` Prelude.rnf verificationState
      `Prelude.seq` Prelude.rnf behavior
