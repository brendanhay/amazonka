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
import Network.AWS.IoT.Types.ViolationEventAdditionalInfo
import qualified Network.AWS.Lens as Lens

-- | Information about an active Device Defender security profile behavior
-- violation.
--
-- /See:/ 'newActiveViolation' smart constructor.
data ActiveViolation = ActiveViolation'
  { -- | The ID of the active violation.
    violationId :: Core.Maybe Core.Text,
    -- | The time the most recent violation occurred.
    lastViolationTime :: Core.Maybe Core.POSIX,
    -- | The name of the thing responsible for the active violation.
    thingName :: Core.Maybe Core.Text,
    -- | The value of the metric (the measurement) that caused the most recent
    -- violation.
    lastViolationValue :: Core.Maybe MetricValue,
    -- | The security profile with the behavior is in violation.
    securityProfileName :: Core.Maybe Core.Text,
    -- | The behavior that is being violated.
    behavior :: Core.Maybe Behavior,
    -- | The time the violation started.
    violationStartTime :: Core.Maybe Core.POSIX,
    -- | The details of a violation event.
    violationEventAdditionalInfo :: Core.Maybe ViolationEventAdditionalInfo
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'securityProfileName', 'activeViolation_securityProfileName' - The security profile with the behavior is in violation.
--
-- 'behavior', 'activeViolation_behavior' - The behavior that is being violated.
--
-- 'violationStartTime', 'activeViolation_violationStartTime' - The time the violation started.
--
-- 'violationEventAdditionalInfo', 'activeViolation_violationEventAdditionalInfo' - The details of a violation event.
newActiveViolation ::
  ActiveViolation
newActiveViolation =
  ActiveViolation'
    { violationId = Core.Nothing,
      lastViolationTime = Core.Nothing,
      thingName = Core.Nothing,
      lastViolationValue = Core.Nothing,
      securityProfileName = Core.Nothing,
      behavior = Core.Nothing,
      violationStartTime = Core.Nothing,
      violationEventAdditionalInfo = Core.Nothing
    }

-- | The ID of the active violation.
activeViolation_violationId :: Lens.Lens' ActiveViolation (Core.Maybe Core.Text)
activeViolation_violationId = Lens.lens (\ActiveViolation' {violationId} -> violationId) (\s@ActiveViolation' {} a -> s {violationId = a} :: ActiveViolation)

-- | The time the most recent violation occurred.
activeViolation_lastViolationTime :: Lens.Lens' ActiveViolation (Core.Maybe Core.UTCTime)
activeViolation_lastViolationTime = Lens.lens (\ActiveViolation' {lastViolationTime} -> lastViolationTime) (\s@ActiveViolation' {} a -> s {lastViolationTime = a} :: ActiveViolation) Core.. Lens.mapping Core._Time

-- | The name of the thing responsible for the active violation.
activeViolation_thingName :: Lens.Lens' ActiveViolation (Core.Maybe Core.Text)
activeViolation_thingName = Lens.lens (\ActiveViolation' {thingName} -> thingName) (\s@ActiveViolation' {} a -> s {thingName = a} :: ActiveViolation)

-- | The value of the metric (the measurement) that caused the most recent
-- violation.
activeViolation_lastViolationValue :: Lens.Lens' ActiveViolation (Core.Maybe MetricValue)
activeViolation_lastViolationValue = Lens.lens (\ActiveViolation' {lastViolationValue} -> lastViolationValue) (\s@ActiveViolation' {} a -> s {lastViolationValue = a} :: ActiveViolation)

-- | The security profile with the behavior is in violation.
activeViolation_securityProfileName :: Lens.Lens' ActiveViolation (Core.Maybe Core.Text)
activeViolation_securityProfileName = Lens.lens (\ActiveViolation' {securityProfileName} -> securityProfileName) (\s@ActiveViolation' {} a -> s {securityProfileName = a} :: ActiveViolation)

-- | The behavior that is being violated.
activeViolation_behavior :: Lens.Lens' ActiveViolation (Core.Maybe Behavior)
activeViolation_behavior = Lens.lens (\ActiveViolation' {behavior} -> behavior) (\s@ActiveViolation' {} a -> s {behavior = a} :: ActiveViolation)

-- | The time the violation started.
activeViolation_violationStartTime :: Lens.Lens' ActiveViolation (Core.Maybe Core.UTCTime)
activeViolation_violationStartTime = Lens.lens (\ActiveViolation' {violationStartTime} -> violationStartTime) (\s@ActiveViolation' {} a -> s {violationStartTime = a} :: ActiveViolation) Core.. Lens.mapping Core._Time

-- | The details of a violation event.
activeViolation_violationEventAdditionalInfo :: Lens.Lens' ActiveViolation (Core.Maybe ViolationEventAdditionalInfo)
activeViolation_violationEventAdditionalInfo = Lens.lens (\ActiveViolation' {violationEventAdditionalInfo} -> violationEventAdditionalInfo) (\s@ActiveViolation' {} a -> s {violationEventAdditionalInfo = a} :: ActiveViolation)

instance Core.FromJSON ActiveViolation where
  parseJSON =
    Core.withObject
      "ActiveViolation"
      ( \x ->
          ActiveViolation'
            Core.<$> (x Core..:? "violationId")
            Core.<*> (x Core..:? "lastViolationTime")
            Core.<*> (x Core..:? "thingName")
            Core.<*> (x Core..:? "lastViolationValue")
            Core.<*> (x Core..:? "securityProfileName")
            Core.<*> (x Core..:? "behavior")
            Core.<*> (x Core..:? "violationStartTime")
            Core.<*> (x Core..:? "violationEventAdditionalInfo")
      )

instance Core.Hashable ActiveViolation

instance Core.NFData ActiveViolation
