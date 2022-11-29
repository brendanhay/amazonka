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
-- Module      : Amazonka.IoTEvents.Types.DetectorModelVersionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.DetectorModelVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types.DetectorModelVersionStatus
import Amazonka.IoTEvents.Types.EvaluationMethod
import qualified Amazonka.Prelude as Prelude

-- | Information about the detector model version.
--
-- /See:/ 'newDetectorModelVersionSummary' smart constructor.
data DetectorModelVersionSummary = DetectorModelVersionSummary'
  { -- | The ARN of the role that grants the detector model permission to perform
    -- its tasks.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the order in which events are evaluated and how
    -- actions are executed.
    evaluationMethod :: Prelude.Maybe EvaluationMethod,
    -- | The status of the detector model version.
    status :: Prelude.Maybe DetectorModelVersionStatus,
    -- | The name of the detector model.
    detectorModelName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the detector model version.
    detectorModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The time the detector model version was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The last time the detector model version was updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the detector model version.
    detectorModelArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorModelVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'detectorModelVersionSummary_roleArn' - The ARN of the role that grants the detector model permission to perform
-- its tasks.
--
-- 'evaluationMethod', 'detectorModelVersionSummary_evaluationMethod' - Information about the order in which events are evaluated and how
-- actions are executed.
--
-- 'status', 'detectorModelVersionSummary_status' - The status of the detector model version.
--
-- 'detectorModelName', 'detectorModelVersionSummary_detectorModelName' - The name of the detector model.
--
-- 'detectorModelVersion', 'detectorModelVersionSummary_detectorModelVersion' - The ID of the detector model version.
--
-- 'creationTime', 'detectorModelVersionSummary_creationTime' - The time the detector model version was created.
--
-- 'lastUpdateTime', 'detectorModelVersionSummary_lastUpdateTime' - The last time the detector model version was updated.
--
-- 'detectorModelArn', 'detectorModelVersionSummary_detectorModelArn' - The ARN of the detector model version.
newDetectorModelVersionSummary ::
  DetectorModelVersionSummary
newDetectorModelVersionSummary =
  DetectorModelVersionSummary'
    { roleArn =
        Prelude.Nothing,
      evaluationMethod = Prelude.Nothing,
      status = Prelude.Nothing,
      detectorModelName = Prelude.Nothing,
      detectorModelVersion = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      detectorModelArn = Prelude.Nothing
    }

-- | The ARN of the role that grants the detector model permission to perform
-- its tasks.
detectorModelVersionSummary_roleArn :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.Text)
detectorModelVersionSummary_roleArn = Lens.lens (\DetectorModelVersionSummary' {roleArn} -> roleArn) (\s@DetectorModelVersionSummary' {} a -> s {roleArn = a} :: DetectorModelVersionSummary)

-- | Information about the order in which events are evaluated and how
-- actions are executed.
detectorModelVersionSummary_evaluationMethod :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe EvaluationMethod)
detectorModelVersionSummary_evaluationMethod = Lens.lens (\DetectorModelVersionSummary' {evaluationMethod} -> evaluationMethod) (\s@DetectorModelVersionSummary' {} a -> s {evaluationMethod = a} :: DetectorModelVersionSummary)

-- | The status of the detector model version.
detectorModelVersionSummary_status :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe DetectorModelVersionStatus)
detectorModelVersionSummary_status = Lens.lens (\DetectorModelVersionSummary' {status} -> status) (\s@DetectorModelVersionSummary' {} a -> s {status = a} :: DetectorModelVersionSummary)

-- | The name of the detector model.
detectorModelVersionSummary_detectorModelName :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.Text)
detectorModelVersionSummary_detectorModelName = Lens.lens (\DetectorModelVersionSummary' {detectorModelName} -> detectorModelName) (\s@DetectorModelVersionSummary' {} a -> s {detectorModelName = a} :: DetectorModelVersionSummary)

-- | The ID of the detector model version.
detectorModelVersionSummary_detectorModelVersion :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.Text)
detectorModelVersionSummary_detectorModelVersion = Lens.lens (\DetectorModelVersionSummary' {detectorModelVersion} -> detectorModelVersion) (\s@DetectorModelVersionSummary' {} a -> s {detectorModelVersion = a} :: DetectorModelVersionSummary)

-- | The time the detector model version was created.
detectorModelVersionSummary_creationTime :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.UTCTime)
detectorModelVersionSummary_creationTime = Lens.lens (\DetectorModelVersionSummary' {creationTime} -> creationTime) (\s@DetectorModelVersionSummary' {} a -> s {creationTime = a} :: DetectorModelVersionSummary) Prelude.. Lens.mapping Core._Time

-- | The last time the detector model version was updated.
detectorModelVersionSummary_lastUpdateTime :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.UTCTime)
detectorModelVersionSummary_lastUpdateTime = Lens.lens (\DetectorModelVersionSummary' {lastUpdateTime} -> lastUpdateTime) (\s@DetectorModelVersionSummary' {} a -> s {lastUpdateTime = a} :: DetectorModelVersionSummary) Prelude.. Lens.mapping Core._Time

-- | The ARN of the detector model version.
detectorModelVersionSummary_detectorModelArn :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.Text)
detectorModelVersionSummary_detectorModelArn = Lens.lens (\DetectorModelVersionSummary' {detectorModelArn} -> detectorModelArn) (\s@DetectorModelVersionSummary' {} a -> s {detectorModelArn = a} :: DetectorModelVersionSummary)

instance Core.FromJSON DetectorModelVersionSummary where
  parseJSON =
    Core.withObject
      "DetectorModelVersionSummary"
      ( \x ->
          DetectorModelVersionSummary'
            Prelude.<$> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "evaluationMethod")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "detectorModelName")
            Prelude.<*> (x Core..:? "detectorModelVersion")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "detectorModelArn")
      )

instance Prelude.Hashable DetectorModelVersionSummary where
  hashWithSalt _salt DetectorModelVersionSummary' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` evaluationMethod
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` detectorModelName
      `Prelude.hashWithSalt` detectorModelVersion
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` detectorModelArn

instance Prelude.NFData DetectorModelVersionSummary where
  rnf DetectorModelVersionSummary' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf evaluationMethod
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf detectorModelName
      `Prelude.seq` Prelude.rnf detectorModelVersion
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf detectorModelArn
