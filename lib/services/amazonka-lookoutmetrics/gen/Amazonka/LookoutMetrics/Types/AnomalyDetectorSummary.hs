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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyDetectorSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyDetectorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types.AnomalyDetectorStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an an anomaly detector.
--
-- /See:/ 'newAnomalyDetectorSummary' smart constructor.
data AnomalyDetectorSummary = AnomalyDetectorSummary'
  { -- | The time at which the detector was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of detector.
    status :: Prelude.Maybe AnomalyDetectorStatus,
    -- | The ARN of the detector.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the detector.
    anomalyDetectorName :: Prelude.Maybe Prelude.Text,
    -- | A description of the detector.
    anomalyDetectorDescription :: Prelude.Maybe Prelude.Text,
    -- | The detector\'s
    -- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time at which the detector was last modified.
    lastModificationTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyDetectorSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'anomalyDetectorSummary_creationTime' - The time at which the detector was created.
--
-- 'status', 'anomalyDetectorSummary_status' - The status of detector.
--
-- 'anomalyDetectorArn', 'anomalyDetectorSummary_anomalyDetectorArn' - The ARN of the detector.
--
-- 'anomalyDetectorName', 'anomalyDetectorSummary_anomalyDetectorName' - The name of the detector.
--
-- 'anomalyDetectorDescription', 'anomalyDetectorSummary_anomalyDetectorDescription' - A description of the detector.
--
-- 'tags', 'anomalyDetectorSummary_tags' - The detector\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
--
-- 'lastModificationTime', 'anomalyDetectorSummary_lastModificationTime' - The time at which the detector was last modified.
newAnomalyDetectorSummary ::
  AnomalyDetectorSummary
newAnomalyDetectorSummary =
  AnomalyDetectorSummary'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      anomalyDetectorArn = Prelude.Nothing,
      anomalyDetectorName = Prelude.Nothing,
      anomalyDetectorDescription = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing
    }

-- | The time at which the detector was created.
anomalyDetectorSummary_creationTime :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.UTCTime)
anomalyDetectorSummary_creationTime = Lens.lens (\AnomalyDetectorSummary' {creationTime} -> creationTime) (\s@AnomalyDetectorSummary' {} a -> s {creationTime = a} :: AnomalyDetectorSummary) Prelude.. Lens.mapping Core._Time

-- | The status of detector.
anomalyDetectorSummary_status :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe AnomalyDetectorStatus)
anomalyDetectorSummary_status = Lens.lens (\AnomalyDetectorSummary' {status} -> status) (\s@AnomalyDetectorSummary' {} a -> s {status = a} :: AnomalyDetectorSummary)

-- | The ARN of the detector.
anomalyDetectorSummary_anomalyDetectorArn :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.Text)
anomalyDetectorSummary_anomalyDetectorArn = Lens.lens (\AnomalyDetectorSummary' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@AnomalyDetectorSummary' {} a -> s {anomalyDetectorArn = a} :: AnomalyDetectorSummary)

-- | The name of the detector.
anomalyDetectorSummary_anomalyDetectorName :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.Text)
anomalyDetectorSummary_anomalyDetectorName = Lens.lens (\AnomalyDetectorSummary' {anomalyDetectorName} -> anomalyDetectorName) (\s@AnomalyDetectorSummary' {} a -> s {anomalyDetectorName = a} :: AnomalyDetectorSummary)

-- | A description of the detector.
anomalyDetectorSummary_anomalyDetectorDescription :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.Text)
anomalyDetectorSummary_anomalyDetectorDescription = Lens.lens (\AnomalyDetectorSummary' {anomalyDetectorDescription} -> anomalyDetectorDescription) (\s@AnomalyDetectorSummary' {} a -> s {anomalyDetectorDescription = a} :: AnomalyDetectorSummary)

-- | The detector\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
anomalyDetectorSummary_tags :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
anomalyDetectorSummary_tags = Lens.lens (\AnomalyDetectorSummary' {tags} -> tags) (\s@AnomalyDetectorSummary' {} a -> s {tags = a} :: AnomalyDetectorSummary) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the detector was last modified.
anomalyDetectorSummary_lastModificationTime :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.UTCTime)
anomalyDetectorSummary_lastModificationTime = Lens.lens (\AnomalyDetectorSummary' {lastModificationTime} -> lastModificationTime) (\s@AnomalyDetectorSummary' {} a -> s {lastModificationTime = a} :: AnomalyDetectorSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON AnomalyDetectorSummary where
  parseJSON =
    Core.withObject
      "AnomalyDetectorSummary"
      ( \x ->
          AnomalyDetectorSummary'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AnomalyDetectorArn")
            Prelude.<*> (x Core..:? "AnomalyDetectorName")
            Prelude.<*> (x Core..:? "AnomalyDetectorDescription")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastModificationTime")
      )

instance Prelude.Hashable AnomalyDetectorSummary where
  hashWithSalt salt' AnomalyDetectorSummary' {..} =
    salt' `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` anomalyDetectorDescription
      `Prelude.hashWithSalt` anomalyDetectorName
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData AnomalyDetectorSummary where
  rnf AnomalyDetectorSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf anomalyDetectorDescription
      `Prelude.seq` Prelude.rnf anomalyDetectorName
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf status
