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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyDetectorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.AnomalyDetectorStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an an anomaly detector.
--
-- /See:/ 'newAnomalyDetectorSummary' smart constructor.
data AnomalyDetectorSummary = AnomalyDetectorSummary'
  { -- | The ARN of the detector.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the detector.
    anomalyDetectorDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the detector.
    anomalyDetectorName :: Prelude.Maybe Prelude.Text,
    -- | The time at which the detector was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time at which the detector was last modified.
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | The status of detector.
    status :: Prelude.Maybe AnomalyDetectorStatus,
    -- | The detector\'s
    -- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'anomalyDetectorArn', 'anomalyDetectorSummary_anomalyDetectorArn' - The ARN of the detector.
--
-- 'anomalyDetectorDescription', 'anomalyDetectorSummary_anomalyDetectorDescription' - A description of the detector.
--
-- 'anomalyDetectorName', 'anomalyDetectorSummary_anomalyDetectorName' - The name of the detector.
--
-- 'creationTime', 'anomalyDetectorSummary_creationTime' - The time at which the detector was created.
--
-- 'lastModificationTime', 'anomalyDetectorSummary_lastModificationTime' - The time at which the detector was last modified.
--
-- 'status', 'anomalyDetectorSummary_status' - The status of detector.
--
-- 'tags', 'anomalyDetectorSummary_tags' - The detector\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
newAnomalyDetectorSummary ::
  AnomalyDetectorSummary
newAnomalyDetectorSummary =
  AnomalyDetectorSummary'
    { anomalyDetectorArn =
        Prelude.Nothing,
      anomalyDetectorDescription = Prelude.Nothing,
      anomalyDetectorName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of the detector.
anomalyDetectorSummary_anomalyDetectorArn :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.Text)
anomalyDetectorSummary_anomalyDetectorArn = Lens.lens (\AnomalyDetectorSummary' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@AnomalyDetectorSummary' {} a -> s {anomalyDetectorArn = a} :: AnomalyDetectorSummary)

-- | A description of the detector.
anomalyDetectorSummary_anomalyDetectorDescription :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.Text)
anomalyDetectorSummary_anomalyDetectorDescription = Lens.lens (\AnomalyDetectorSummary' {anomalyDetectorDescription} -> anomalyDetectorDescription) (\s@AnomalyDetectorSummary' {} a -> s {anomalyDetectorDescription = a} :: AnomalyDetectorSummary)

-- | The name of the detector.
anomalyDetectorSummary_anomalyDetectorName :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.Text)
anomalyDetectorSummary_anomalyDetectorName = Lens.lens (\AnomalyDetectorSummary' {anomalyDetectorName} -> anomalyDetectorName) (\s@AnomalyDetectorSummary' {} a -> s {anomalyDetectorName = a} :: AnomalyDetectorSummary)

-- | The time at which the detector was created.
anomalyDetectorSummary_creationTime :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.UTCTime)
anomalyDetectorSummary_creationTime = Lens.lens (\AnomalyDetectorSummary' {creationTime} -> creationTime) (\s@AnomalyDetectorSummary' {} a -> s {creationTime = a} :: AnomalyDetectorSummary) Prelude.. Lens.mapping Data._Time

-- | The time at which the detector was last modified.
anomalyDetectorSummary_lastModificationTime :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe Prelude.UTCTime)
anomalyDetectorSummary_lastModificationTime = Lens.lens (\AnomalyDetectorSummary' {lastModificationTime} -> lastModificationTime) (\s@AnomalyDetectorSummary' {} a -> s {lastModificationTime = a} :: AnomalyDetectorSummary) Prelude.. Lens.mapping Data._Time

-- | The status of detector.
anomalyDetectorSummary_status :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe AnomalyDetectorStatus)
anomalyDetectorSummary_status = Lens.lens (\AnomalyDetectorSummary' {status} -> status) (\s@AnomalyDetectorSummary' {} a -> s {status = a} :: AnomalyDetectorSummary)

-- | The detector\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
anomalyDetectorSummary_tags :: Lens.Lens' AnomalyDetectorSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
anomalyDetectorSummary_tags = Lens.lens (\AnomalyDetectorSummary' {tags} -> tags) (\s@AnomalyDetectorSummary' {} a -> s {tags = a} :: AnomalyDetectorSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AnomalyDetectorSummary where
  parseJSON =
    Data.withObject
      "AnomalyDetectorSummary"
      ( \x ->
          AnomalyDetectorSummary'
            Prelude.<$> (x Data..:? "AnomalyDetectorArn")
            Prelude.<*> (x Data..:? "AnomalyDetectorDescription")
            Prelude.<*> (x Data..:? "AnomalyDetectorName")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AnomalyDetectorSummary where
  hashWithSalt _salt AnomalyDetectorSummary' {..} =
    _salt
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` anomalyDetectorDescription
      `Prelude.hashWithSalt` anomalyDetectorName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags

instance Prelude.NFData AnomalyDetectorSummary where
  rnf AnomalyDetectorSummary' {..} =
    Prelude.rnf anomalyDetectorArn `Prelude.seq`
      Prelude.rnf anomalyDetectorDescription `Prelude.seq`
        Prelude.rnf anomalyDetectorName `Prelude.seq`
          Prelude.rnf creationTime `Prelude.seq`
            Prelude.rnf lastModificationTime `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf tags
