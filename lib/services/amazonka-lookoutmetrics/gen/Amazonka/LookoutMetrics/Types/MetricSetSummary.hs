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
-- Module      : Amazonka.LookoutMetrics.Types.MetricSetSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.MetricSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a dataset.
--
-- /See:/ 'newMetricSetSummary' smart constructor.
data MetricSetSummary = MetricSetSummary'
  { -- | The ARN of the detector to which the dataset belongs.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the dataset was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time at which the dataset was last modified.
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the dataset.
    metricSetArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the dataset.
    metricSetDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset.
    metricSetName :: Prelude.Maybe Prelude.Text,
    -- | The dataset\'s
    -- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorArn', 'metricSetSummary_anomalyDetectorArn' - The ARN of the detector to which the dataset belongs.
--
-- 'creationTime', 'metricSetSummary_creationTime' - The time at which the dataset was created.
--
-- 'lastModificationTime', 'metricSetSummary_lastModificationTime' - The time at which the dataset was last modified.
--
-- 'metricSetArn', 'metricSetSummary_metricSetArn' - The ARN of the dataset.
--
-- 'metricSetDescription', 'metricSetSummary_metricSetDescription' - The description of the dataset.
--
-- 'metricSetName', 'metricSetSummary_metricSetName' - The name of the dataset.
--
-- 'tags', 'metricSetSummary_tags' - The dataset\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
newMetricSetSummary ::
  MetricSetSummary
newMetricSetSummary =
  MetricSetSummary'
    { anomalyDetectorArn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      metricSetArn = Prelude.Nothing,
      metricSetDescription = Prelude.Nothing,
      metricSetName = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of the detector to which the dataset belongs.
metricSetSummary_anomalyDetectorArn :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.Text)
metricSetSummary_anomalyDetectorArn = Lens.lens (\MetricSetSummary' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@MetricSetSummary' {} a -> s {anomalyDetectorArn = a} :: MetricSetSummary)

-- | The time at which the dataset was created.
metricSetSummary_creationTime :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.UTCTime)
metricSetSummary_creationTime = Lens.lens (\MetricSetSummary' {creationTime} -> creationTime) (\s@MetricSetSummary' {} a -> s {creationTime = a} :: MetricSetSummary) Prelude.. Lens.mapping Data._Time

-- | The time at which the dataset was last modified.
metricSetSummary_lastModificationTime :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.UTCTime)
metricSetSummary_lastModificationTime = Lens.lens (\MetricSetSummary' {lastModificationTime} -> lastModificationTime) (\s@MetricSetSummary' {} a -> s {lastModificationTime = a} :: MetricSetSummary) Prelude.. Lens.mapping Data._Time

-- | The ARN of the dataset.
metricSetSummary_metricSetArn :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.Text)
metricSetSummary_metricSetArn = Lens.lens (\MetricSetSummary' {metricSetArn} -> metricSetArn) (\s@MetricSetSummary' {} a -> s {metricSetArn = a} :: MetricSetSummary)

-- | The description of the dataset.
metricSetSummary_metricSetDescription :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.Text)
metricSetSummary_metricSetDescription = Lens.lens (\MetricSetSummary' {metricSetDescription} -> metricSetDescription) (\s@MetricSetSummary' {} a -> s {metricSetDescription = a} :: MetricSetSummary)

-- | The name of the dataset.
metricSetSummary_metricSetName :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.Text)
metricSetSummary_metricSetName = Lens.lens (\MetricSetSummary' {metricSetName} -> metricSetName) (\s@MetricSetSummary' {} a -> s {metricSetName = a} :: MetricSetSummary)

-- | The dataset\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
metricSetSummary_tags :: Lens.Lens' MetricSetSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
metricSetSummary_tags = Lens.lens (\MetricSetSummary' {tags} -> tags) (\s@MetricSetSummary' {} a -> s {tags = a} :: MetricSetSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MetricSetSummary where
  parseJSON =
    Data.withObject
      "MetricSetSummary"
      ( \x ->
          MetricSetSummary'
            Prelude.<$> (x Data..:? "AnomalyDetectorArn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "MetricSetArn")
            Prelude.<*> (x Data..:? "MetricSetDescription")
            Prelude.<*> (x Data..:? "MetricSetName")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MetricSetSummary where
  hashWithSalt _salt MetricSetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` metricSetArn
      `Prelude.hashWithSalt` metricSetDescription
      `Prelude.hashWithSalt` metricSetName
      `Prelude.hashWithSalt` tags

instance Prelude.NFData MetricSetSummary where
  rnf MetricSetSummary' {..} =
    Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf metricSetArn
      `Prelude.seq` Prelude.rnf metricSetDescription
      `Prelude.seq` Prelude.rnf metricSetName
      `Prelude.seq` Prelude.rnf tags
