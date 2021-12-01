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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.MetricSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a dataset.
--
-- /See:/ 'newMetricSetSummary' smart constructor.
data MetricSetSummary = MetricSetSummary'
  { -- | The time at which the dataset was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the detector to which the dataset belongs.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset.
    metricSetName :: Prelude.Maybe Prelude.Text,
    -- | The description of the dataset.
    metricSetDescription :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dataset.
    metricSetArn :: Prelude.Maybe Prelude.Text,
    -- | The dataset\'s
    -- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time at which the dataset was last modified.
    lastModificationTime :: Prelude.Maybe Core.POSIX
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
-- 'creationTime', 'metricSetSummary_creationTime' - The time at which the dataset was created.
--
-- 'anomalyDetectorArn', 'metricSetSummary_anomalyDetectorArn' - The ARN of the detector to which the dataset belongs.
--
-- 'metricSetName', 'metricSetSummary_metricSetName' - The name of the dataset.
--
-- 'metricSetDescription', 'metricSetSummary_metricSetDescription' - The description of the dataset.
--
-- 'metricSetArn', 'metricSetSummary_metricSetArn' - The ARN of the dataset.
--
-- 'tags', 'metricSetSummary_tags' - The dataset\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
--
-- 'lastModificationTime', 'metricSetSummary_lastModificationTime' - The time at which the dataset was last modified.
newMetricSetSummary ::
  MetricSetSummary
newMetricSetSummary =
  MetricSetSummary'
    { creationTime = Prelude.Nothing,
      anomalyDetectorArn = Prelude.Nothing,
      metricSetName = Prelude.Nothing,
      metricSetDescription = Prelude.Nothing,
      metricSetArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing
    }

-- | The time at which the dataset was created.
metricSetSummary_creationTime :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.UTCTime)
metricSetSummary_creationTime = Lens.lens (\MetricSetSummary' {creationTime} -> creationTime) (\s@MetricSetSummary' {} a -> s {creationTime = a} :: MetricSetSummary) Prelude.. Lens.mapping Core._Time

-- | The ARN of the detector to which the dataset belongs.
metricSetSummary_anomalyDetectorArn :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.Text)
metricSetSummary_anomalyDetectorArn = Lens.lens (\MetricSetSummary' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@MetricSetSummary' {} a -> s {anomalyDetectorArn = a} :: MetricSetSummary)

-- | The name of the dataset.
metricSetSummary_metricSetName :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.Text)
metricSetSummary_metricSetName = Lens.lens (\MetricSetSummary' {metricSetName} -> metricSetName) (\s@MetricSetSummary' {} a -> s {metricSetName = a} :: MetricSetSummary)

-- | The description of the dataset.
metricSetSummary_metricSetDescription :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.Text)
metricSetSummary_metricSetDescription = Lens.lens (\MetricSetSummary' {metricSetDescription} -> metricSetDescription) (\s@MetricSetSummary' {} a -> s {metricSetDescription = a} :: MetricSetSummary)

-- | The ARN of the dataset.
metricSetSummary_metricSetArn :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.Text)
metricSetSummary_metricSetArn = Lens.lens (\MetricSetSummary' {metricSetArn} -> metricSetArn) (\s@MetricSetSummary' {} a -> s {metricSetArn = a} :: MetricSetSummary)

-- | The dataset\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
metricSetSummary_tags :: Lens.Lens' MetricSetSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
metricSetSummary_tags = Lens.lens (\MetricSetSummary' {tags} -> tags) (\s@MetricSetSummary' {} a -> s {tags = a} :: MetricSetSummary) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the dataset was last modified.
metricSetSummary_lastModificationTime :: Lens.Lens' MetricSetSummary (Prelude.Maybe Prelude.UTCTime)
metricSetSummary_lastModificationTime = Lens.lens (\MetricSetSummary' {lastModificationTime} -> lastModificationTime) (\s@MetricSetSummary' {} a -> s {lastModificationTime = a} :: MetricSetSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON MetricSetSummary where
  parseJSON =
    Core.withObject
      "MetricSetSummary"
      ( \x ->
          MetricSetSummary'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "AnomalyDetectorArn")
            Prelude.<*> (x Core..:? "MetricSetName")
            Prelude.<*> (x Core..:? "MetricSetDescription")
            Prelude.<*> (x Core..:? "MetricSetArn")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastModificationTime")
      )

instance Prelude.Hashable MetricSetSummary where
  hashWithSalt salt' MetricSetSummary' {..} =
    salt' `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` metricSetArn
      `Prelude.hashWithSalt` metricSetDescription
      `Prelude.hashWithSalt` metricSetName
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData MetricSetSummary where
  rnf MetricSetSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf metricSetArn
      `Prelude.seq` Prelude.rnf metricSetDescription
      `Prelude.seq` Prelude.rnf metricSetName
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
