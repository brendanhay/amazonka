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
-- Module      : Amazonka.Personalize.Types.MetricAttribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.MetricAttribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.MetricAttributionOutput
import qualified Amazonka.Prelude as Prelude

-- | Contains information on a metric attribution. A metric attribution
-- creates reports on the data that you import into Amazon Personalize.
-- Depending on how you import the data, you can view reports in Amazon
-- CloudWatch or Amazon S3. For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/measuring-recommendation-impact.html Measuring impact of recommendations>.
--
-- /See:/ 'newMetricAttribution' smart constructor.
data MetricAttribution = MetricAttribution'
  { -- | The metric attribution\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The metric attribution\'s creation date time.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The metric attribution\'s status.
    status :: Prelude.Maybe Prelude.Text,
    -- | The metric attribution\'s output configuration.
    metricsOutputConfig :: Prelude.Maybe MetricAttributionOutput,
    -- | The metric attribution\'s Amazon Resource Name (ARN).
    metricAttributionArn :: Prelude.Maybe Prelude.Text,
    -- | The metric attribution\'s dataset group Amazon Resource Name (ARN).
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The metric attribution\'s last updated date time.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The metric attribution\'s failure reason.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricAttribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'metricAttribution_name' - The metric attribution\'s name.
--
-- 'creationDateTime', 'metricAttribution_creationDateTime' - The metric attribution\'s creation date time.
--
-- 'status', 'metricAttribution_status' - The metric attribution\'s status.
--
-- 'metricsOutputConfig', 'metricAttribution_metricsOutputConfig' - The metric attribution\'s output configuration.
--
-- 'metricAttributionArn', 'metricAttribution_metricAttributionArn' - The metric attribution\'s Amazon Resource Name (ARN).
--
-- 'datasetGroupArn', 'metricAttribution_datasetGroupArn' - The metric attribution\'s dataset group Amazon Resource Name (ARN).
--
-- 'lastUpdatedDateTime', 'metricAttribution_lastUpdatedDateTime' - The metric attribution\'s last updated date time.
--
-- 'failureReason', 'metricAttribution_failureReason' - The metric attribution\'s failure reason.
newMetricAttribution ::
  MetricAttribution
newMetricAttribution =
  MetricAttribution'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      status = Prelude.Nothing,
      metricsOutputConfig = Prelude.Nothing,
      metricAttributionArn = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The metric attribution\'s name.
metricAttribution_name :: Lens.Lens' MetricAttribution (Prelude.Maybe Prelude.Text)
metricAttribution_name = Lens.lens (\MetricAttribution' {name} -> name) (\s@MetricAttribution' {} a -> s {name = a} :: MetricAttribution)

-- | The metric attribution\'s creation date time.
metricAttribution_creationDateTime :: Lens.Lens' MetricAttribution (Prelude.Maybe Prelude.UTCTime)
metricAttribution_creationDateTime = Lens.lens (\MetricAttribution' {creationDateTime} -> creationDateTime) (\s@MetricAttribution' {} a -> s {creationDateTime = a} :: MetricAttribution) Prelude.. Lens.mapping Data._Time

-- | The metric attribution\'s status.
metricAttribution_status :: Lens.Lens' MetricAttribution (Prelude.Maybe Prelude.Text)
metricAttribution_status = Lens.lens (\MetricAttribution' {status} -> status) (\s@MetricAttribution' {} a -> s {status = a} :: MetricAttribution)

-- | The metric attribution\'s output configuration.
metricAttribution_metricsOutputConfig :: Lens.Lens' MetricAttribution (Prelude.Maybe MetricAttributionOutput)
metricAttribution_metricsOutputConfig = Lens.lens (\MetricAttribution' {metricsOutputConfig} -> metricsOutputConfig) (\s@MetricAttribution' {} a -> s {metricsOutputConfig = a} :: MetricAttribution)

-- | The metric attribution\'s Amazon Resource Name (ARN).
metricAttribution_metricAttributionArn :: Lens.Lens' MetricAttribution (Prelude.Maybe Prelude.Text)
metricAttribution_metricAttributionArn = Lens.lens (\MetricAttribution' {metricAttributionArn} -> metricAttributionArn) (\s@MetricAttribution' {} a -> s {metricAttributionArn = a} :: MetricAttribution)

-- | The metric attribution\'s dataset group Amazon Resource Name (ARN).
metricAttribution_datasetGroupArn :: Lens.Lens' MetricAttribution (Prelude.Maybe Prelude.Text)
metricAttribution_datasetGroupArn = Lens.lens (\MetricAttribution' {datasetGroupArn} -> datasetGroupArn) (\s@MetricAttribution' {} a -> s {datasetGroupArn = a} :: MetricAttribution)

-- | The metric attribution\'s last updated date time.
metricAttribution_lastUpdatedDateTime :: Lens.Lens' MetricAttribution (Prelude.Maybe Prelude.UTCTime)
metricAttribution_lastUpdatedDateTime = Lens.lens (\MetricAttribution' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@MetricAttribution' {} a -> s {lastUpdatedDateTime = a} :: MetricAttribution) Prelude.. Lens.mapping Data._Time

-- | The metric attribution\'s failure reason.
metricAttribution_failureReason :: Lens.Lens' MetricAttribution (Prelude.Maybe Prelude.Text)
metricAttribution_failureReason = Lens.lens (\MetricAttribution' {failureReason} -> failureReason) (\s@MetricAttribution' {} a -> s {failureReason = a} :: MetricAttribution)

instance Data.FromJSON MetricAttribution where
  parseJSON =
    Data.withObject
      "MetricAttribution"
      ( \x ->
          MetricAttribution'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "metricsOutputConfig")
            Prelude.<*> (x Data..:? "metricAttributionArn")
            Prelude.<*> (x Data..:? "datasetGroupArn")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "failureReason")
      )

instance Prelude.Hashable MetricAttribution where
  hashWithSalt _salt MetricAttribution' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` metricsOutputConfig
      `Prelude.hashWithSalt` metricAttributionArn
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData MetricAttribution where
  rnf MetricAttribution' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf metricsOutputConfig
      `Prelude.seq` Prelude.rnf metricAttributionArn
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
