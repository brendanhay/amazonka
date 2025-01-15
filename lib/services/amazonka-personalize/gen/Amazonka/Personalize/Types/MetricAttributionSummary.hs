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
-- Module      : Amazonka.Personalize.Types.MetricAttributionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.MetricAttributionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a metric attribution. For a
-- complete listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeMetricAttribution.html DescribeMetricAttribution>.
--
-- /See:/ 'newMetricAttributionSummary' smart constructor.
data MetricAttributionSummary = MetricAttributionSummary'
  { -- | The metric attribution\'s creation date time.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The metric attribution\'s failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The metric attribution\'s last updated date time.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The metric attribution\'s Amazon Resource Name (ARN).
    metricAttributionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric attribution.
    name :: Prelude.Maybe Prelude.Text,
    -- | The metric attribution\'s status.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricAttributionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'metricAttributionSummary_creationDateTime' - The metric attribution\'s creation date time.
--
-- 'failureReason', 'metricAttributionSummary_failureReason' - The metric attribution\'s failure reason.
--
-- 'lastUpdatedDateTime', 'metricAttributionSummary_lastUpdatedDateTime' - The metric attribution\'s last updated date time.
--
-- 'metricAttributionArn', 'metricAttributionSummary_metricAttributionArn' - The metric attribution\'s Amazon Resource Name (ARN).
--
-- 'name', 'metricAttributionSummary_name' - The name of the metric attribution.
--
-- 'status', 'metricAttributionSummary_status' - The metric attribution\'s status.
newMetricAttributionSummary ::
  MetricAttributionSummary
newMetricAttributionSummary =
  MetricAttributionSummary'
    { creationDateTime =
        Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      metricAttributionArn = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The metric attribution\'s creation date time.
metricAttributionSummary_creationDateTime :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.UTCTime)
metricAttributionSummary_creationDateTime = Lens.lens (\MetricAttributionSummary' {creationDateTime} -> creationDateTime) (\s@MetricAttributionSummary' {} a -> s {creationDateTime = a} :: MetricAttributionSummary) Prelude.. Lens.mapping Data._Time

-- | The metric attribution\'s failure reason.
metricAttributionSummary_failureReason :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.Text)
metricAttributionSummary_failureReason = Lens.lens (\MetricAttributionSummary' {failureReason} -> failureReason) (\s@MetricAttributionSummary' {} a -> s {failureReason = a} :: MetricAttributionSummary)

-- | The metric attribution\'s last updated date time.
metricAttributionSummary_lastUpdatedDateTime :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.UTCTime)
metricAttributionSummary_lastUpdatedDateTime = Lens.lens (\MetricAttributionSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@MetricAttributionSummary' {} a -> s {lastUpdatedDateTime = a} :: MetricAttributionSummary) Prelude.. Lens.mapping Data._Time

-- | The metric attribution\'s Amazon Resource Name (ARN).
metricAttributionSummary_metricAttributionArn :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.Text)
metricAttributionSummary_metricAttributionArn = Lens.lens (\MetricAttributionSummary' {metricAttributionArn} -> metricAttributionArn) (\s@MetricAttributionSummary' {} a -> s {metricAttributionArn = a} :: MetricAttributionSummary)

-- | The name of the metric attribution.
metricAttributionSummary_name :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.Text)
metricAttributionSummary_name = Lens.lens (\MetricAttributionSummary' {name} -> name) (\s@MetricAttributionSummary' {} a -> s {name = a} :: MetricAttributionSummary)

-- | The metric attribution\'s status.
metricAttributionSummary_status :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.Text)
metricAttributionSummary_status = Lens.lens (\MetricAttributionSummary' {status} -> status) (\s@MetricAttributionSummary' {} a -> s {status = a} :: MetricAttributionSummary)

instance Data.FromJSON MetricAttributionSummary where
  parseJSON =
    Data.withObject
      "MetricAttributionSummary"
      ( \x ->
          MetricAttributionSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "metricAttributionArn")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable MetricAttributionSummary where
  hashWithSalt _salt MetricAttributionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` metricAttributionArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData MetricAttributionSummary where
  rnf MetricAttributionSummary' {..} =
    Prelude.rnf creationDateTime `Prelude.seq`
      Prelude.rnf failureReason `Prelude.seq`
        Prelude.rnf lastUpdatedDateTime `Prelude.seq`
          Prelude.rnf metricAttributionArn `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf status
