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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.MetricAttributionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a metric attribution. For a
-- complete listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeMetricAttribution.html DescribeMetricAttribution>.
--
-- /See:/ 'newMetricAttributionSummary' smart constructor.
data MetricAttributionSummary = MetricAttributionSummary'
  { -- | The name of the metric attribution.
    name :: Prelude.Maybe Prelude.Text,
    -- | The metric attribution\'s creation date time.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The metric attribution\'s status.
    status :: Prelude.Maybe Prelude.Text,
    -- | The metric attribution\'s Amazon Resource Name (ARN).
    metricAttributionArn :: Prelude.Maybe Prelude.Text,
    -- | The metric attribution\'s last updated date time.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The metric attribution\'s failure reason.
    failureReason :: Prelude.Maybe Prelude.Text
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
-- 'name', 'metricAttributionSummary_name' - The name of the metric attribution.
--
-- 'creationDateTime', 'metricAttributionSummary_creationDateTime' - The metric attribution\'s creation date time.
--
-- 'status', 'metricAttributionSummary_status' - The metric attribution\'s status.
--
-- 'metricAttributionArn', 'metricAttributionSummary_metricAttributionArn' - The metric attribution\'s Amazon Resource Name (ARN).
--
-- 'lastUpdatedDateTime', 'metricAttributionSummary_lastUpdatedDateTime' - The metric attribution\'s last updated date time.
--
-- 'failureReason', 'metricAttributionSummary_failureReason' - The metric attribution\'s failure reason.
newMetricAttributionSummary ::
  MetricAttributionSummary
newMetricAttributionSummary =
  MetricAttributionSummary'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      status = Prelude.Nothing,
      metricAttributionArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The name of the metric attribution.
metricAttributionSummary_name :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.Text)
metricAttributionSummary_name = Lens.lens (\MetricAttributionSummary' {name} -> name) (\s@MetricAttributionSummary' {} a -> s {name = a} :: MetricAttributionSummary)

-- | The metric attribution\'s creation date time.
metricAttributionSummary_creationDateTime :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.UTCTime)
metricAttributionSummary_creationDateTime = Lens.lens (\MetricAttributionSummary' {creationDateTime} -> creationDateTime) (\s@MetricAttributionSummary' {} a -> s {creationDateTime = a} :: MetricAttributionSummary) Prelude.. Lens.mapping Core._Time

-- | The metric attribution\'s status.
metricAttributionSummary_status :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.Text)
metricAttributionSummary_status = Lens.lens (\MetricAttributionSummary' {status} -> status) (\s@MetricAttributionSummary' {} a -> s {status = a} :: MetricAttributionSummary)

-- | The metric attribution\'s Amazon Resource Name (ARN).
metricAttributionSummary_metricAttributionArn :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.Text)
metricAttributionSummary_metricAttributionArn = Lens.lens (\MetricAttributionSummary' {metricAttributionArn} -> metricAttributionArn) (\s@MetricAttributionSummary' {} a -> s {metricAttributionArn = a} :: MetricAttributionSummary)

-- | The metric attribution\'s last updated date time.
metricAttributionSummary_lastUpdatedDateTime :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.UTCTime)
metricAttributionSummary_lastUpdatedDateTime = Lens.lens (\MetricAttributionSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@MetricAttributionSummary' {} a -> s {lastUpdatedDateTime = a} :: MetricAttributionSummary) Prelude.. Lens.mapping Core._Time

-- | The metric attribution\'s failure reason.
metricAttributionSummary_failureReason :: Lens.Lens' MetricAttributionSummary (Prelude.Maybe Prelude.Text)
metricAttributionSummary_failureReason = Lens.lens (\MetricAttributionSummary' {failureReason} -> failureReason) (\s@MetricAttributionSummary' {} a -> s {failureReason = a} :: MetricAttributionSummary)

instance Core.FromJSON MetricAttributionSummary where
  parseJSON =
    Core.withObject
      "MetricAttributionSummary"
      ( \x ->
          MetricAttributionSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "metricAttributionArn")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable MetricAttributionSummary where
  hashWithSalt _salt MetricAttributionSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` metricAttributionArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData MetricAttributionSummary where
  rnf MetricAttributionSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf metricAttributionArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
