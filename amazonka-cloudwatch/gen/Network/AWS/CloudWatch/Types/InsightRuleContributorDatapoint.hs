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
-- Module      : Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | One data point related to one contributor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport>
-- and
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_InsightRuleContributor.html InsightRuleContributor>.
--
-- /See:/ 'newInsightRuleContributorDatapoint' smart constructor.
data InsightRuleContributorDatapoint = InsightRuleContributorDatapoint'
  { -- | The timestamp of the data point.
    timestamp :: Core.ISO8601,
    -- | The approximate value that this contributor added during this timestamp.
    approximateValue :: Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InsightRuleContributorDatapoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'insightRuleContributorDatapoint_timestamp' - The timestamp of the data point.
--
-- 'approximateValue', 'insightRuleContributorDatapoint_approximateValue' - The approximate value that this contributor added during this timestamp.
newInsightRuleContributorDatapoint ::
  -- | 'timestamp'
  Core.UTCTime ->
  -- | 'approximateValue'
  Core.Double ->
  InsightRuleContributorDatapoint
newInsightRuleContributorDatapoint
  pTimestamp_
  pApproximateValue_ =
    InsightRuleContributorDatapoint'
      { timestamp =
          Core._Time Lens.# pTimestamp_,
        approximateValue = pApproximateValue_
      }

-- | The timestamp of the data point.
insightRuleContributorDatapoint_timestamp :: Lens.Lens' InsightRuleContributorDatapoint Core.UTCTime
insightRuleContributorDatapoint_timestamp = Lens.lens (\InsightRuleContributorDatapoint' {timestamp} -> timestamp) (\s@InsightRuleContributorDatapoint' {} a -> s {timestamp = a} :: InsightRuleContributorDatapoint) Core.. Core._Time

-- | The approximate value that this contributor added during this timestamp.
insightRuleContributorDatapoint_approximateValue :: Lens.Lens' InsightRuleContributorDatapoint Core.Double
insightRuleContributorDatapoint_approximateValue = Lens.lens (\InsightRuleContributorDatapoint' {approximateValue} -> approximateValue) (\s@InsightRuleContributorDatapoint' {} a -> s {approximateValue = a} :: InsightRuleContributorDatapoint)

instance Core.FromXML InsightRuleContributorDatapoint where
  parseXML x =
    InsightRuleContributorDatapoint'
      Core.<$> (x Core..@ "Timestamp")
      Core.<*> (x Core..@ "ApproximateValue")

instance
  Core.Hashable
    InsightRuleContributorDatapoint

instance Core.NFData InsightRuleContributorDatapoint
