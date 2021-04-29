{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    timestamp :: Prelude.ISO8601,
    -- | The approximate value that this contributor added during this timestamp.
    approximateValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.UTCTime ->
  -- | 'approximateValue'
  Prelude.Double ->
  InsightRuleContributorDatapoint
newInsightRuleContributorDatapoint
  pTimestamp_
  pApproximateValue_ =
    InsightRuleContributorDatapoint'
      { timestamp =
          Prelude._Time Lens.# pTimestamp_,
        approximateValue = pApproximateValue_
      }

-- | The timestamp of the data point.
insightRuleContributorDatapoint_timestamp :: Lens.Lens' InsightRuleContributorDatapoint Prelude.UTCTime
insightRuleContributorDatapoint_timestamp = Lens.lens (\InsightRuleContributorDatapoint' {timestamp} -> timestamp) (\s@InsightRuleContributorDatapoint' {} a -> s {timestamp = a} :: InsightRuleContributorDatapoint) Prelude.. Prelude._Time

-- | The approximate value that this contributor added during this timestamp.
insightRuleContributorDatapoint_approximateValue :: Lens.Lens' InsightRuleContributorDatapoint Prelude.Double
insightRuleContributorDatapoint_approximateValue = Lens.lens (\InsightRuleContributorDatapoint' {approximateValue} -> approximateValue) (\s@InsightRuleContributorDatapoint' {} a -> s {approximateValue = a} :: InsightRuleContributorDatapoint)

instance
  Prelude.FromXML
    InsightRuleContributorDatapoint
  where
  parseXML x =
    InsightRuleContributorDatapoint'
      Prelude.<$> (x Prelude..@ "Timestamp")
      Prelude.<*> (x Prelude..@ "ApproximateValue")

instance
  Prelude.Hashable
    InsightRuleContributorDatapoint

instance
  Prelude.NFData
    InsightRuleContributorDatapoint
