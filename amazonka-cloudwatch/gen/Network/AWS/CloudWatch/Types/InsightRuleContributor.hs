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
-- Module      : Network.AWS.CloudWatch.Types.InsightRuleContributor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRuleContributor where

import Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | One of the unique contributors found by a Contributor Insights rule. If
-- the rule contains multiple keys, then a unique contributor is a unique
-- combination of values from all the keys in the rule.
--
-- If the rule contains a single key, then each unique contributor is each
-- unique value for this key.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport>.
--
-- /See:/ 'newInsightRuleContributor' smart constructor.
data InsightRuleContributor = InsightRuleContributor'
  { -- | One of the log entry field keywords that is used to define contributors
    -- for this rule.
    keys :: [Prelude.Text],
    -- | An approximation of the aggregate value that comes from this
    -- contributor.
    approximateAggregateValue :: Prelude.Double,
    -- | An array of the data points where this contributor is present. Only the
    -- data points when this contributor appeared are included in the array.
    datapoints :: [InsightRuleContributorDatapoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InsightRuleContributor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keys', 'insightRuleContributor_keys' - One of the log entry field keywords that is used to define contributors
-- for this rule.
--
-- 'approximateAggregateValue', 'insightRuleContributor_approximateAggregateValue' - An approximation of the aggregate value that comes from this
-- contributor.
--
-- 'datapoints', 'insightRuleContributor_datapoints' - An array of the data points where this contributor is present. Only the
-- data points when this contributor appeared are included in the array.
newInsightRuleContributor ::
  -- | 'approximateAggregateValue'
  Prelude.Double ->
  InsightRuleContributor
newInsightRuleContributor pApproximateAggregateValue_ =
  InsightRuleContributor'
    { keys = Prelude.mempty,
      approximateAggregateValue =
        pApproximateAggregateValue_,
      datapoints = Prelude.mempty
    }

-- | One of the log entry field keywords that is used to define contributors
-- for this rule.
insightRuleContributor_keys :: Lens.Lens' InsightRuleContributor [Prelude.Text]
insightRuleContributor_keys = Lens.lens (\InsightRuleContributor' {keys} -> keys) (\s@InsightRuleContributor' {} a -> s {keys = a} :: InsightRuleContributor) Prelude.. Prelude._Coerce

-- | An approximation of the aggregate value that comes from this
-- contributor.
insightRuleContributor_approximateAggregateValue :: Lens.Lens' InsightRuleContributor Prelude.Double
insightRuleContributor_approximateAggregateValue = Lens.lens (\InsightRuleContributor' {approximateAggregateValue} -> approximateAggregateValue) (\s@InsightRuleContributor' {} a -> s {approximateAggregateValue = a} :: InsightRuleContributor)

-- | An array of the data points where this contributor is present. Only the
-- data points when this contributor appeared are included in the array.
insightRuleContributor_datapoints :: Lens.Lens' InsightRuleContributor [InsightRuleContributorDatapoint]
insightRuleContributor_datapoints = Lens.lens (\InsightRuleContributor' {datapoints} -> datapoints) (\s@InsightRuleContributor' {} a -> s {datapoints = a} :: InsightRuleContributor) Prelude.. Prelude._Coerce

instance Prelude.FromXML InsightRuleContributor where
  parseXML x =
    InsightRuleContributor'
      Prelude.<$> ( x Prelude..@? "Keys" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.parseXMLList "member"
                  )
      Prelude.<*> (x Prelude..@ "ApproximateAggregateValue")
      Prelude.<*> ( x Prelude..@? "Datapoints"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.parseXMLList "member"
                  )

instance Prelude.Hashable InsightRuleContributor

instance Prelude.NFData InsightRuleContributor
