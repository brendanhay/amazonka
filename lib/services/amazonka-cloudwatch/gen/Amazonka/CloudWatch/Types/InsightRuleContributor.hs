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
-- Module      : Amazonka.CloudWatch.Types.InsightRuleContributor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.InsightRuleContributor where

import Amazonka.CloudWatch.Types.InsightRuleContributorDatapoint
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
insightRuleContributor_keys = Lens.lens (\InsightRuleContributor' {keys} -> keys) (\s@InsightRuleContributor' {} a -> s {keys = a} :: InsightRuleContributor) Prelude.. Lens.coerced

-- | An approximation of the aggregate value that comes from this
-- contributor.
insightRuleContributor_approximateAggregateValue :: Lens.Lens' InsightRuleContributor Prelude.Double
insightRuleContributor_approximateAggregateValue = Lens.lens (\InsightRuleContributor' {approximateAggregateValue} -> approximateAggregateValue) (\s@InsightRuleContributor' {} a -> s {approximateAggregateValue = a} :: InsightRuleContributor)

-- | An array of the data points where this contributor is present. Only the
-- data points when this contributor appeared are included in the array.
insightRuleContributor_datapoints :: Lens.Lens' InsightRuleContributor [InsightRuleContributorDatapoint]
insightRuleContributor_datapoints = Lens.lens (\InsightRuleContributor' {datapoints} -> datapoints) (\s@InsightRuleContributor' {} a -> s {datapoints = a} :: InsightRuleContributor) Prelude.. Lens.coerced

instance Data.FromXML InsightRuleContributor where
  parseXML x =
    InsightRuleContributor'
      Prelude.<$> ( x Data..@? "Keys" Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "member"
                  )
      Prelude.<*> (x Data..@ "ApproximateAggregateValue")
      Prelude.<*> ( x Data..@? "Datapoints" Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "member"
                  )

instance Prelude.Hashable InsightRuleContributor where
  hashWithSalt _salt InsightRuleContributor' {..} =
    _salt `Prelude.hashWithSalt` keys
      `Prelude.hashWithSalt` approximateAggregateValue
      `Prelude.hashWithSalt` datapoints

instance Prelude.NFData InsightRuleContributor where
  rnf InsightRuleContributor' {..} =
    Prelude.rnf keys
      `Prelude.seq` Prelude.rnf approximateAggregateValue
      `Prelude.seq` Prelude.rnf datapoints
