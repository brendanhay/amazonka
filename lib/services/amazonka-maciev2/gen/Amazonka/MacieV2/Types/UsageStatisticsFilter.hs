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
-- Module      : Amazonka.MacieV2.Types.UsageStatisticsFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UsageStatisticsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.UsageStatisticsFilterComparator
import Amazonka.MacieV2.Types.UsageStatisticsFilterKey
import qualified Amazonka.Prelude as Prelude

-- | Specifies a condition for filtering the results of a query for quota and
-- usage data for one or more Amazon Macie accounts.
--
-- /See:/ 'newUsageStatisticsFilter' smart constructor.
data UsageStatisticsFilter = UsageStatisticsFilter'
  { -- | The operator to use in the condition. If the value for the key property
    -- is accountId, this value must be CONTAINS. If the value for the key
    -- property is any other supported field, this value can be EQ, GT, GTE,
    -- LT, LTE, or NE.
    comparator :: Prelude.Maybe UsageStatisticsFilterComparator,
    -- | The field to use in the condition.
    key :: Prelude.Maybe UsageStatisticsFilterKey,
    -- | An array that lists values to use in the condition, based on the value
    -- for the field specified by the key property. If the value for the key
    -- property is accountId, this array can specify multiple values.
    -- Otherwise, this array can specify only one value.
    --
    -- Valid values for each supported field are:
    --
    -- -   accountId - The unique identifier for an Amazon Web Services
    --     account.
    --
    -- -   freeTrialStartDate - The date and time, in UTC and extended ISO 8601
    --     format, when the Amazon Macie free trial started for an account.
    --
    -- -   serviceLimit - A Boolean (true or false) value that indicates
    --     whether an account has reached its monthly quota.
    --
    -- -   total - A string that represents the current estimated cost for an
    --     account.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageStatisticsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparator', 'usageStatisticsFilter_comparator' - The operator to use in the condition. If the value for the key property
-- is accountId, this value must be CONTAINS. If the value for the key
-- property is any other supported field, this value can be EQ, GT, GTE,
-- LT, LTE, or NE.
--
-- 'key', 'usageStatisticsFilter_key' - The field to use in the condition.
--
-- 'values', 'usageStatisticsFilter_values' - An array that lists values to use in the condition, based on the value
-- for the field specified by the key property. If the value for the key
-- property is accountId, this array can specify multiple values.
-- Otherwise, this array can specify only one value.
--
-- Valid values for each supported field are:
--
-- -   accountId - The unique identifier for an Amazon Web Services
--     account.
--
-- -   freeTrialStartDate - The date and time, in UTC and extended ISO 8601
--     format, when the Amazon Macie free trial started for an account.
--
-- -   serviceLimit - A Boolean (true or false) value that indicates
--     whether an account has reached its monthly quota.
--
-- -   total - A string that represents the current estimated cost for an
--     account.
newUsageStatisticsFilter ::
  UsageStatisticsFilter
newUsageStatisticsFilter =
  UsageStatisticsFilter'
    { comparator =
        Prelude.Nothing,
      key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The operator to use in the condition. If the value for the key property
-- is accountId, this value must be CONTAINS. If the value for the key
-- property is any other supported field, this value can be EQ, GT, GTE,
-- LT, LTE, or NE.
usageStatisticsFilter_comparator :: Lens.Lens' UsageStatisticsFilter (Prelude.Maybe UsageStatisticsFilterComparator)
usageStatisticsFilter_comparator = Lens.lens (\UsageStatisticsFilter' {comparator} -> comparator) (\s@UsageStatisticsFilter' {} a -> s {comparator = a} :: UsageStatisticsFilter)

-- | The field to use in the condition.
usageStatisticsFilter_key :: Lens.Lens' UsageStatisticsFilter (Prelude.Maybe UsageStatisticsFilterKey)
usageStatisticsFilter_key = Lens.lens (\UsageStatisticsFilter' {key} -> key) (\s@UsageStatisticsFilter' {} a -> s {key = a} :: UsageStatisticsFilter)

-- | An array that lists values to use in the condition, based on the value
-- for the field specified by the key property. If the value for the key
-- property is accountId, this array can specify multiple values.
-- Otherwise, this array can specify only one value.
--
-- Valid values for each supported field are:
--
-- -   accountId - The unique identifier for an Amazon Web Services
--     account.
--
-- -   freeTrialStartDate - The date and time, in UTC and extended ISO 8601
--     format, when the Amazon Macie free trial started for an account.
--
-- -   serviceLimit - A Boolean (true or false) value that indicates
--     whether an account has reached its monthly quota.
--
-- -   total - A string that represents the current estimated cost for an
--     account.
usageStatisticsFilter_values :: Lens.Lens' UsageStatisticsFilter (Prelude.Maybe [Prelude.Text])
usageStatisticsFilter_values = Lens.lens (\UsageStatisticsFilter' {values} -> values) (\s@UsageStatisticsFilter' {} a -> s {values = a} :: UsageStatisticsFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UsageStatisticsFilter where
  hashWithSalt _salt UsageStatisticsFilter' {..} =
    _salt `Prelude.hashWithSalt` comparator
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData UsageStatisticsFilter where
  rnf UsageStatisticsFilter' {..} =
    Prelude.rnf comparator
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON UsageStatisticsFilter where
  toJSON UsageStatisticsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comparator" Data..=) Prelude.<$> comparator,
            ("key" Data..=) Prelude.<$> key,
            ("values" Data..=) Prelude.<$> values
          ]
      )
