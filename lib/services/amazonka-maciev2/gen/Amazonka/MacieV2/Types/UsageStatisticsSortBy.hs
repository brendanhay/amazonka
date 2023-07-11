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
-- Module      : Amazonka.MacieV2.Types.UsageStatisticsSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UsageStatisticsSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.OrderBy
import Amazonka.MacieV2.Types.UsageStatisticsSortKey
import qualified Amazonka.Prelude as Prelude

-- | Specifies criteria for sorting the results of a query for Amazon Macie
-- account quotas and usage data.
--
-- /See:/ 'newUsageStatisticsSortBy' smart constructor.
data UsageStatisticsSortBy = UsageStatisticsSortBy'
  { -- | The field to sort the results by.
    key :: Prelude.Maybe UsageStatisticsSortKey,
    -- | The sort order to apply to the results, based on the value for the field
    -- specified by the key property. Valid values are: ASC, sort the results
    -- in ascending order; and, DESC, sort the results in descending order.
    orderBy :: Prelude.Maybe OrderBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageStatisticsSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'usageStatisticsSortBy_key' - The field to sort the results by.
--
-- 'orderBy', 'usageStatisticsSortBy_orderBy' - The sort order to apply to the results, based on the value for the field
-- specified by the key property. Valid values are: ASC, sort the results
-- in ascending order; and, DESC, sort the results in descending order.
newUsageStatisticsSortBy ::
  UsageStatisticsSortBy
newUsageStatisticsSortBy =
  UsageStatisticsSortBy'
    { key = Prelude.Nothing,
      orderBy = Prelude.Nothing
    }

-- | The field to sort the results by.
usageStatisticsSortBy_key :: Lens.Lens' UsageStatisticsSortBy (Prelude.Maybe UsageStatisticsSortKey)
usageStatisticsSortBy_key = Lens.lens (\UsageStatisticsSortBy' {key} -> key) (\s@UsageStatisticsSortBy' {} a -> s {key = a} :: UsageStatisticsSortBy)

-- | The sort order to apply to the results, based on the value for the field
-- specified by the key property. Valid values are: ASC, sort the results
-- in ascending order; and, DESC, sort the results in descending order.
usageStatisticsSortBy_orderBy :: Lens.Lens' UsageStatisticsSortBy (Prelude.Maybe OrderBy)
usageStatisticsSortBy_orderBy = Lens.lens (\UsageStatisticsSortBy' {orderBy} -> orderBy) (\s@UsageStatisticsSortBy' {} a -> s {orderBy = a} :: UsageStatisticsSortBy)

instance Prelude.Hashable UsageStatisticsSortBy where
  hashWithSalt _salt UsageStatisticsSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` orderBy

instance Prelude.NFData UsageStatisticsSortBy where
  rnf UsageStatisticsSortBy' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf orderBy

instance Data.ToJSON UsageStatisticsSortBy where
  toJSON UsageStatisticsSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("orderBy" Data..=) Prelude.<$> orderBy
          ]
      )
