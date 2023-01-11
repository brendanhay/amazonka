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
-- Module      : Amazonka.MacieV2.Types.FindingStatisticsSortCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FindingStatisticsSortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.FindingStatisticsSortAttributeName
import Amazonka.MacieV2.Types.OrderBy
import qualified Amazonka.Prelude as Prelude

-- | Specifies criteria for sorting the results of a query that retrieves
-- aggregated statistical data about findings.
--
-- /See:/ 'newFindingStatisticsSortCriteria' smart constructor.
data FindingStatisticsSortCriteria = FindingStatisticsSortCriteria'
  { -- | The grouping to sort the results by. Valid values are: count, sort the
    -- results by the number of findings in each group of results; and,
    -- groupKey, sort the results by the name of each group of results.
    attributeName :: Prelude.Maybe FindingStatisticsSortAttributeName,
    -- | The sort order to apply to the results, based on the value for the
    -- property specified by the attributeName property. Valid values are: ASC,
    -- sort the results in ascending order; and, DESC, sort the results in
    -- descending order.
    orderBy :: Prelude.Maybe OrderBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingStatisticsSortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'findingStatisticsSortCriteria_attributeName' - The grouping to sort the results by. Valid values are: count, sort the
-- results by the number of findings in each group of results; and,
-- groupKey, sort the results by the name of each group of results.
--
-- 'orderBy', 'findingStatisticsSortCriteria_orderBy' - The sort order to apply to the results, based on the value for the
-- property specified by the attributeName property. Valid values are: ASC,
-- sort the results in ascending order; and, DESC, sort the results in
-- descending order.
newFindingStatisticsSortCriteria ::
  FindingStatisticsSortCriteria
newFindingStatisticsSortCriteria =
  FindingStatisticsSortCriteria'
    { attributeName =
        Prelude.Nothing,
      orderBy = Prelude.Nothing
    }

-- | The grouping to sort the results by. Valid values are: count, sort the
-- results by the number of findings in each group of results; and,
-- groupKey, sort the results by the name of each group of results.
findingStatisticsSortCriteria_attributeName :: Lens.Lens' FindingStatisticsSortCriteria (Prelude.Maybe FindingStatisticsSortAttributeName)
findingStatisticsSortCriteria_attributeName = Lens.lens (\FindingStatisticsSortCriteria' {attributeName} -> attributeName) (\s@FindingStatisticsSortCriteria' {} a -> s {attributeName = a} :: FindingStatisticsSortCriteria)

-- | The sort order to apply to the results, based on the value for the
-- property specified by the attributeName property. Valid values are: ASC,
-- sort the results in ascending order; and, DESC, sort the results in
-- descending order.
findingStatisticsSortCriteria_orderBy :: Lens.Lens' FindingStatisticsSortCriteria (Prelude.Maybe OrderBy)
findingStatisticsSortCriteria_orderBy = Lens.lens (\FindingStatisticsSortCriteria' {orderBy} -> orderBy) (\s@FindingStatisticsSortCriteria' {} a -> s {orderBy = a} :: FindingStatisticsSortCriteria)

instance
  Prelude.Hashable
    FindingStatisticsSortCriteria
  where
  hashWithSalt _salt FindingStatisticsSortCriteria' {..} =
    _salt `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` orderBy

instance Prelude.NFData FindingStatisticsSortCriteria where
  rnf FindingStatisticsSortCriteria' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf orderBy

instance Data.ToJSON FindingStatisticsSortCriteria where
  toJSON FindingStatisticsSortCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributeName" Data..=) Prelude.<$> attributeName,
            ("orderBy" Data..=) Prelude.<$> orderBy
          ]
      )
