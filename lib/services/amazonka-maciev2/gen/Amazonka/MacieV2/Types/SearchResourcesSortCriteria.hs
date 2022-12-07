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
-- Module      : Amazonka.MacieV2.Types.SearchResourcesSortCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SearchResourcesSortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.OrderBy
import Amazonka.MacieV2.Types.SearchResourcesSortAttributeName
import qualified Amazonka.Prelude as Prelude

-- | Specifies criteria for sorting the results of a query for information
-- about Amazon Web Services resources that Amazon Macie monitors and
-- analyzes.
--
-- /See:/ 'newSearchResourcesSortCriteria' smart constructor.
data SearchResourcesSortCriteria = SearchResourcesSortCriteria'
  { -- | The sort order to apply to the results, based on the value for the
    -- property specified by the attributeName property. Valid values are: ASC,
    -- sort the results in ascending order; and, DESC, sort the results in
    -- descending order.
    orderBy :: Prelude.Maybe OrderBy,
    -- | The property to sort the results by.
    attributeName :: Prelude.Maybe SearchResourcesSortAttributeName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesSortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderBy', 'searchResourcesSortCriteria_orderBy' - The sort order to apply to the results, based on the value for the
-- property specified by the attributeName property. Valid values are: ASC,
-- sort the results in ascending order; and, DESC, sort the results in
-- descending order.
--
-- 'attributeName', 'searchResourcesSortCriteria_attributeName' - The property to sort the results by.
newSearchResourcesSortCriteria ::
  SearchResourcesSortCriteria
newSearchResourcesSortCriteria =
  SearchResourcesSortCriteria'
    { orderBy =
        Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The sort order to apply to the results, based on the value for the
-- property specified by the attributeName property. Valid values are: ASC,
-- sort the results in ascending order; and, DESC, sort the results in
-- descending order.
searchResourcesSortCriteria_orderBy :: Lens.Lens' SearchResourcesSortCriteria (Prelude.Maybe OrderBy)
searchResourcesSortCriteria_orderBy = Lens.lens (\SearchResourcesSortCriteria' {orderBy} -> orderBy) (\s@SearchResourcesSortCriteria' {} a -> s {orderBy = a} :: SearchResourcesSortCriteria)

-- | The property to sort the results by.
searchResourcesSortCriteria_attributeName :: Lens.Lens' SearchResourcesSortCriteria (Prelude.Maybe SearchResourcesSortAttributeName)
searchResourcesSortCriteria_attributeName = Lens.lens (\SearchResourcesSortCriteria' {attributeName} -> attributeName) (\s@SearchResourcesSortCriteria' {} a -> s {attributeName = a} :: SearchResourcesSortCriteria)

instance Prelude.Hashable SearchResourcesSortCriteria where
  hashWithSalt _salt SearchResourcesSortCriteria' {..} =
    _salt `Prelude.hashWithSalt` orderBy
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData SearchResourcesSortCriteria where
  rnf SearchResourcesSortCriteria' {..} =
    Prelude.rnf orderBy
      `Prelude.seq` Prelude.rnf attributeName

instance Data.ToJSON SearchResourcesSortCriteria where
  toJSON SearchResourcesSortCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("orderBy" Data..=) Prelude.<$> orderBy,
            ("attributeName" Data..=) Prelude.<$> attributeName
          ]
      )
