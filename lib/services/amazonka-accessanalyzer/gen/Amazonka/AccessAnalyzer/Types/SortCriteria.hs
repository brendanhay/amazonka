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
-- Module      : Amazonka.AccessAnalyzer.Types.SortCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.SortCriteria where

import Amazonka.AccessAnalyzer.Types.OrderBy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The criteria used to sort.
--
-- /See:/ 'newSortCriteria' smart constructor.
data SortCriteria = SortCriteria'
  { -- | The name of the attribute to sort on.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | The sort order, ascending or descending.
    orderBy :: Prelude.Maybe OrderBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'sortCriteria_attributeName' - The name of the attribute to sort on.
--
-- 'orderBy', 'sortCriteria_orderBy' - The sort order, ascending or descending.
newSortCriteria ::
  SortCriteria
newSortCriteria =
  SortCriteria'
    { attributeName = Prelude.Nothing,
      orderBy = Prelude.Nothing
    }

-- | The name of the attribute to sort on.
sortCriteria_attributeName :: Lens.Lens' SortCriteria (Prelude.Maybe Prelude.Text)
sortCriteria_attributeName = Lens.lens (\SortCriteria' {attributeName} -> attributeName) (\s@SortCriteria' {} a -> s {attributeName = a} :: SortCriteria)

-- | The sort order, ascending or descending.
sortCriteria_orderBy :: Lens.Lens' SortCriteria (Prelude.Maybe OrderBy)
sortCriteria_orderBy = Lens.lens (\SortCriteria' {orderBy} -> orderBy) (\s@SortCriteria' {} a -> s {orderBy = a} :: SortCriteria)

instance Prelude.Hashable SortCriteria where
  hashWithSalt _salt SortCriteria' {..} =
    _salt `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` orderBy

instance Prelude.NFData SortCriteria where
  rnf SortCriteria' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf orderBy

instance Data.ToJSON SortCriteria where
  toJSON SortCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributeName" Data..=) Prelude.<$> attributeName,
            ("orderBy" Data..=) Prelude.<$> orderBy
          ]
      )
