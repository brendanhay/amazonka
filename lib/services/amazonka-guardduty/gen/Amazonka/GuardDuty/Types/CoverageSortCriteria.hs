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
-- Module      : Amazonka.GuardDuty.Types.CoverageSortCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageSortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.CoverageSortKey
import Amazonka.GuardDuty.Types.OrderBy
import qualified Amazonka.Prelude as Prelude

-- | Information about the sorting criteria used in the coverage statistics.
--
-- /See:/ 'newCoverageSortCriteria' smart constructor.
data CoverageSortCriteria = CoverageSortCriteria'
  { -- | Represents the field name used to sort the coverage details.
    attributeName :: Prelude.Maybe CoverageSortKey,
    -- | The order in which the sorted findings are to be displayed.
    orderBy :: Prelude.Maybe OrderBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageSortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'coverageSortCriteria_attributeName' - Represents the field name used to sort the coverage details.
--
-- 'orderBy', 'coverageSortCriteria_orderBy' - The order in which the sorted findings are to be displayed.
newCoverageSortCriteria ::
  CoverageSortCriteria
newCoverageSortCriteria =
  CoverageSortCriteria'
    { attributeName =
        Prelude.Nothing,
      orderBy = Prelude.Nothing
    }

-- | Represents the field name used to sort the coverage details.
coverageSortCriteria_attributeName :: Lens.Lens' CoverageSortCriteria (Prelude.Maybe CoverageSortKey)
coverageSortCriteria_attributeName = Lens.lens (\CoverageSortCriteria' {attributeName} -> attributeName) (\s@CoverageSortCriteria' {} a -> s {attributeName = a} :: CoverageSortCriteria)

-- | The order in which the sorted findings are to be displayed.
coverageSortCriteria_orderBy :: Lens.Lens' CoverageSortCriteria (Prelude.Maybe OrderBy)
coverageSortCriteria_orderBy = Lens.lens (\CoverageSortCriteria' {orderBy} -> orderBy) (\s@CoverageSortCriteria' {} a -> s {orderBy = a} :: CoverageSortCriteria)

instance Prelude.Hashable CoverageSortCriteria where
  hashWithSalt _salt CoverageSortCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` orderBy

instance Prelude.NFData CoverageSortCriteria where
  rnf CoverageSortCriteria' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf orderBy

instance Data.ToJSON CoverageSortCriteria where
  toJSON CoverageSortCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributeName" Data..=) Prelude.<$> attributeName,
            ("orderBy" Data..=) Prelude.<$> orderBy
          ]
      )
