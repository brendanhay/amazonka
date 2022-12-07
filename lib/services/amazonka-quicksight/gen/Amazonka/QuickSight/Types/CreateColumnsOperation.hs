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
-- Module      : Amazonka.QuickSight.Types.CreateColumnsOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CreateColumnsOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CalculatedColumn

-- | A transform operation that creates calculated columns. Columns created
-- in one such operation form a lexical closure.
--
-- /See:/ 'newCreateColumnsOperation' smart constructor.
data CreateColumnsOperation = CreateColumnsOperation'
  { -- | Calculated columns to create.
    columns :: Prelude.NonEmpty CalculatedColumn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateColumnsOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columns', 'createColumnsOperation_columns' - Calculated columns to create.
newCreateColumnsOperation ::
  -- | 'columns'
  Prelude.NonEmpty CalculatedColumn ->
  CreateColumnsOperation
newCreateColumnsOperation pColumns_ =
  CreateColumnsOperation'
    { columns =
        Lens.coerced Lens.# pColumns_
    }

-- | Calculated columns to create.
createColumnsOperation_columns :: Lens.Lens' CreateColumnsOperation (Prelude.NonEmpty CalculatedColumn)
createColumnsOperation_columns = Lens.lens (\CreateColumnsOperation' {columns} -> columns) (\s@CreateColumnsOperation' {} a -> s {columns = a} :: CreateColumnsOperation) Prelude.. Lens.coerced

instance Data.FromJSON CreateColumnsOperation where
  parseJSON =
    Data.withObject
      "CreateColumnsOperation"
      ( \x ->
          CreateColumnsOperation'
            Prelude.<$> (x Data..: "Columns")
      )

instance Prelude.Hashable CreateColumnsOperation where
  hashWithSalt _salt CreateColumnsOperation' {..} =
    _salt `Prelude.hashWithSalt` columns

instance Prelude.NFData CreateColumnsOperation where
  rnf CreateColumnsOperation' {..} = Prelude.rnf columns

instance Data.ToJSON CreateColumnsOperation where
  toJSON CreateColumnsOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Columns" Data..= columns)]
      )
