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
-- Module      : Amazonka.Glue.Types.ColumnError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ColumnError where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.ErrorDetail
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Encapsulates a column name that failed and the reason for failure.
--
-- /See:/ 'newColumnError' smart constructor.
data ColumnError = ColumnError'
  { -- | An error message with the reason for the failure of an operation.
    error :: Prelude.Maybe ErrorDetail,
    -- | The name of the column that failed.
    columnName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'columnError_error' - An error message with the reason for the failure of an operation.
--
-- 'columnName', 'columnError_columnName' - The name of the column that failed.
newColumnError ::
  ColumnError
newColumnError =
  ColumnError'
    { error = Prelude.Nothing,
      columnName = Prelude.Nothing
    }

-- | An error message with the reason for the failure of an operation.
columnError_error :: Lens.Lens' ColumnError (Prelude.Maybe ErrorDetail)
columnError_error = Lens.lens (\ColumnError' {error} -> error) (\s@ColumnError' {} a -> s {error = a} :: ColumnError)

-- | The name of the column that failed.
columnError_columnName :: Lens.Lens' ColumnError (Prelude.Maybe Prelude.Text)
columnError_columnName = Lens.lens (\ColumnError' {columnName} -> columnName) (\s@ColumnError' {} a -> s {columnName = a} :: ColumnError)

instance Core.FromJSON ColumnError where
  parseJSON =
    Core.withObject
      "ColumnError"
      ( \x ->
          ColumnError'
            Prelude.<$> (x Core..:? "Error")
            Prelude.<*> (x Core..:? "ColumnName")
      )

instance Prelude.Hashable ColumnError

instance Prelude.NFData ColumnError
