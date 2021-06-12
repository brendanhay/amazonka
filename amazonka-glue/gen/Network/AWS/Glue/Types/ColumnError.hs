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
-- Module      : Network.AWS.Glue.Types.ColumnError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnError where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens

-- | Encapsulates a column name that failed and the reason for failure.
--
-- /See:/ 'newColumnError' smart constructor.
data ColumnError = ColumnError'
  { -- | The name of the column that failed.
    columnName :: Core.Maybe Core.Text,
    -- | An error message with the reason for the failure of an operation.
    error :: Core.Maybe ErrorDetail
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ColumnError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnName', 'columnError_columnName' - The name of the column that failed.
--
-- 'error', 'columnError_error' - An error message with the reason for the failure of an operation.
newColumnError ::
  ColumnError
newColumnError =
  ColumnError'
    { columnName = Core.Nothing,
      error = Core.Nothing
    }

-- | The name of the column that failed.
columnError_columnName :: Lens.Lens' ColumnError (Core.Maybe Core.Text)
columnError_columnName = Lens.lens (\ColumnError' {columnName} -> columnName) (\s@ColumnError' {} a -> s {columnName = a} :: ColumnError)

-- | An error message with the reason for the failure of an operation.
columnError_error :: Lens.Lens' ColumnError (Core.Maybe ErrorDetail)
columnError_error = Lens.lens (\ColumnError' {error} -> error) (\s@ColumnError' {} a -> s {error = a} :: ColumnError)

instance Core.FromJSON ColumnError where
  parseJSON =
    Core.withObject
      "ColumnError"
      ( \x ->
          ColumnError'
            Core.<$> (x Core..:? "ColumnName")
            Core.<*> (x Core..:? "Error")
      )

instance Core.Hashable ColumnError

instance Core.NFData ColumnError
