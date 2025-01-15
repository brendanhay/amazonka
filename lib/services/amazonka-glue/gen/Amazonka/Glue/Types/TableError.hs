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
-- Module      : Amazonka.Glue.Types.TableError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TableError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ErrorDetail
import qualified Amazonka.Prelude as Prelude

-- | An error record for table operations.
--
-- /See:/ 'newTableError' smart constructor.
data TableError = TableError'
  { -- | The details about the error.
    errorDetail :: Prelude.Maybe ErrorDetail,
    -- | The name of the table. For Hive compatibility, this must be entirely
    -- lowercase.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDetail', 'tableError_errorDetail' - The details about the error.
--
-- 'tableName', 'tableError_tableName' - The name of the table. For Hive compatibility, this must be entirely
-- lowercase.
newTableError ::
  TableError
newTableError =
  TableError'
    { errorDetail = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The details about the error.
tableError_errorDetail :: Lens.Lens' TableError (Prelude.Maybe ErrorDetail)
tableError_errorDetail = Lens.lens (\TableError' {errorDetail} -> errorDetail) (\s@TableError' {} a -> s {errorDetail = a} :: TableError)

-- | The name of the table. For Hive compatibility, this must be entirely
-- lowercase.
tableError_tableName :: Lens.Lens' TableError (Prelude.Maybe Prelude.Text)
tableError_tableName = Lens.lens (\TableError' {tableName} -> tableName) (\s@TableError' {} a -> s {tableName = a} :: TableError)

instance Data.FromJSON TableError where
  parseJSON =
    Data.withObject
      "TableError"
      ( \x ->
          TableError'
            Prelude.<$> (x Data..:? "ErrorDetail")
            Prelude.<*> (x Data..:? "TableName")
      )

instance Prelude.Hashable TableError where
  hashWithSalt _salt TableError' {..} =
    _salt
      `Prelude.hashWithSalt` errorDetail
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData TableError where
  rnf TableError' {..} =
    Prelude.rnf errorDetail `Prelude.seq`
      Prelude.rnf tableName
