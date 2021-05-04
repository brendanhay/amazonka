{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.TableError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableError where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An error record for table operations.
--
-- /See:/ 'newTableError' smart constructor.
data TableError = TableError'
  { -- | The name of the table. For Hive compatibility, this must be entirely
    -- lowercase.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The details about the error.
    errorDetail :: Prelude.Maybe ErrorDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TableError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'tableError_tableName' - The name of the table. For Hive compatibility, this must be entirely
-- lowercase.
--
-- 'errorDetail', 'tableError_errorDetail' - The details about the error.
newTableError ::
  TableError
newTableError =
  TableError'
    { tableName = Prelude.Nothing,
      errorDetail = Prelude.Nothing
    }

-- | The name of the table. For Hive compatibility, this must be entirely
-- lowercase.
tableError_tableName :: Lens.Lens' TableError (Prelude.Maybe Prelude.Text)
tableError_tableName = Lens.lens (\TableError' {tableName} -> tableName) (\s@TableError' {} a -> s {tableName = a} :: TableError)

-- | The details about the error.
tableError_errorDetail :: Lens.Lens' TableError (Prelude.Maybe ErrorDetail)
tableError_errorDetail = Lens.lens (\TableError' {errorDetail} -> errorDetail) (\s@TableError' {} a -> s {errorDetail = a} :: TableError)

instance Prelude.FromJSON TableError where
  parseJSON =
    Prelude.withObject
      "TableError"
      ( \x ->
          TableError'
            Prelude.<$> (x Prelude..:? "TableName")
            Prelude.<*> (x Prelude..:? "ErrorDetail")
      )

instance Prelude.Hashable TableError

instance Prelude.NFData TableError
