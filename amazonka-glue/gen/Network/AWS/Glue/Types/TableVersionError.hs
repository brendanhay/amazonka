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
-- Module      : Network.AWS.Glue.Types.TableVersionError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableVersionError where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An error record for table-version operations.
--
-- /See:/ 'newTableVersionError' smart constructor.
data TableVersionError = TableVersionError'
  { -- | The name of the table in question.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The details about the error.
    errorDetail :: Prelude.Maybe ErrorDetail,
    -- | The ID value of the version in question. A @VersionID@ is a string
    -- representation of an integer. Each version is incremented by 1.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TableVersionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'tableVersionError_tableName' - The name of the table in question.
--
-- 'errorDetail', 'tableVersionError_errorDetail' - The details about the error.
--
-- 'versionId', 'tableVersionError_versionId' - The ID value of the version in question. A @VersionID@ is a string
-- representation of an integer. Each version is incremented by 1.
newTableVersionError ::
  TableVersionError
newTableVersionError =
  TableVersionError'
    { tableName = Prelude.Nothing,
      errorDetail = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The name of the table in question.
tableVersionError_tableName :: Lens.Lens' TableVersionError (Prelude.Maybe Prelude.Text)
tableVersionError_tableName = Lens.lens (\TableVersionError' {tableName} -> tableName) (\s@TableVersionError' {} a -> s {tableName = a} :: TableVersionError)

-- | The details about the error.
tableVersionError_errorDetail :: Lens.Lens' TableVersionError (Prelude.Maybe ErrorDetail)
tableVersionError_errorDetail = Lens.lens (\TableVersionError' {errorDetail} -> errorDetail) (\s@TableVersionError' {} a -> s {errorDetail = a} :: TableVersionError)

-- | The ID value of the version in question. A @VersionID@ is a string
-- representation of an integer. Each version is incremented by 1.
tableVersionError_versionId :: Lens.Lens' TableVersionError (Prelude.Maybe Prelude.Text)
tableVersionError_versionId = Lens.lens (\TableVersionError' {versionId} -> versionId) (\s@TableVersionError' {} a -> s {versionId = a} :: TableVersionError)

instance Prelude.FromJSON TableVersionError where
  parseJSON =
    Prelude.withObject
      "TableVersionError"
      ( \x ->
          TableVersionError'
            Prelude.<$> (x Prelude..:? "TableName")
            Prelude.<*> (x Prelude..:? "ErrorDetail")
            Prelude.<*> (x Prelude..:? "VersionId")
      )

instance Prelude.Hashable TableVersionError

instance Prelude.NFData TableVersionError
