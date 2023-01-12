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
-- Module      : Amazonka.StorageGateway.Types.FileSystemAssociationStatusDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.FileSystemAssociationStatusDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed information on file system association status.
--
-- /See:/ 'newFileSystemAssociationStatusDetail' smart constructor.
data FileSystemAssociationStatusDetail = FileSystemAssociationStatusDetail'
  { -- | The error code for a given file system association status.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemAssociationStatusDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'fileSystemAssociationStatusDetail_errorCode' - The error code for a given file system association status.
newFileSystemAssociationStatusDetail ::
  FileSystemAssociationStatusDetail
newFileSystemAssociationStatusDetail =
  FileSystemAssociationStatusDetail'
    { errorCode =
        Prelude.Nothing
    }

-- | The error code for a given file system association status.
fileSystemAssociationStatusDetail_errorCode :: Lens.Lens' FileSystemAssociationStatusDetail (Prelude.Maybe Prelude.Text)
fileSystemAssociationStatusDetail_errorCode = Lens.lens (\FileSystemAssociationStatusDetail' {errorCode} -> errorCode) (\s@FileSystemAssociationStatusDetail' {} a -> s {errorCode = a} :: FileSystemAssociationStatusDetail)

instance
  Data.FromJSON
    FileSystemAssociationStatusDetail
  where
  parseJSON =
    Data.withObject
      "FileSystemAssociationStatusDetail"
      ( \x ->
          FileSystemAssociationStatusDetail'
            Prelude.<$> (x Data..:? "ErrorCode")
      )

instance
  Prelude.Hashable
    FileSystemAssociationStatusDetail
  where
  hashWithSalt
    _salt
    FileSystemAssociationStatusDetail' {..} =
      _salt `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    FileSystemAssociationStatusDetail
  where
  rnf FileSystemAssociationStatusDetail' {..} =
    Prelude.rnf errorCode
