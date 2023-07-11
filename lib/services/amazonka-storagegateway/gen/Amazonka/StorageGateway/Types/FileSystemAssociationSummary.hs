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
-- Module      : Amazonka.StorageGateway.Types.FileSystemAssociationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.FileSystemAssociationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Gets the summary returned by @ListFileSystemAssociation@, which is a
-- summary of a created file system association.
--
-- /See:/ 'newFileSystemAssociationSummary' smart constructor.
data FileSystemAssociationSummary = FileSystemAssociationSummary'
  { -- | The Amazon Resource Name (ARN) of the file system association.
    fileSystemAssociationARN :: Prelude.Maybe Prelude.Text,
    -- | The ID of the file system association.
    fileSystemAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The status of the file share. Valid Values: @AVAILABLE@ | @CREATING@ |
    -- @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
    fileSystemAssociationStatus :: Prelude.Maybe Prelude.Text,
    gatewayARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemAssociationARN', 'fileSystemAssociationSummary_fileSystemAssociationARN' - The Amazon Resource Name (ARN) of the file system association.
--
-- 'fileSystemAssociationId', 'fileSystemAssociationSummary_fileSystemAssociationId' - The ID of the file system association.
--
-- 'fileSystemAssociationStatus', 'fileSystemAssociationSummary_fileSystemAssociationStatus' - The status of the file share. Valid Values: @AVAILABLE@ | @CREATING@ |
-- @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
--
-- 'gatewayARN', 'fileSystemAssociationSummary_gatewayARN' - Undocumented member.
newFileSystemAssociationSummary ::
  FileSystemAssociationSummary
newFileSystemAssociationSummary =
  FileSystemAssociationSummary'
    { fileSystemAssociationARN =
        Prelude.Nothing,
      fileSystemAssociationId = Prelude.Nothing,
      fileSystemAssociationStatus = Prelude.Nothing,
      gatewayARN = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the file system association.
fileSystemAssociationSummary_fileSystemAssociationARN :: Lens.Lens' FileSystemAssociationSummary (Prelude.Maybe Prelude.Text)
fileSystemAssociationSummary_fileSystemAssociationARN = Lens.lens (\FileSystemAssociationSummary' {fileSystemAssociationARN} -> fileSystemAssociationARN) (\s@FileSystemAssociationSummary' {} a -> s {fileSystemAssociationARN = a} :: FileSystemAssociationSummary)

-- | The ID of the file system association.
fileSystemAssociationSummary_fileSystemAssociationId :: Lens.Lens' FileSystemAssociationSummary (Prelude.Maybe Prelude.Text)
fileSystemAssociationSummary_fileSystemAssociationId = Lens.lens (\FileSystemAssociationSummary' {fileSystemAssociationId} -> fileSystemAssociationId) (\s@FileSystemAssociationSummary' {} a -> s {fileSystemAssociationId = a} :: FileSystemAssociationSummary)

-- | The status of the file share. Valid Values: @AVAILABLE@ | @CREATING@ |
-- @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
fileSystemAssociationSummary_fileSystemAssociationStatus :: Lens.Lens' FileSystemAssociationSummary (Prelude.Maybe Prelude.Text)
fileSystemAssociationSummary_fileSystemAssociationStatus = Lens.lens (\FileSystemAssociationSummary' {fileSystemAssociationStatus} -> fileSystemAssociationStatus) (\s@FileSystemAssociationSummary' {} a -> s {fileSystemAssociationStatus = a} :: FileSystemAssociationSummary)

-- | Undocumented member.
fileSystemAssociationSummary_gatewayARN :: Lens.Lens' FileSystemAssociationSummary (Prelude.Maybe Prelude.Text)
fileSystemAssociationSummary_gatewayARN = Lens.lens (\FileSystemAssociationSummary' {gatewayARN} -> gatewayARN) (\s@FileSystemAssociationSummary' {} a -> s {gatewayARN = a} :: FileSystemAssociationSummary)

instance Data.FromJSON FileSystemAssociationSummary where
  parseJSON =
    Data.withObject
      "FileSystemAssociationSummary"
      ( \x ->
          FileSystemAssociationSummary'
            Prelude.<$> (x Data..:? "FileSystemAssociationARN")
            Prelude.<*> (x Data..:? "FileSystemAssociationId")
            Prelude.<*> (x Data..:? "FileSystemAssociationStatus")
            Prelude.<*> (x Data..:? "GatewayARN")
      )

instance
  Prelude.Hashable
    FileSystemAssociationSummary
  where
  hashWithSalt _salt FileSystemAssociationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` fileSystemAssociationARN
      `Prelude.hashWithSalt` fileSystemAssociationId
      `Prelude.hashWithSalt` fileSystemAssociationStatus
      `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData FileSystemAssociationSummary where
  rnf FileSystemAssociationSummary' {..} =
    Prelude.rnf fileSystemAssociationARN
      `Prelude.seq` Prelude.rnf fileSystemAssociationId
      `Prelude.seq` Prelude.rnf fileSystemAssociationStatus
      `Prelude.seq` Prelude.rnf gatewayARN
