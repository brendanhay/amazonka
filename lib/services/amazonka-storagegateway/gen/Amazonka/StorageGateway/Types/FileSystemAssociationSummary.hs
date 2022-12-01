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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.FileSystemAssociationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Gets the summary returned by @ListFileSystemAssociation@, which is a
-- summary of a created file system association.
--
-- /See:/ 'newFileSystemAssociationSummary' smart constructor.
data FileSystemAssociationSummary = FileSystemAssociationSummary'
  { -- | The ID of the file system association.
    fileSystemAssociationId :: Prelude.Maybe Prelude.Text,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The status of the file share. Valid Values: @AVAILABLE@ | @CREATING@ |
    -- @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
    fileSystemAssociationStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the file system association.
    fileSystemAssociationARN :: Prelude.Maybe Prelude.Text
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
-- 'fileSystemAssociationId', 'fileSystemAssociationSummary_fileSystemAssociationId' - The ID of the file system association.
--
-- 'gatewayARN', 'fileSystemAssociationSummary_gatewayARN' - Undocumented member.
--
-- 'fileSystemAssociationStatus', 'fileSystemAssociationSummary_fileSystemAssociationStatus' - The status of the file share. Valid Values: @AVAILABLE@ | @CREATING@ |
-- @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
--
-- 'fileSystemAssociationARN', 'fileSystemAssociationSummary_fileSystemAssociationARN' - The Amazon Resource Name (ARN) of the file system association.
newFileSystemAssociationSummary ::
  FileSystemAssociationSummary
newFileSystemAssociationSummary =
  FileSystemAssociationSummary'
    { fileSystemAssociationId =
        Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      fileSystemAssociationStatus = Prelude.Nothing,
      fileSystemAssociationARN = Prelude.Nothing
    }

-- | The ID of the file system association.
fileSystemAssociationSummary_fileSystemAssociationId :: Lens.Lens' FileSystemAssociationSummary (Prelude.Maybe Prelude.Text)
fileSystemAssociationSummary_fileSystemAssociationId = Lens.lens (\FileSystemAssociationSummary' {fileSystemAssociationId} -> fileSystemAssociationId) (\s@FileSystemAssociationSummary' {} a -> s {fileSystemAssociationId = a} :: FileSystemAssociationSummary)

-- | Undocumented member.
fileSystemAssociationSummary_gatewayARN :: Lens.Lens' FileSystemAssociationSummary (Prelude.Maybe Prelude.Text)
fileSystemAssociationSummary_gatewayARN = Lens.lens (\FileSystemAssociationSummary' {gatewayARN} -> gatewayARN) (\s@FileSystemAssociationSummary' {} a -> s {gatewayARN = a} :: FileSystemAssociationSummary)

-- | The status of the file share. Valid Values: @AVAILABLE@ | @CREATING@ |
-- @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
fileSystemAssociationSummary_fileSystemAssociationStatus :: Lens.Lens' FileSystemAssociationSummary (Prelude.Maybe Prelude.Text)
fileSystemAssociationSummary_fileSystemAssociationStatus = Lens.lens (\FileSystemAssociationSummary' {fileSystemAssociationStatus} -> fileSystemAssociationStatus) (\s@FileSystemAssociationSummary' {} a -> s {fileSystemAssociationStatus = a} :: FileSystemAssociationSummary)

-- | The Amazon Resource Name (ARN) of the file system association.
fileSystemAssociationSummary_fileSystemAssociationARN :: Lens.Lens' FileSystemAssociationSummary (Prelude.Maybe Prelude.Text)
fileSystemAssociationSummary_fileSystemAssociationARN = Lens.lens (\FileSystemAssociationSummary' {fileSystemAssociationARN} -> fileSystemAssociationARN) (\s@FileSystemAssociationSummary' {} a -> s {fileSystemAssociationARN = a} :: FileSystemAssociationSummary)

instance Core.FromJSON FileSystemAssociationSummary where
  parseJSON =
    Core.withObject
      "FileSystemAssociationSummary"
      ( \x ->
          FileSystemAssociationSummary'
            Prelude.<$> (x Core..:? "FileSystemAssociationId")
            Prelude.<*> (x Core..:? "GatewayARN")
            Prelude.<*> (x Core..:? "FileSystemAssociationStatus")
            Prelude.<*> (x Core..:? "FileSystemAssociationARN")
      )

instance
  Prelude.Hashable
    FileSystemAssociationSummary
  where
  hashWithSalt _salt FileSystemAssociationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` fileSystemAssociationId
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` fileSystemAssociationStatus
      `Prelude.hashWithSalt` fileSystemAssociationARN

instance Prelude.NFData FileSystemAssociationSummary where
  rnf FileSystemAssociationSummary' {..} =
    Prelude.rnf fileSystemAssociationId
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf fileSystemAssociationStatus
      `Prelude.seq` Prelude.rnf fileSystemAssociationARN
