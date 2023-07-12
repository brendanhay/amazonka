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
-- Module      : Amazonka.DirectoryService.Types.SharedDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.SharedDirectory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.ShareMethod
import Amazonka.DirectoryService.Types.ShareStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about the shared directory in the directory owner account for
-- which the share request in the directory consumer account has been
-- accepted.
--
-- /See:/ 'newSharedDirectory' smart constructor.
data SharedDirectory = SharedDirectory'
  { -- | The date and time that the shared directory was created.
    createdDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the shared directory was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Identifier of the directory owner account, which contains the directory
    -- that has been shared to the consumer account.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the directory in the directory owner account.
    ownerDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your Amazon Web Services organization
    -- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
    -- shared directory request (@HANDSHAKE@).
    shareMethod :: Prelude.Maybe ShareMethod,
    -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Current directory status of the shared Managed Microsoft AD directory.
    shareStatus :: Prelude.Maybe ShareStatus,
    -- | Identifier of the directory consumer account that has access to the
    -- shared directory (@OwnerDirectoryId@) in the directory owner account.
    sharedAccountId :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the shared directory in the directory consumer account.
    -- This identifier is different for each directory owner account.
    sharedDirectoryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SharedDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDateTime', 'sharedDirectory_createdDateTime' - The date and time that the shared directory was created.
--
-- 'lastUpdatedDateTime', 'sharedDirectory_lastUpdatedDateTime' - The date and time that the shared directory was last updated.
--
-- 'ownerAccountId', 'sharedDirectory_ownerAccountId' - Identifier of the directory owner account, which contains the directory
-- that has been shared to the consumer account.
--
-- 'ownerDirectoryId', 'sharedDirectory_ownerDirectoryId' - Identifier of the directory in the directory owner account.
--
-- 'shareMethod', 'sharedDirectory_shareMethod' - The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- shared directory request (@HANDSHAKE@).
--
-- 'shareNotes', 'sharedDirectory_shareNotes' - A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
--
-- 'shareStatus', 'sharedDirectory_shareStatus' - Current directory status of the shared Managed Microsoft AD directory.
--
-- 'sharedAccountId', 'sharedDirectory_sharedAccountId' - Identifier of the directory consumer account that has access to the
-- shared directory (@OwnerDirectoryId@) in the directory owner account.
--
-- 'sharedDirectoryId', 'sharedDirectory_sharedDirectoryId' - Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
newSharedDirectory ::
  SharedDirectory
newSharedDirectory =
  SharedDirectory'
    { createdDateTime = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      ownerDirectoryId = Prelude.Nothing,
      shareMethod = Prelude.Nothing,
      shareNotes = Prelude.Nothing,
      shareStatus = Prelude.Nothing,
      sharedAccountId = Prelude.Nothing,
      sharedDirectoryId = Prelude.Nothing
    }

-- | The date and time that the shared directory was created.
sharedDirectory_createdDateTime :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.UTCTime)
sharedDirectory_createdDateTime = Lens.lens (\SharedDirectory' {createdDateTime} -> createdDateTime) (\s@SharedDirectory' {} a -> s {createdDateTime = a} :: SharedDirectory) Prelude.. Lens.mapping Data._Time

-- | The date and time that the shared directory was last updated.
sharedDirectory_lastUpdatedDateTime :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.UTCTime)
sharedDirectory_lastUpdatedDateTime = Lens.lens (\SharedDirectory' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SharedDirectory' {} a -> s {lastUpdatedDateTime = a} :: SharedDirectory) Prelude.. Lens.mapping Data._Time

-- | Identifier of the directory owner account, which contains the directory
-- that has been shared to the consumer account.
sharedDirectory_ownerAccountId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_ownerAccountId = Lens.lens (\SharedDirectory' {ownerAccountId} -> ownerAccountId) (\s@SharedDirectory' {} a -> s {ownerAccountId = a} :: SharedDirectory)

-- | Identifier of the directory in the directory owner account.
sharedDirectory_ownerDirectoryId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_ownerDirectoryId = Lens.lens (\SharedDirectory' {ownerDirectoryId} -> ownerDirectoryId) (\s@SharedDirectory' {} a -> s {ownerDirectoryId = a} :: SharedDirectory)

-- | The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- shared directory request (@HANDSHAKE@).
sharedDirectory_shareMethod :: Lens.Lens' SharedDirectory (Prelude.Maybe ShareMethod)
sharedDirectory_shareMethod = Lens.lens (\SharedDirectory' {shareMethod} -> shareMethod) (\s@SharedDirectory' {} a -> s {shareMethod = a} :: SharedDirectory)

-- | A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
sharedDirectory_shareNotes :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_shareNotes = Lens.lens (\SharedDirectory' {shareNotes} -> shareNotes) (\s@SharedDirectory' {} a -> s {shareNotes = a} :: SharedDirectory) Prelude.. Lens.mapping Data._Sensitive

-- | Current directory status of the shared Managed Microsoft AD directory.
sharedDirectory_shareStatus :: Lens.Lens' SharedDirectory (Prelude.Maybe ShareStatus)
sharedDirectory_shareStatus = Lens.lens (\SharedDirectory' {shareStatus} -> shareStatus) (\s@SharedDirectory' {} a -> s {shareStatus = a} :: SharedDirectory)

-- | Identifier of the directory consumer account that has access to the
-- shared directory (@OwnerDirectoryId@) in the directory owner account.
sharedDirectory_sharedAccountId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_sharedAccountId = Lens.lens (\SharedDirectory' {sharedAccountId} -> sharedAccountId) (\s@SharedDirectory' {} a -> s {sharedAccountId = a} :: SharedDirectory)

-- | Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
sharedDirectory_sharedDirectoryId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_sharedDirectoryId = Lens.lens (\SharedDirectory' {sharedDirectoryId} -> sharedDirectoryId) (\s@SharedDirectory' {} a -> s {sharedDirectoryId = a} :: SharedDirectory)

instance Data.FromJSON SharedDirectory where
  parseJSON =
    Data.withObject
      "SharedDirectory"
      ( \x ->
          SharedDirectory'
            Prelude.<$> (x Data..:? "CreatedDateTime")
            Prelude.<*> (x Data..:? "LastUpdatedDateTime")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "OwnerDirectoryId")
            Prelude.<*> (x Data..:? "ShareMethod")
            Prelude.<*> (x Data..:? "ShareNotes")
            Prelude.<*> (x Data..:? "ShareStatus")
            Prelude.<*> (x Data..:? "SharedAccountId")
            Prelude.<*> (x Data..:? "SharedDirectoryId")
      )

instance Prelude.Hashable SharedDirectory where
  hashWithSalt _salt SharedDirectory' {..} =
    _salt
      `Prelude.hashWithSalt` createdDateTime
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` ownerDirectoryId
      `Prelude.hashWithSalt` shareMethod
      `Prelude.hashWithSalt` shareNotes
      `Prelude.hashWithSalt` shareStatus
      `Prelude.hashWithSalt` sharedAccountId
      `Prelude.hashWithSalt` sharedDirectoryId

instance Prelude.NFData SharedDirectory where
  rnf SharedDirectory' {..} =
    Prelude.rnf createdDateTime
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf ownerDirectoryId
      `Prelude.seq` Prelude.rnf shareMethod
      `Prelude.seq` Prelude.rnf shareNotes
      `Prelude.seq` Prelude.rnf shareStatus
      `Prelude.seq` Prelude.rnf sharedAccountId
      `Prelude.seq` Prelude.rnf sharedDirectoryId
