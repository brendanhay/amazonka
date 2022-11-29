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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.SharedDirectory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types.ShareMethod
import Amazonka.DirectoryService.Types.ShareStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about the shared directory in the directory owner account for
-- which the share request in the directory consumer account has been
-- accepted.
--
-- /See:/ 'newSharedDirectory' smart constructor.
data SharedDirectory = SharedDirectory'
  { -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Current directory status of the shared Managed Microsoft AD directory.
    shareStatus :: Prelude.Maybe ShareStatus,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your Amazon Web Services organization
    -- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
    -- shared directory request (@HANDSHAKE@).
    shareMethod :: Prelude.Maybe ShareMethod,
    -- | The date and time that the shared directory was created.
    createdDateTime :: Prelude.Maybe Core.POSIX,
    -- | Identifier of the directory in the directory owner account.
    ownerDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the directory owner account, which contains the directory
    -- that has been shared to the consumer account.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the directory consumer account that has access to the
    -- shared directory (@OwnerDirectoryId@) in the directory owner account.
    sharedAccountId :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the shared directory in the directory consumer account.
    -- This identifier is different for each directory owner account.
    sharedDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the shared directory was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
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
-- 'shareNotes', 'sharedDirectory_shareNotes' - A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
--
-- 'shareStatus', 'sharedDirectory_shareStatus' - Current directory status of the shared Managed Microsoft AD directory.
--
-- 'shareMethod', 'sharedDirectory_shareMethod' - The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- shared directory request (@HANDSHAKE@).
--
-- 'createdDateTime', 'sharedDirectory_createdDateTime' - The date and time that the shared directory was created.
--
-- 'ownerDirectoryId', 'sharedDirectory_ownerDirectoryId' - Identifier of the directory in the directory owner account.
--
-- 'ownerAccountId', 'sharedDirectory_ownerAccountId' - Identifier of the directory owner account, which contains the directory
-- that has been shared to the consumer account.
--
-- 'sharedAccountId', 'sharedDirectory_sharedAccountId' - Identifier of the directory consumer account that has access to the
-- shared directory (@OwnerDirectoryId@) in the directory owner account.
--
-- 'sharedDirectoryId', 'sharedDirectory_sharedDirectoryId' - Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
--
-- 'lastUpdatedDateTime', 'sharedDirectory_lastUpdatedDateTime' - The date and time that the shared directory was last updated.
newSharedDirectory ::
  SharedDirectory
newSharedDirectory =
  SharedDirectory'
    { shareNotes = Prelude.Nothing,
      shareStatus = Prelude.Nothing,
      shareMethod = Prelude.Nothing,
      createdDateTime = Prelude.Nothing,
      ownerDirectoryId = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      sharedAccountId = Prelude.Nothing,
      sharedDirectoryId = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
sharedDirectory_shareNotes :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_shareNotes = Lens.lens (\SharedDirectory' {shareNotes} -> shareNotes) (\s@SharedDirectory' {} a -> s {shareNotes = a} :: SharedDirectory) Prelude.. Lens.mapping Core._Sensitive

-- | Current directory status of the shared Managed Microsoft AD directory.
sharedDirectory_shareStatus :: Lens.Lens' SharedDirectory (Prelude.Maybe ShareStatus)
sharedDirectory_shareStatus = Lens.lens (\SharedDirectory' {shareStatus} -> shareStatus) (\s@SharedDirectory' {} a -> s {shareStatus = a} :: SharedDirectory)

-- | The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- shared directory request (@HANDSHAKE@).
sharedDirectory_shareMethod :: Lens.Lens' SharedDirectory (Prelude.Maybe ShareMethod)
sharedDirectory_shareMethod = Lens.lens (\SharedDirectory' {shareMethod} -> shareMethod) (\s@SharedDirectory' {} a -> s {shareMethod = a} :: SharedDirectory)

-- | The date and time that the shared directory was created.
sharedDirectory_createdDateTime :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.UTCTime)
sharedDirectory_createdDateTime = Lens.lens (\SharedDirectory' {createdDateTime} -> createdDateTime) (\s@SharedDirectory' {} a -> s {createdDateTime = a} :: SharedDirectory) Prelude.. Lens.mapping Core._Time

-- | Identifier of the directory in the directory owner account.
sharedDirectory_ownerDirectoryId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_ownerDirectoryId = Lens.lens (\SharedDirectory' {ownerDirectoryId} -> ownerDirectoryId) (\s@SharedDirectory' {} a -> s {ownerDirectoryId = a} :: SharedDirectory)

-- | Identifier of the directory owner account, which contains the directory
-- that has been shared to the consumer account.
sharedDirectory_ownerAccountId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_ownerAccountId = Lens.lens (\SharedDirectory' {ownerAccountId} -> ownerAccountId) (\s@SharedDirectory' {} a -> s {ownerAccountId = a} :: SharedDirectory)

-- | Identifier of the directory consumer account that has access to the
-- shared directory (@OwnerDirectoryId@) in the directory owner account.
sharedDirectory_sharedAccountId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_sharedAccountId = Lens.lens (\SharedDirectory' {sharedAccountId} -> sharedAccountId) (\s@SharedDirectory' {} a -> s {sharedAccountId = a} :: SharedDirectory)

-- | Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
sharedDirectory_sharedDirectoryId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_sharedDirectoryId = Lens.lens (\SharedDirectory' {sharedDirectoryId} -> sharedDirectoryId) (\s@SharedDirectory' {} a -> s {sharedDirectoryId = a} :: SharedDirectory)

-- | The date and time that the shared directory was last updated.
sharedDirectory_lastUpdatedDateTime :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.UTCTime)
sharedDirectory_lastUpdatedDateTime = Lens.lens (\SharedDirectory' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SharedDirectory' {} a -> s {lastUpdatedDateTime = a} :: SharedDirectory) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SharedDirectory where
  parseJSON =
    Core.withObject
      "SharedDirectory"
      ( \x ->
          SharedDirectory'
            Prelude.<$> (x Core..:? "ShareNotes")
            Prelude.<*> (x Core..:? "ShareStatus")
            Prelude.<*> (x Core..:? "ShareMethod")
            Prelude.<*> (x Core..:? "CreatedDateTime")
            Prelude.<*> (x Core..:? "OwnerDirectoryId")
            Prelude.<*> (x Core..:? "OwnerAccountId")
            Prelude.<*> (x Core..:? "SharedAccountId")
            Prelude.<*> (x Core..:? "SharedDirectoryId")
            Prelude.<*> (x Core..:? "LastUpdatedDateTime")
      )

instance Prelude.Hashable SharedDirectory where
  hashWithSalt _salt SharedDirectory' {..} =
    _salt `Prelude.hashWithSalt` shareNotes
      `Prelude.hashWithSalt` shareStatus
      `Prelude.hashWithSalt` shareMethod
      `Prelude.hashWithSalt` createdDateTime
      `Prelude.hashWithSalt` ownerDirectoryId
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` sharedAccountId
      `Prelude.hashWithSalt` sharedDirectoryId
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData SharedDirectory where
  rnf SharedDirectory' {..} =
    Prelude.rnf shareNotes
      `Prelude.seq` Prelude.rnf shareStatus
      `Prelude.seq` Prelude.rnf shareMethod
      `Prelude.seq` Prelude.rnf createdDateTime
      `Prelude.seq` Prelude.rnf ownerDirectoryId
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf sharedAccountId
      `Prelude.seq` Prelude.rnf sharedDirectoryId
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
