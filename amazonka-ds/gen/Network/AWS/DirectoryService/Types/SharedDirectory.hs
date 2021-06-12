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
-- Module      : Network.AWS.DirectoryService.Types.SharedDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SharedDirectory where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.ShareMethod
import Network.AWS.DirectoryService.Types.ShareStatus
import qualified Network.AWS.Lens as Lens

-- | Details about the shared directory in the directory owner account for
-- which the share request in the directory consumer account has been
-- accepted.
--
-- /See:/ 'newSharedDirectory' smart constructor.
data SharedDirectory = SharedDirectory'
  { -- | The date and time that the shared directory was created.
    createdDateTime :: Core.Maybe Core.POSIX,
    -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The date and time that the shared directory was last updated.
    lastUpdatedDateTime :: Core.Maybe Core.POSIX,
    -- | Identifier of the directory consumer account that has access to the
    -- shared directory (@OwnerDirectoryId@) in the directory owner account.
    sharedAccountId :: Core.Maybe Core.Text,
    -- | Identifier of the directory in the directory owner account.
    ownerDirectoryId :: Core.Maybe Core.Text,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your AWS organization
    -- (@ORGANIZATIONS@) or with any AWS account by sending a shared directory
    -- request (@HANDSHAKE@).
    shareMethod :: Core.Maybe ShareMethod,
    -- | Current directory status of the shared AWS Managed Microsoft AD
    -- directory.
    shareStatus :: Core.Maybe ShareStatus,
    -- | Identifier of the directory owner account, which contains the directory
    -- that has been shared to the consumer account.
    ownerAccountId :: Core.Maybe Core.Text,
    -- | Identifier of the shared directory in the directory consumer account.
    -- This identifier is different for each directory owner account.
    sharedDirectoryId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- 'shareNotes', 'sharedDirectory_shareNotes' - A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
--
-- 'lastUpdatedDateTime', 'sharedDirectory_lastUpdatedDateTime' - The date and time that the shared directory was last updated.
--
-- 'sharedAccountId', 'sharedDirectory_sharedAccountId' - Identifier of the directory consumer account that has access to the
-- shared directory (@OwnerDirectoryId@) in the directory owner account.
--
-- 'ownerDirectoryId', 'sharedDirectory_ownerDirectoryId' - Identifier of the directory in the directory owner account.
--
-- 'shareMethod', 'sharedDirectory_shareMethod' - The method used when sharing a directory to determine whether the
-- directory should be shared within your AWS organization
-- (@ORGANIZATIONS@) or with any AWS account by sending a shared directory
-- request (@HANDSHAKE@).
--
-- 'shareStatus', 'sharedDirectory_shareStatus' - Current directory status of the shared AWS Managed Microsoft AD
-- directory.
--
-- 'ownerAccountId', 'sharedDirectory_ownerAccountId' - Identifier of the directory owner account, which contains the directory
-- that has been shared to the consumer account.
--
-- 'sharedDirectoryId', 'sharedDirectory_sharedDirectoryId' - Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
newSharedDirectory ::
  SharedDirectory
newSharedDirectory =
  SharedDirectory'
    { createdDateTime = Core.Nothing,
      shareNotes = Core.Nothing,
      lastUpdatedDateTime = Core.Nothing,
      sharedAccountId = Core.Nothing,
      ownerDirectoryId = Core.Nothing,
      shareMethod = Core.Nothing,
      shareStatus = Core.Nothing,
      ownerAccountId = Core.Nothing,
      sharedDirectoryId = Core.Nothing
    }

-- | The date and time that the shared directory was created.
sharedDirectory_createdDateTime :: Lens.Lens' SharedDirectory (Core.Maybe Core.UTCTime)
sharedDirectory_createdDateTime = Lens.lens (\SharedDirectory' {createdDateTime} -> createdDateTime) (\s@SharedDirectory' {} a -> s {createdDateTime = a} :: SharedDirectory) Core.. Lens.mapping Core._Time

-- | A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
sharedDirectory_shareNotes :: Lens.Lens' SharedDirectory (Core.Maybe Core.Text)
sharedDirectory_shareNotes = Lens.lens (\SharedDirectory' {shareNotes} -> shareNotes) (\s@SharedDirectory' {} a -> s {shareNotes = a} :: SharedDirectory) Core.. Lens.mapping Core._Sensitive

-- | The date and time that the shared directory was last updated.
sharedDirectory_lastUpdatedDateTime :: Lens.Lens' SharedDirectory (Core.Maybe Core.UTCTime)
sharedDirectory_lastUpdatedDateTime = Lens.lens (\SharedDirectory' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SharedDirectory' {} a -> s {lastUpdatedDateTime = a} :: SharedDirectory) Core.. Lens.mapping Core._Time

-- | Identifier of the directory consumer account that has access to the
-- shared directory (@OwnerDirectoryId@) in the directory owner account.
sharedDirectory_sharedAccountId :: Lens.Lens' SharedDirectory (Core.Maybe Core.Text)
sharedDirectory_sharedAccountId = Lens.lens (\SharedDirectory' {sharedAccountId} -> sharedAccountId) (\s@SharedDirectory' {} a -> s {sharedAccountId = a} :: SharedDirectory)

-- | Identifier of the directory in the directory owner account.
sharedDirectory_ownerDirectoryId :: Lens.Lens' SharedDirectory (Core.Maybe Core.Text)
sharedDirectory_ownerDirectoryId = Lens.lens (\SharedDirectory' {ownerDirectoryId} -> ownerDirectoryId) (\s@SharedDirectory' {} a -> s {ownerDirectoryId = a} :: SharedDirectory)

-- | The method used when sharing a directory to determine whether the
-- directory should be shared within your AWS organization
-- (@ORGANIZATIONS@) or with any AWS account by sending a shared directory
-- request (@HANDSHAKE@).
sharedDirectory_shareMethod :: Lens.Lens' SharedDirectory (Core.Maybe ShareMethod)
sharedDirectory_shareMethod = Lens.lens (\SharedDirectory' {shareMethod} -> shareMethod) (\s@SharedDirectory' {} a -> s {shareMethod = a} :: SharedDirectory)

-- | Current directory status of the shared AWS Managed Microsoft AD
-- directory.
sharedDirectory_shareStatus :: Lens.Lens' SharedDirectory (Core.Maybe ShareStatus)
sharedDirectory_shareStatus = Lens.lens (\SharedDirectory' {shareStatus} -> shareStatus) (\s@SharedDirectory' {} a -> s {shareStatus = a} :: SharedDirectory)

-- | Identifier of the directory owner account, which contains the directory
-- that has been shared to the consumer account.
sharedDirectory_ownerAccountId :: Lens.Lens' SharedDirectory (Core.Maybe Core.Text)
sharedDirectory_ownerAccountId = Lens.lens (\SharedDirectory' {ownerAccountId} -> ownerAccountId) (\s@SharedDirectory' {} a -> s {ownerAccountId = a} :: SharedDirectory)

-- | Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
sharedDirectory_sharedDirectoryId :: Lens.Lens' SharedDirectory (Core.Maybe Core.Text)
sharedDirectory_sharedDirectoryId = Lens.lens (\SharedDirectory' {sharedDirectoryId} -> sharedDirectoryId) (\s@SharedDirectory' {} a -> s {sharedDirectoryId = a} :: SharedDirectory)

instance Core.FromJSON SharedDirectory where
  parseJSON =
    Core.withObject
      "SharedDirectory"
      ( \x ->
          SharedDirectory'
            Core.<$> (x Core..:? "CreatedDateTime")
            Core.<*> (x Core..:? "ShareNotes")
            Core.<*> (x Core..:? "LastUpdatedDateTime")
            Core.<*> (x Core..:? "SharedAccountId")
            Core.<*> (x Core..:? "OwnerDirectoryId")
            Core.<*> (x Core..:? "ShareMethod")
            Core.<*> (x Core..:? "ShareStatus")
            Core.<*> (x Core..:? "OwnerAccountId")
            Core.<*> (x Core..:? "SharedDirectoryId")
      )

instance Core.Hashable SharedDirectory

instance Core.NFData SharedDirectory
