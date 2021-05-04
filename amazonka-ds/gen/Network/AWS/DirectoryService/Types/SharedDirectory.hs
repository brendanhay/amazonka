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
-- Module      : Network.AWS.DirectoryService.Types.SharedDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SharedDirectory where

import Network.AWS.DirectoryService.Types.ShareMethod
import Network.AWS.DirectoryService.Types.ShareStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the shared directory in the directory owner account for
-- which the share request in the directory consumer account has been
-- accepted.
--
-- /See:/ 'newSharedDirectory' smart constructor.
data SharedDirectory = SharedDirectory'
  { -- | The date and time that the shared directory was created.
    createdDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The date and time that the shared directory was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | Identifier of the directory consumer account that has access to the
    -- shared directory (@OwnerDirectoryId@) in the directory owner account.
    sharedAccountId :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the directory in the directory owner account.
    ownerDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your AWS organization
    -- (@ORGANIZATIONS@) or with any AWS account by sending a shared directory
    -- request (@HANDSHAKE@).
    shareMethod :: Prelude.Maybe ShareMethod,
    -- | Current directory status of the shared AWS Managed Microsoft AD
    -- directory.
    shareStatus :: Prelude.Maybe ShareStatus,
    -- | Identifier of the directory owner account, which contains the directory
    -- that has been shared to the consumer account.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the shared directory in the directory consumer account.
    -- This identifier is different for each directory owner account.
    sharedDirectoryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { createdDateTime = Prelude.Nothing,
      shareNotes = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      sharedAccountId = Prelude.Nothing,
      ownerDirectoryId = Prelude.Nothing,
      shareMethod = Prelude.Nothing,
      shareStatus = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      sharedDirectoryId = Prelude.Nothing
    }

-- | The date and time that the shared directory was created.
sharedDirectory_createdDateTime :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.UTCTime)
sharedDirectory_createdDateTime = Lens.lens (\SharedDirectory' {createdDateTime} -> createdDateTime) (\s@SharedDirectory' {} a -> s {createdDateTime = a} :: SharedDirectory) Prelude.. Lens.mapping Prelude._Time

-- | A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
sharedDirectory_shareNotes :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_shareNotes = Lens.lens (\SharedDirectory' {shareNotes} -> shareNotes) (\s@SharedDirectory' {} a -> s {shareNotes = a} :: SharedDirectory) Prelude.. Lens.mapping Prelude._Sensitive

-- | The date and time that the shared directory was last updated.
sharedDirectory_lastUpdatedDateTime :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.UTCTime)
sharedDirectory_lastUpdatedDateTime = Lens.lens (\SharedDirectory' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SharedDirectory' {} a -> s {lastUpdatedDateTime = a} :: SharedDirectory) Prelude.. Lens.mapping Prelude._Time

-- | Identifier of the directory consumer account that has access to the
-- shared directory (@OwnerDirectoryId@) in the directory owner account.
sharedDirectory_sharedAccountId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_sharedAccountId = Lens.lens (\SharedDirectory' {sharedAccountId} -> sharedAccountId) (\s@SharedDirectory' {} a -> s {sharedAccountId = a} :: SharedDirectory)

-- | Identifier of the directory in the directory owner account.
sharedDirectory_ownerDirectoryId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_ownerDirectoryId = Lens.lens (\SharedDirectory' {ownerDirectoryId} -> ownerDirectoryId) (\s@SharedDirectory' {} a -> s {ownerDirectoryId = a} :: SharedDirectory)

-- | The method used when sharing a directory to determine whether the
-- directory should be shared within your AWS organization
-- (@ORGANIZATIONS@) or with any AWS account by sending a shared directory
-- request (@HANDSHAKE@).
sharedDirectory_shareMethod :: Lens.Lens' SharedDirectory (Prelude.Maybe ShareMethod)
sharedDirectory_shareMethod = Lens.lens (\SharedDirectory' {shareMethod} -> shareMethod) (\s@SharedDirectory' {} a -> s {shareMethod = a} :: SharedDirectory)

-- | Current directory status of the shared AWS Managed Microsoft AD
-- directory.
sharedDirectory_shareStatus :: Lens.Lens' SharedDirectory (Prelude.Maybe ShareStatus)
sharedDirectory_shareStatus = Lens.lens (\SharedDirectory' {shareStatus} -> shareStatus) (\s@SharedDirectory' {} a -> s {shareStatus = a} :: SharedDirectory)

-- | Identifier of the directory owner account, which contains the directory
-- that has been shared to the consumer account.
sharedDirectory_ownerAccountId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_ownerAccountId = Lens.lens (\SharedDirectory' {ownerAccountId} -> ownerAccountId) (\s@SharedDirectory' {} a -> s {ownerAccountId = a} :: SharedDirectory)

-- | Identifier of the shared directory in the directory consumer account.
-- This identifier is different for each directory owner account.
sharedDirectory_sharedDirectoryId :: Lens.Lens' SharedDirectory (Prelude.Maybe Prelude.Text)
sharedDirectory_sharedDirectoryId = Lens.lens (\SharedDirectory' {sharedDirectoryId} -> sharedDirectoryId) (\s@SharedDirectory' {} a -> s {sharedDirectoryId = a} :: SharedDirectory)

instance Prelude.FromJSON SharedDirectory where
  parseJSON =
    Prelude.withObject
      "SharedDirectory"
      ( \x ->
          SharedDirectory'
            Prelude.<$> (x Prelude..:? "CreatedDateTime")
            Prelude.<*> (x Prelude..:? "ShareNotes")
            Prelude.<*> (x Prelude..:? "LastUpdatedDateTime")
            Prelude.<*> (x Prelude..:? "SharedAccountId")
            Prelude.<*> (x Prelude..:? "OwnerDirectoryId")
            Prelude.<*> (x Prelude..:? "ShareMethod")
            Prelude.<*> (x Prelude..:? "ShareStatus")
            Prelude.<*> (x Prelude..:? "OwnerAccountId")
            Prelude.<*> (x Prelude..:? "SharedDirectoryId")
      )

instance Prelude.Hashable SharedDirectory

instance Prelude.NFData SharedDirectory
