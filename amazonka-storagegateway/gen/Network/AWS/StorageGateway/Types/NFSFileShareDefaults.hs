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
-- Module      : Network.AWS.StorageGateway.Types.NFSFileShareDefaults
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.NFSFileShareDefaults where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes Network File System (NFS) file share default values. Files and
-- folders stored as Amazon S3 objects in S3 buckets don\'t, by default,
-- have Unix file permissions assigned to them. Upon discovery in an S3
-- bucket by Storage Gateway, the S3 objects that represent files and
-- folders are assigned these default Unix permissions. This operation is
-- only supported for file gateways.
--
-- /See:/ 'newNFSFileShareDefaults' smart constructor.
data NFSFileShareDefaults = NFSFileShareDefaults'
  { -- | The default owner ID for files in the file share (unless the files have
    -- another owner ID specified). The default value is @nfsnobody@.
    ownerId :: Core.Maybe Core.Natural,
    -- | The default group ID for the file share (unless the files have another
    -- group ID specified). The default value is @nfsnobody@.
    groupId :: Core.Maybe Core.Natural,
    -- | The Unix directory mode in the form \"nnnn\". For example, @0666@
    -- represents the default access mode for all directories inside the file
    -- share. The default value is @0777@.
    directoryMode :: Core.Maybe Core.Text,
    -- | The Unix file mode in the form \"nnnn\". For example, @0666@ represents
    -- the default file mode inside the file share. The default value is
    -- @0666@.
    fileMode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NFSFileShareDefaults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'nFSFileShareDefaults_ownerId' - The default owner ID for files in the file share (unless the files have
-- another owner ID specified). The default value is @nfsnobody@.
--
-- 'groupId', 'nFSFileShareDefaults_groupId' - The default group ID for the file share (unless the files have another
-- group ID specified). The default value is @nfsnobody@.
--
-- 'directoryMode', 'nFSFileShareDefaults_directoryMode' - The Unix directory mode in the form \"nnnn\". For example, @0666@
-- represents the default access mode for all directories inside the file
-- share. The default value is @0777@.
--
-- 'fileMode', 'nFSFileShareDefaults_fileMode' - The Unix file mode in the form \"nnnn\". For example, @0666@ represents
-- the default file mode inside the file share. The default value is
-- @0666@.
newNFSFileShareDefaults ::
  NFSFileShareDefaults
newNFSFileShareDefaults =
  NFSFileShareDefaults'
    { ownerId = Core.Nothing,
      groupId = Core.Nothing,
      directoryMode = Core.Nothing,
      fileMode = Core.Nothing
    }

-- | The default owner ID for files in the file share (unless the files have
-- another owner ID specified). The default value is @nfsnobody@.
nFSFileShareDefaults_ownerId :: Lens.Lens' NFSFileShareDefaults (Core.Maybe Core.Natural)
nFSFileShareDefaults_ownerId = Lens.lens (\NFSFileShareDefaults' {ownerId} -> ownerId) (\s@NFSFileShareDefaults' {} a -> s {ownerId = a} :: NFSFileShareDefaults)

-- | The default group ID for the file share (unless the files have another
-- group ID specified). The default value is @nfsnobody@.
nFSFileShareDefaults_groupId :: Lens.Lens' NFSFileShareDefaults (Core.Maybe Core.Natural)
nFSFileShareDefaults_groupId = Lens.lens (\NFSFileShareDefaults' {groupId} -> groupId) (\s@NFSFileShareDefaults' {} a -> s {groupId = a} :: NFSFileShareDefaults)

-- | The Unix directory mode in the form \"nnnn\". For example, @0666@
-- represents the default access mode for all directories inside the file
-- share. The default value is @0777@.
nFSFileShareDefaults_directoryMode :: Lens.Lens' NFSFileShareDefaults (Core.Maybe Core.Text)
nFSFileShareDefaults_directoryMode = Lens.lens (\NFSFileShareDefaults' {directoryMode} -> directoryMode) (\s@NFSFileShareDefaults' {} a -> s {directoryMode = a} :: NFSFileShareDefaults)

-- | The Unix file mode in the form \"nnnn\". For example, @0666@ represents
-- the default file mode inside the file share. The default value is
-- @0666@.
nFSFileShareDefaults_fileMode :: Lens.Lens' NFSFileShareDefaults (Core.Maybe Core.Text)
nFSFileShareDefaults_fileMode = Lens.lens (\NFSFileShareDefaults' {fileMode} -> fileMode) (\s@NFSFileShareDefaults' {} a -> s {fileMode = a} :: NFSFileShareDefaults)

instance Core.FromJSON NFSFileShareDefaults where
  parseJSON =
    Core.withObject
      "NFSFileShareDefaults"
      ( \x ->
          NFSFileShareDefaults'
            Core.<$> (x Core..:? "OwnerId")
            Core.<*> (x Core..:? "GroupId")
            Core.<*> (x Core..:? "DirectoryMode")
            Core.<*> (x Core..:? "FileMode")
      )

instance Core.Hashable NFSFileShareDefaults

instance Core.NFData NFSFileShareDefaults

instance Core.ToJSON NFSFileShareDefaults where
  toJSON NFSFileShareDefaults' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OwnerId" Core..=) Core.<$> ownerId,
            ("GroupId" Core..=) Core.<$> groupId,
            ("DirectoryMode" Core..=) Core.<$> directoryMode,
            ("FileMode" Core..=) Core.<$> fileMode
          ]
      )
