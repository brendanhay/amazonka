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
-- Module      : Amazonka.StorageGateway.Types.NFSFileShareDefaults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.NFSFileShareDefaults where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes Network File System (NFS) file share default values. Files and
-- folders stored as Amazon S3 objects in S3 buckets don\'t, by default,
-- have Unix file permissions assigned to them. Upon discovery in an S3
-- bucket by Storage Gateway, the S3 objects that represent files and
-- folders are assigned these default Unix permissions. This operation is
-- only supported for S3 File Gateways.
--
-- /See:/ 'newNFSFileShareDefaults' smart constructor.
data NFSFileShareDefaults = NFSFileShareDefaults'
  { -- | The Unix directory mode in the form \"nnnn\". For example, @0666@
    -- represents the default access mode for all directories inside the file
    -- share. The default value is @0777@.
    directoryMode :: Prelude.Maybe Prelude.Text,
    -- | The Unix file mode in the form \"nnnn\". For example, @0666@ represents
    -- the default file mode inside the file share. The default value is
    -- @0666@.
    fileMode :: Prelude.Maybe Prelude.Text,
    -- | The default group ID for the file share (unless the files have another
    -- group ID specified). The default value is @nfsnobody@.
    groupId :: Prelude.Maybe Prelude.Natural,
    -- | The default owner ID for files in the file share (unless the files have
    -- another owner ID specified). The default value is @nfsnobody@.
    ownerId :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NFSFileShareDefaults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryMode', 'nFSFileShareDefaults_directoryMode' - The Unix directory mode in the form \"nnnn\". For example, @0666@
-- represents the default access mode for all directories inside the file
-- share. The default value is @0777@.
--
-- 'fileMode', 'nFSFileShareDefaults_fileMode' - The Unix file mode in the form \"nnnn\". For example, @0666@ represents
-- the default file mode inside the file share. The default value is
-- @0666@.
--
-- 'groupId', 'nFSFileShareDefaults_groupId' - The default group ID for the file share (unless the files have another
-- group ID specified). The default value is @nfsnobody@.
--
-- 'ownerId', 'nFSFileShareDefaults_ownerId' - The default owner ID for files in the file share (unless the files have
-- another owner ID specified). The default value is @nfsnobody@.
newNFSFileShareDefaults ::
  NFSFileShareDefaults
newNFSFileShareDefaults =
  NFSFileShareDefaults'
    { directoryMode =
        Prelude.Nothing,
      fileMode = Prelude.Nothing,
      groupId = Prelude.Nothing,
      ownerId = Prelude.Nothing
    }

-- | The Unix directory mode in the form \"nnnn\". For example, @0666@
-- represents the default access mode for all directories inside the file
-- share. The default value is @0777@.
nFSFileShareDefaults_directoryMode :: Lens.Lens' NFSFileShareDefaults (Prelude.Maybe Prelude.Text)
nFSFileShareDefaults_directoryMode = Lens.lens (\NFSFileShareDefaults' {directoryMode} -> directoryMode) (\s@NFSFileShareDefaults' {} a -> s {directoryMode = a} :: NFSFileShareDefaults)

-- | The Unix file mode in the form \"nnnn\". For example, @0666@ represents
-- the default file mode inside the file share. The default value is
-- @0666@.
nFSFileShareDefaults_fileMode :: Lens.Lens' NFSFileShareDefaults (Prelude.Maybe Prelude.Text)
nFSFileShareDefaults_fileMode = Lens.lens (\NFSFileShareDefaults' {fileMode} -> fileMode) (\s@NFSFileShareDefaults' {} a -> s {fileMode = a} :: NFSFileShareDefaults)

-- | The default group ID for the file share (unless the files have another
-- group ID specified). The default value is @nfsnobody@.
nFSFileShareDefaults_groupId :: Lens.Lens' NFSFileShareDefaults (Prelude.Maybe Prelude.Natural)
nFSFileShareDefaults_groupId = Lens.lens (\NFSFileShareDefaults' {groupId} -> groupId) (\s@NFSFileShareDefaults' {} a -> s {groupId = a} :: NFSFileShareDefaults)

-- | The default owner ID for files in the file share (unless the files have
-- another owner ID specified). The default value is @nfsnobody@.
nFSFileShareDefaults_ownerId :: Lens.Lens' NFSFileShareDefaults (Prelude.Maybe Prelude.Natural)
nFSFileShareDefaults_ownerId = Lens.lens (\NFSFileShareDefaults' {ownerId} -> ownerId) (\s@NFSFileShareDefaults' {} a -> s {ownerId = a} :: NFSFileShareDefaults)

instance Data.FromJSON NFSFileShareDefaults where
  parseJSON =
    Data.withObject
      "NFSFileShareDefaults"
      ( \x ->
          NFSFileShareDefaults'
            Prelude.<$> (x Data..:? "DirectoryMode")
            Prelude.<*> (x Data..:? "FileMode")
            Prelude.<*> (x Data..:? "GroupId")
            Prelude.<*> (x Data..:? "OwnerId")
      )

instance Prelude.Hashable NFSFileShareDefaults where
  hashWithSalt _salt NFSFileShareDefaults' {..} =
    _salt
      `Prelude.hashWithSalt` directoryMode
      `Prelude.hashWithSalt` fileMode
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` ownerId

instance Prelude.NFData NFSFileShareDefaults where
  rnf NFSFileShareDefaults' {..} =
    Prelude.rnf directoryMode
      `Prelude.seq` Prelude.rnf fileMode
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf ownerId

instance Data.ToJSON NFSFileShareDefaults where
  toJSON NFSFileShareDefaults' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DirectoryMode" Data..=) Prelude.<$> directoryMode,
            ("FileMode" Data..=) Prelude.<$> fileMode,
            ("GroupId" Data..=) Prelude.<$> groupId,
            ("OwnerId" Data..=) Prelude.<$> ownerId
          ]
      )
