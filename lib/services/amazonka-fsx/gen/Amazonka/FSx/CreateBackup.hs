{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FSx.CreateBackup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a backup of an existing Amazon FSx for Windows File Server file
-- system, Amazon FSx for Lustre file system, Amazon FSx for NetApp ONTAP
-- volume, or Amazon FSx for OpenZFS file system. We recommend creating
-- regular backups so that you can restore a file system or volume from a
-- backup if an issue arises with the original file system or volume.
--
-- For Amazon FSx for Lustre file systems, you can create a backup only for
-- file systems that have the following configuration:
--
-- -   A Persistent deployment type
--
-- -   Are /not/ linked to a data repository
--
-- For more information about backups, see the following:
--
-- -   For Amazon FSx for Lustre, see
--     <https://docs.aws.amazon.com/fsx/latest/LustreGuide/using-backups-fsx.html Working with FSx for Lustre backups>.
--
-- -   For Amazon FSx for Windows, see
--     <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/using-backups.html Working with FSx for Windows backups>.
--
-- -   For Amazon FSx for NetApp ONTAP, see
--     <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/using-backups.html Working with FSx for NetApp ONTAP backups>.
--
-- -   For Amazon FSx for OpenZFS, see
--     <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/using-backups.html Working with FSx for OpenZFS backups>.
--
-- If a backup with the specified client request token exists and the
-- parameters match, this operation returns the description of the existing
-- backup. If a backup with the specified client request token exists and
-- the parameters don\'t match, this operation returns
-- @IncompatibleParameterError@. If a backup with the specified client
-- request token doesn\'t exist, @CreateBackup@ does the following:
--
-- -   Creates a new Amazon FSx backup with an assigned ID, and an initial
--     lifecycle state of @CREATING@.
--
-- -   Returns the description of the backup.
--
-- By using the idempotent operation, you can retry a @CreateBackup@
-- operation without the risk of creating an extra backup. This approach
-- can be useful when an initial call fails in a way that makes it unclear
-- whether a backup was created. If you use the same client request token
-- and the initial call created a backup, the operation returns a
-- successful result because all the parameters are the same.
--
-- The @CreateBackup@ operation returns while the backup\'s lifecycle state
-- is still @CREATING@. You can check the backup creation status by calling
-- the
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_DescribeBackups.html DescribeBackups>
-- operation, which returns the backup state along with other information.
module Amazonka.FSx.CreateBackup
  ( -- * Creating a Request
    CreateBackup (..),
    newCreateBackup,

    -- * Request Lenses
    createBackup_clientRequestToken,
    createBackup_fileSystemId,
    createBackup_tags,
    createBackup_volumeId,

    -- * Destructuring the Response
    CreateBackupResponse (..),
    newCreateBackupResponse,

    -- * Response Lenses
    createBackupResponse_backup,
    createBackupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object for the @CreateBackup@ operation.
--
-- /See:/ 'newCreateBackup' smart constructor.
data CreateBackup = CreateBackup'
  { -- | (Optional) A string of up to 64 ASCII characters that Amazon FSx uses to
    -- ensure idempotent creation. This string is automatically filled on your
    -- behalf when you use the Command Line Interface (CLI) or an Amazon Web
    -- Services SDK.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the file system to back up.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The tags to apply to the backup at backup creation. The key
    -- value of the @Name@ tag appears in the console as the backup name. If
    -- you have set @CopyTagsToBackups@ to @true@, and you specify one or more
    -- tags using the @CreateBackup@ operation, no existing file system tags
    -- are copied from the file system to the backup.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | (Optional) The ID of the FSx for ONTAP volume to back up.
    volumeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createBackup_clientRequestToken' - (Optional) A string of up to 64 ASCII characters that Amazon FSx uses to
-- ensure idempotent creation. This string is automatically filled on your
-- behalf when you use the Command Line Interface (CLI) or an Amazon Web
-- Services SDK.
--
-- 'fileSystemId', 'createBackup_fileSystemId' - The ID of the file system to back up.
--
-- 'tags', 'createBackup_tags' - (Optional) The tags to apply to the backup at backup creation. The key
-- value of the @Name@ tag appears in the console as the backup name. If
-- you have set @CopyTagsToBackups@ to @true@, and you specify one or more
-- tags using the @CreateBackup@ operation, no existing file system tags
-- are copied from the file system to the backup.
--
-- 'volumeId', 'createBackup_volumeId' - (Optional) The ID of the FSx for ONTAP volume to back up.
newCreateBackup ::
  CreateBackup
newCreateBackup =
  CreateBackup'
    { clientRequestToken = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      tags = Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | (Optional) A string of up to 64 ASCII characters that Amazon FSx uses to
-- ensure idempotent creation. This string is automatically filled on your
-- behalf when you use the Command Line Interface (CLI) or an Amazon Web
-- Services SDK.
createBackup_clientRequestToken :: Lens.Lens' CreateBackup (Prelude.Maybe Prelude.Text)
createBackup_clientRequestToken = Lens.lens (\CreateBackup' {clientRequestToken} -> clientRequestToken) (\s@CreateBackup' {} a -> s {clientRequestToken = a} :: CreateBackup)

-- | The ID of the file system to back up.
createBackup_fileSystemId :: Lens.Lens' CreateBackup (Prelude.Maybe Prelude.Text)
createBackup_fileSystemId = Lens.lens (\CreateBackup' {fileSystemId} -> fileSystemId) (\s@CreateBackup' {} a -> s {fileSystemId = a} :: CreateBackup)

-- | (Optional) The tags to apply to the backup at backup creation. The key
-- value of the @Name@ tag appears in the console as the backup name. If
-- you have set @CopyTagsToBackups@ to @true@, and you specify one or more
-- tags using the @CreateBackup@ operation, no existing file system tags
-- are copied from the file system to the backup.
createBackup_tags :: Lens.Lens' CreateBackup (Prelude.Maybe (Prelude.NonEmpty Tag))
createBackup_tags = Lens.lens (\CreateBackup' {tags} -> tags) (\s@CreateBackup' {} a -> s {tags = a} :: CreateBackup) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) The ID of the FSx for ONTAP volume to back up.
createBackup_volumeId :: Lens.Lens' CreateBackup (Prelude.Maybe Prelude.Text)
createBackup_volumeId = Lens.lens (\CreateBackup' {volumeId} -> volumeId) (\s@CreateBackup' {} a -> s {volumeId = a} :: CreateBackup)

instance Core.AWSRequest CreateBackup where
  type AWSResponse CreateBackup = CreateBackupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackupResponse'
            Prelude.<$> (x Data..?> "Backup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackup where
  hashWithSalt _salt CreateBackup' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData CreateBackup where
  rnf CreateBackup' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volumeId

instance Data.ToHeaders CreateBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CreateBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackup where
  toJSON CreateBackup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("FileSystemId" Data..=) Prelude.<$> fileSystemId,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VolumeId" Data..=) Prelude.<$> volumeId
          ]
      )

instance Data.ToPath CreateBackup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateBackup where
  toQuery = Prelude.const Prelude.mempty

-- | The response object for the @CreateBackup@ operation.
--
-- /See:/ 'newCreateBackupResponse' smart constructor.
data CreateBackupResponse = CreateBackupResponse'
  { -- | A description of the backup.
    backup :: Prelude.Maybe Backup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backup', 'createBackupResponse_backup' - A description of the backup.
--
-- 'httpStatus', 'createBackupResponse_httpStatus' - The response's http status code.
newCreateBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackupResponse
newCreateBackupResponse pHttpStatus_ =
  CreateBackupResponse'
    { backup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the backup.
createBackupResponse_backup :: Lens.Lens' CreateBackupResponse (Prelude.Maybe Backup)
createBackupResponse_backup = Lens.lens (\CreateBackupResponse' {backup} -> backup) (\s@CreateBackupResponse' {} a -> s {backup = a} :: CreateBackupResponse)

-- | The response's http status code.
createBackupResponse_httpStatus :: Lens.Lens' CreateBackupResponse Prelude.Int
createBackupResponse_httpStatus = Lens.lens (\CreateBackupResponse' {httpStatus} -> httpStatus) (\s@CreateBackupResponse' {} a -> s {httpStatus = a} :: CreateBackupResponse)

instance Prelude.NFData CreateBackupResponse where
  rnf CreateBackupResponse' {..} =
    Prelude.rnf backup
      `Prelude.seq` Prelude.rnf httpStatus
