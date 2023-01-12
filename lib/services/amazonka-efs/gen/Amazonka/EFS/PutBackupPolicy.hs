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
-- Module      : Amazonka.EFS.PutBackupPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the file system\'s backup policy. Use this action to start or
-- stop automatic backups of the file system.
module Amazonka.EFS.PutBackupPolicy
  ( -- * Creating a Request
    PutBackupPolicy (..),
    newPutBackupPolicy,

    -- * Request Lenses
    putBackupPolicy_fileSystemId,
    putBackupPolicy_backupPolicy,

    -- * Destructuring the Response
    BackupPolicyDescription (..),
    newBackupPolicyDescription,

    -- * Response Lenses
    backupPolicyDescription_backupPolicy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutBackupPolicy' smart constructor.
data PutBackupPolicy = PutBackupPolicy'
  { -- | Specifies which EFS file system to update the backup policy for.
    fileSystemId :: Prelude.Text,
    -- | The backup policy included in the @PutBackupPolicy@ request.
    backupPolicy :: BackupPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBackupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'putBackupPolicy_fileSystemId' - Specifies which EFS file system to update the backup policy for.
--
-- 'backupPolicy', 'putBackupPolicy_backupPolicy' - The backup policy included in the @PutBackupPolicy@ request.
newPutBackupPolicy ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'backupPolicy'
  BackupPolicy ->
  PutBackupPolicy
newPutBackupPolicy pFileSystemId_ pBackupPolicy_ =
  PutBackupPolicy'
    { fileSystemId = pFileSystemId_,
      backupPolicy = pBackupPolicy_
    }

-- | Specifies which EFS file system to update the backup policy for.
putBackupPolicy_fileSystemId :: Lens.Lens' PutBackupPolicy Prelude.Text
putBackupPolicy_fileSystemId = Lens.lens (\PutBackupPolicy' {fileSystemId} -> fileSystemId) (\s@PutBackupPolicy' {} a -> s {fileSystemId = a} :: PutBackupPolicy)

-- | The backup policy included in the @PutBackupPolicy@ request.
putBackupPolicy_backupPolicy :: Lens.Lens' PutBackupPolicy BackupPolicy
putBackupPolicy_backupPolicy = Lens.lens (\PutBackupPolicy' {backupPolicy} -> backupPolicy) (\s@PutBackupPolicy' {} a -> s {backupPolicy = a} :: PutBackupPolicy)

instance Core.AWSRequest PutBackupPolicy where
  type
    AWSResponse PutBackupPolicy =
      BackupPolicyDescription
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable PutBackupPolicy where
  hashWithSalt _salt PutBackupPolicy' {..} =
    _salt `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` backupPolicy

instance Prelude.NFData PutBackupPolicy where
  rnf PutBackupPolicy' {..} =
    Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf backupPolicy

instance Data.ToHeaders PutBackupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutBackupPolicy where
  toJSON PutBackupPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BackupPolicy" Data..= backupPolicy)]
      )

instance Data.ToPath PutBackupPolicy where
  toPath PutBackupPolicy' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Data.toBS fileSystemId,
        "/backup-policy"
      ]

instance Data.ToQuery PutBackupPolicy where
  toQuery = Prelude.const Prelude.mempty
