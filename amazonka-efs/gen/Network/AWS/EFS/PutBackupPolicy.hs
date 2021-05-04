{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EFS.PutBackupPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the file system\'s backup policy. Use this action to start or
-- stop automatic backups of the file system.
module Network.AWS.EFS.PutBackupPolicy
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

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutBackupPolicy' smart constructor.
data PutBackupPolicy = PutBackupPolicy'
  { -- | Specifies which EFS file system to update the backup policy for.
    fileSystemId :: Prelude.Text,
    -- | The backup policy included in the @PutBackupPolicy@ request.
    backupPolicy :: BackupPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest PutBackupPolicy where
  type Rs PutBackupPolicy = BackupPolicyDescription
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable PutBackupPolicy

instance Prelude.NFData PutBackupPolicy

instance Prelude.ToHeaders PutBackupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON PutBackupPolicy where
  toJSON PutBackupPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("BackupPolicy" Prelude..= backupPolicy)
          ]
      )

instance Prelude.ToPath PutBackupPolicy where
  toPath PutBackupPolicy' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Prelude.toBS fileSystemId,
        "/backup-policy"
      ]

instance Prelude.ToQuery PutBackupPolicy where
  toQuery = Prelude.const Prelude.mempty
