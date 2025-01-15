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
-- Module      : Amazonka.Backup.CreateBackupVault
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a logical container where backups are stored. A
-- @CreateBackupVault@ request includes a name, optionally one or more
-- resource tags, an encryption key, and a request ID.
--
-- Do not include sensitive data, such as passport numbers, in the name of
-- a backup vault.
module Amazonka.Backup.CreateBackupVault
  ( -- * Creating a Request
    CreateBackupVault (..),
    newCreateBackupVault,

    -- * Request Lenses
    createBackupVault_backupVaultTags,
    createBackupVault_creatorRequestId,
    createBackupVault_encryptionKeyArn,
    createBackupVault_backupVaultName,

    -- * Destructuring the Response
    CreateBackupVaultResponse (..),
    newCreateBackupVaultResponse,

    -- * Response Lenses
    createBackupVaultResponse_backupVaultArn,
    createBackupVaultResponse_backupVaultName,
    createBackupVaultResponse_creationDate,
    createBackupVaultResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBackupVault' smart constructor.
data CreateBackupVault = CreateBackupVault'
  { -- | Metadata that you can assign to help organize the resources that you
    -- create. Each tag is a key-value pair.
    backupVaultTags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A unique string that identifies the request and allows failed requests
    -- to be retried without the risk of running the operation twice. This
    -- parameter is optional.
    --
    -- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
    -- characters.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The server-side encryption key that is used to protect your backups; for
    -- example,
    -- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackupVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultTags', 'createBackupVault_backupVaultTags' - Metadata that you can assign to help organize the resources that you
-- create. Each tag is a key-value pair.
--
-- 'creatorRequestId', 'createBackupVault_creatorRequestId' - A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice. This
-- parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
--
-- 'encryptionKeyArn', 'createBackupVault_encryptionKeyArn' - The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 'backupVaultName', 'createBackupVault_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of letters, numbers, and hyphens.
newCreateBackupVault ::
  -- | 'backupVaultName'
  Prelude.Text ->
  CreateBackupVault
newCreateBackupVault pBackupVaultName_ =
  CreateBackupVault'
    { backupVaultTags =
        Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      backupVaultName = pBackupVaultName_
    }

-- | Metadata that you can assign to help organize the resources that you
-- create. Each tag is a key-value pair.
createBackupVault_backupVaultTags :: Lens.Lens' CreateBackupVault (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBackupVault_backupVaultTags = Lens.lens (\CreateBackupVault' {backupVaultTags} -> backupVaultTags) (\s@CreateBackupVault' {} a -> s {backupVaultTags = a} :: CreateBackupVault) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice. This
-- parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
createBackupVault_creatorRequestId :: Lens.Lens' CreateBackupVault (Prelude.Maybe Prelude.Text)
createBackupVault_creatorRequestId = Lens.lens (\CreateBackupVault' {creatorRequestId} -> creatorRequestId) (\s@CreateBackupVault' {} a -> s {creatorRequestId = a} :: CreateBackupVault)

-- | The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
createBackupVault_encryptionKeyArn :: Lens.Lens' CreateBackupVault (Prelude.Maybe Prelude.Text)
createBackupVault_encryptionKeyArn = Lens.lens (\CreateBackupVault' {encryptionKeyArn} -> encryptionKeyArn) (\s@CreateBackupVault' {} a -> s {encryptionKeyArn = a} :: CreateBackupVault)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of letters, numbers, and hyphens.
createBackupVault_backupVaultName :: Lens.Lens' CreateBackupVault Prelude.Text
createBackupVault_backupVaultName = Lens.lens (\CreateBackupVault' {backupVaultName} -> backupVaultName) (\s@CreateBackupVault' {} a -> s {backupVaultName = a} :: CreateBackupVault)

instance Core.AWSRequest CreateBackupVault where
  type
    AWSResponse CreateBackupVault =
      CreateBackupVaultResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackupVaultResponse'
            Prelude.<$> (x Data..?> "BackupVaultArn")
            Prelude.<*> (x Data..?> "BackupVaultName")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackupVault where
  hashWithSalt _salt CreateBackupVault' {..} =
    _salt
      `Prelude.hashWithSalt` backupVaultTags
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` backupVaultName

instance Prelude.NFData CreateBackupVault where
  rnf CreateBackupVault' {..} =
    Prelude.rnf backupVaultTags `Prelude.seq`
      Prelude.rnf creatorRequestId `Prelude.seq`
        Prelude.rnf encryptionKeyArn `Prelude.seq`
          Prelude.rnf backupVaultName

instance Data.ToHeaders CreateBackupVault where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackupVault where
  toJSON CreateBackupVault' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackupVaultTags" Data..=)
              Prelude.<$> backupVaultTags,
            ("CreatorRequestId" Data..=)
              Prelude.<$> creatorRequestId,
            ("EncryptionKeyArn" Data..=)
              Prelude.<$> encryptionKeyArn
          ]
      )

instance Data.ToPath CreateBackupVault where
  toPath CreateBackupVault' {..} =
    Prelude.mconcat
      ["/backup-vaults/", Data.toBS backupVaultName]

instance Data.ToQuery CreateBackupVault where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackupVaultResponse' smart constructor.
data CreateBackupVaultResponse = CreateBackupVaultResponse'
  { -- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
    -- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Region where they are created. They consist of lowercase
    -- letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The date and time a backup vault is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackupVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultArn', 'createBackupVaultResponse_backupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'backupVaultName', 'createBackupVaultResponse_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
--
-- 'creationDate', 'createBackupVaultResponse_creationDate' - The date and time a backup vault is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'httpStatus', 'createBackupVaultResponse_httpStatus' - The response's http status code.
newCreateBackupVaultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackupVaultResponse
newCreateBackupVaultResponse pHttpStatus_ =
  CreateBackupVaultResponse'
    { backupVaultArn =
        Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
createBackupVaultResponse_backupVaultArn :: Lens.Lens' CreateBackupVaultResponse (Prelude.Maybe Prelude.Text)
createBackupVaultResponse_backupVaultArn = Lens.lens (\CreateBackupVaultResponse' {backupVaultArn} -> backupVaultArn) (\s@CreateBackupVaultResponse' {} a -> s {backupVaultArn = a} :: CreateBackupVaultResponse)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
createBackupVaultResponse_backupVaultName :: Lens.Lens' CreateBackupVaultResponse (Prelude.Maybe Prelude.Text)
createBackupVaultResponse_backupVaultName = Lens.lens (\CreateBackupVaultResponse' {backupVaultName} -> backupVaultName) (\s@CreateBackupVaultResponse' {} a -> s {backupVaultName = a} :: CreateBackupVaultResponse)

-- | The date and time a backup vault is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
createBackupVaultResponse_creationDate :: Lens.Lens' CreateBackupVaultResponse (Prelude.Maybe Prelude.UTCTime)
createBackupVaultResponse_creationDate = Lens.lens (\CreateBackupVaultResponse' {creationDate} -> creationDate) (\s@CreateBackupVaultResponse' {} a -> s {creationDate = a} :: CreateBackupVaultResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
createBackupVaultResponse_httpStatus :: Lens.Lens' CreateBackupVaultResponse Prelude.Int
createBackupVaultResponse_httpStatus = Lens.lens (\CreateBackupVaultResponse' {httpStatus} -> httpStatus) (\s@CreateBackupVaultResponse' {} a -> s {httpStatus = a} :: CreateBackupVaultResponse)

instance Prelude.NFData CreateBackupVaultResponse where
  rnf CreateBackupVaultResponse' {..} =
    Prelude.rnf backupVaultArn `Prelude.seq`
      Prelude.rnf backupVaultName `Prelude.seq`
        Prelude.rnf creationDate `Prelude.seq`
          Prelude.rnf httpStatus
