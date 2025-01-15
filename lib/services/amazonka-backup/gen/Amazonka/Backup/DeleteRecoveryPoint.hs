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
-- Module      : Amazonka.Backup.DeleteRecoveryPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the recovery point specified by a recovery point ID.
--
-- If the recovery point ID belongs to a continuous backup, calling this
-- endpoint deletes the existing continuous backup and stops future
-- continuous backup.
--
-- When an IAM role\'s permissions are insufficient to call this API, the
-- service sends back an HTTP 200 response with an empty HTTP body, but the
-- recovery point is not deleted. Instead, it enters an @EXPIRED@ state.
--
-- @EXPIRED@ recovery points can be deleted with this API once the IAM role
-- has the @iam:CreateServiceLinkedRole@ action. To learn more about adding
-- this role, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/deleting-backups.html#deleting-backups-troubleshooting Troubleshooting manual deletions>.
--
-- If the user or role is deleted or the permission within the role is
-- removed, the deletion will not be successful and will enter an @EXPIRED@
-- state.
module Amazonka.Backup.DeleteRecoveryPoint
  ( -- * Creating a Request
    DeleteRecoveryPoint (..),
    newDeleteRecoveryPoint,

    -- * Request Lenses
    deleteRecoveryPoint_backupVaultName,
    deleteRecoveryPoint_recoveryPointArn,

    -- * Destructuring the Response
    DeleteRecoveryPointResponse (..),
    newDeleteRecoveryPointResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRecoveryPoint' smart constructor.
data DeleteRecoveryPoint = DeleteRecoveryPoint'
  { -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecoveryPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'deleteRecoveryPoint_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'recoveryPointArn', 'deleteRecoveryPoint_recoveryPointArn' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
newDeleteRecoveryPoint ::
  -- | 'backupVaultName'
  Prelude.Text ->
  -- | 'recoveryPointArn'
  Prelude.Text ->
  DeleteRecoveryPoint
newDeleteRecoveryPoint
  pBackupVaultName_
  pRecoveryPointArn_ =
    DeleteRecoveryPoint'
      { backupVaultName =
          pBackupVaultName_,
        recoveryPointArn = pRecoveryPointArn_
      }

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
deleteRecoveryPoint_backupVaultName :: Lens.Lens' DeleteRecoveryPoint Prelude.Text
deleteRecoveryPoint_backupVaultName = Lens.lens (\DeleteRecoveryPoint' {backupVaultName} -> backupVaultName) (\s@DeleteRecoveryPoint' {} a -> s {backupVaultName = a} :: DeleteRecoveryPoint)

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
deleteRecoveryPoint_recoveryPointArn :: Lens.Lens' DeleteRecoveryPoint Prelude.Text
deleteRecoveryPoint_recoveryPointArn = Lens.lens (\DeleteRecoveryPoint' {recoveryPointArn} -> recoveryPointArn) (\s@DeleteRecoveryPoint' {} a -> s {recoveryPointArn = a} :: DeleteRecoveryPoint)

instance Core.AWSRequest DeleteRecoveryPoint where
  type
    AWSResponse DeleteRecoveryPoint =
      DeleteRecoveryPointResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteRecoveryPointResponse'

instance Prelude.Hashable DeleteRecoveryPoint where
  hashWithSalt _salt DeleteRecoveryPoint' {..} =
    _salt
      `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` recoveryPointArn

instance Prelude.NFData DeleteRecoveryPoint where
  rnf DeleteRecoveryPoint' {..} =
    Prelude.rnf backupVaultName `Prelude.seq`
      Prelude.rnf recoveryPointArn

instance Data.ToHeaders DeleteRecoveryPoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRecoveryPoint where
  toPath DeleteRecoveryPoint' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/recovery-points/",
        Data.toBS recoveryPointArn
      ]

instance Data.ToQuery DeleteRecoveryPoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRecoveryPointResponse' smart constructor.
data DeleteRecoveryPointResponse = DeleteRecoveryPointResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecoveryPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRecoveryPointResponse ::
  DeleteRecoveryPointResponse
newDeleteRecoveryPointResponse =
  DeleteRecoveryPointResponse'

instance Prelude.NFData DeleteRecoveryPointResponse where
  rnf _ = ()
