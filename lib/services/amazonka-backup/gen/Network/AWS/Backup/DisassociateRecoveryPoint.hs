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
-- Module      : Network.AWS.Backup.DisassociateRecoveryPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified continuous backup recovery point from Backup and
-- releases control of that continuous backup to the source service, such
-- as Amazon RDS. The source service will continue to create and retain
-- continuous backups using the lifecycle that you specified in your
-- original backup plan.
--
-- Does not support snapshot backup recovery points.
module Network.AWS.Backup.DisassociateRecoveryPoint
  ( -- * Creating a Request
    DisassociateRecoveryPoint (..),
    newDisassociateRecoveryPoint,

    -- * Request Lenses
    disassociateRecoveryPoint_backupVaultName,
    disassociateRecoveryPoint_recoveryPointArn,

    -- * Destructuring the Response
    DisassociateRecoveryPointResponse (..),
    newDisassociateRecoveryPointResponse,
  )
where

import Network.AWS.Backup.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateRecoveryPoint' smart constructor.
data DisassociateRecoveryPoint = DisassociateRecoveryPoint'
  { -- | The unique name of an Backup vault.
    backupVaultName :: Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies an Backup
    -- recovery point.
    recoveryPointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateRecoveryPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'disassociateRecoveryPoint_backupVaultName' - The unique name of an Backup vault.
--
-- 'recoveryPointArn', 'disassociateRecoveryPoint_recoveryPointArn' - An Amazon Resource Name (ARN) that uniquely identifies an Backup
-- recovery point.
newDisassociateRecoveryPoint ::
  -- | 'backupVaultName'
  Prelude.Text ->
  -- | 'recoveryPointArn'
  Prelude.Text ->
  DisassociateRecoveryPoint
newDisassociateRecoveryPoint
  pBackupVaultName_
  pRecoveryPointArn_ =
    DisassociateRecoveryPoint'
      { backupVaultName =
          pBackupVaultName_,
        recoveryPointArn = pRecoveryPointArn_
      }

-- | The unique name of an Backup vault.
disassociateRecoveryPoint_backupVaultName :: Lens.Lens' DisassociateRecoveryPoint Prelude.Text
disassociateRecoveryPoint_backupVaultName = Lens.lens (\DisassociateRecoveryPoint' {backupVaultName} -> backupVaultName) (\s@DisassociateRecoveryPoint' {} a -> s {backupVaultName = a} :: DisassociateRecoveryPoint)

-- | An Amazon Resource Name (ARN) that uniquely identifies an Backup
-- recovery point.
disassociateRecoveryPoint_recoveryPointArn :: Lens.Lens' DisassociateRecoveryPoint Prelude.Text
disassociateRecoveryPoint_recoveryPointArn = Lens.lens (\DisassociateRecoveryPoint' {recoveryPointArn} -> recoveryPointArn) (\s@DisassociateRecoveryPoint' {} a -> s {recoveryPointArn = a} :: DisassociateRecoveryPoint)

instance Core.AWSRequest DisassociateRecoveryPoint where
  type
    AWSResponse DisassociateRecoveryPoint =
      DisassociateRecoveryPointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DisassociateRecoveryPointResponse'

instance Prelude.Hashable DisassociateRecoveryPoint

instance Prelude.NFData DisassociateRecoveryPoint

instance Core.ToHeaders DisassociateRecoveryPoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateRecoveryPoint where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DisassociateRecoveryPoint where
  toPath DisassociateRecoveryPoint' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Core.toBS backupVaultName,
        "/recovery-points/",
        Core.toBS recoveryPointArn,
        "/disassociate"
      ]

instance Core.ToQuery DisassociateRecoveryPoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateRecoveryPointResponse' smart constructor.
data DisassociateRecoveryPointResponse = DisassociateRecoveryPointResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateRecoveryPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateRecoveryPointResponse ::
  DisassociateRecoveryPointResponse
newDisassociateRecoveryPointResponse =
  DisassociateRecoveryPointResponse'

instance
  Prelude.NFData
    DisassociateRecoveryPointResponse
