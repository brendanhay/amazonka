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
-- Module      : Amazonka.Backup.DisassociateRecoveryPointFromParent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action to a specific child (nested) recovery point removes the
-- relationship between the specified recovery point and its parent
-- (composite) recovery point.
module Amazonka.Backup.DisassociateRecoveryPointFromParent
  ( -- * Creating a Request
    DisassociateRecoveryPointFromParent (..),
    newDisassociateRecoveryPointFromParent,

    -- * Request Lenses
    disassociateRecoveryPointFromParent_backupVaultName,
    disassociateRecoveryPointFromParent_recoveryPointArn,

    -- * Destructuring the Response
    DisassociateRecoveryPointFromParentResponse (..),
    newDisassociateRecoveryPointFromParentResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateRecoveryPointFromParent' smart constructor.
data DisassociateRecoveryPointFromParent = DisassociateRecoveryPointFromParent'
  { -- | This is the name of a logical container where the child (nested)
    -- recovery point is stored. Backup vaults are identified by names that are
    -- unique to the account used to create them and the Amazon Web Services
    -- Region where they are created. They consist of lowercase letters,
    -- numbers, and hyphens.
    backupVaultName :: Prelude.Text,
    -- | This is the Amazon Resource Name (ARN) that uniquely identifies the
    -- child (nested) recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45.@
    recoveryPointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateRecoveryPointFromParent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'disassociateRecoveryPointFromParent_backupVaultName' - This is the name of a logical container where the child (nested)
-- recovery point is stored. Backup vaults are identified by names that are
-- unique to the account used to create them and the Amazon Web Services
-- Region where they are created. They consist of lowercase letters,
-- numbers, and hyphens.
--
-- 'recoveryPointArn', 'disassociateRecoveryPointFromParent_recoveryPointArn' - This is the Amazon Resource Name (ARN) that uniquely identifies the
-- child (nested) recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45.@
newDisassociateRecoveryPointFromParent ::
  -- | 'backupVaultName'
  Prelude.Text ->
  -- | 'recoveryPointArn'
  Prelude.Text ->
  DisassociateRecoveryPointFromParent
newDisassociateRecoveryPointFromParent
  pBackupVaultName_
  pRecoveryPointArn_ =
    DisassociateRecoveryPointFromParent'
      { backupVaultName =
          pBackupVaultName_,
        recoveryPointArn = pRecoveryPointArn_
      }

-- | This is the name of a logical container where the child (nested)
-- recovery point is stored. Backup vaults are identified by names that are
-- unique to the account used to create them and the Amazon Web Services
-- Region where they are created. They consist of lowercase letters,
-- numbers, and hyphens.
disassociateRecoveryPointFromParent_backupVaultName :: Lens.Lens' DisassociateRecoveryPointFromParent Prelude.Text
disassociateRecoveryPointFromParent_backupVaultName = Lens.lens (\DisassociateRecoveryPointFromParent' {backupVaultName} -> backupVaultName) (\s@DisassociateRecoveryPointFromParent' {} a -> s {backupVaultName = a} :: DisassociateRecoveryPointFromParent)

-- | This is the Amazon Resource Name (ARN) that uniquely identifies the
-- child (nested) recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45.@
disassociateRecoveryPointFromParent_recoveryPointArn :: Lens.Lens' DisassociateRecoveryPointFromParent Prelude.Text
disassociateRecoveryPointFromParent_recoveryPointArn = Lens.lens (\DisassociateRecoveryPointFromParent' {recoveryPointArn} -> recoveryPointArn) (\s@DisassociateRecoveryPointFromParent' {} a -> s {recoveryPointArn = a} :: DisassociateRecoveryPointFromParent)

instance
  Core.AWSRequest
    DisassociateRecoveryPointFromParent
  where
  type
    AWSResponse DisassociateRecoveryPointFromParent =
      DisassociateRecoveryPointFromParentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateRecoveryPointFromParentResponse'

instance
  Prelude.Hashable
    DisassociateRecoveryPointFromParent
  where
  hashWithSalt
    _salt
    DisassociateRecoveryPointFromParent' {..} =
      _salt `Prelude.hashWithSalt` backupVaultName
        `Prelude.hashWithSalt` recoveryPointArn

instance
  Prelude.NFData
    DisassociateRecoveryPointFromParent
  where
  rnf DisassociateRecoveryPointFromParent' {..} =
    Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf recoveryPointArn

instance
  Data.ToHeaders
    DisassociateRecoveryPointFromParent
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DisassociateRecoveryPointFromParent
  where
  toPath DisassociateRecoveryPointFromParent' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/recovery-points/",
        Data.toBS recoveryPointArn,
        "/parentAssociation"
      ]

instance
  Data.ToQuery
    DisassociateRecoveryPointFromParent
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateRecoveryPointFromParentResponse' smart constructor.
data DisassociateRecoveryPointFromParentResponse = DisassociateRecoveryPointFromParentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateRecoveryPointFromParentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateRecoveryPointFromParentResponse ::
  DisassociateRecoveryPointFromParentResponse
newDisassociateRecoveryPointFromParentResponse =
  DisassociateRecoveryPointFromParentResponse'

instance
  Prelude.NFData
    DisassociateRecoveryPointFromParentResponse
  where
  rnf _ = ()
