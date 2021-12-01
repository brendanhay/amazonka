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
-- Module      : Amazonka.Backup.ListRecoveryPointsByBackupVault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about the recovery points stored in a
-- backup vault.
module Amazonka.Backup.ListRecoveryPointsByBackupVault
  ( -- * Creating a Request
    ListRecoveryPointsByBackupVault (..),
    newListRecoveryPointsByBackupVault,

    -- * Request Lenses
    listRecoveryPointsByBackupVault_byResourceArn,
    listRecoveryPointsByBackupVault_byCreatedAfter,
    listRecoveryPointsByBackupVault_byCreatedBefore,
    listRecoveryPointsByBackupVault_byBackupPlanId,
    listRecoveryPointsByBackupVault_byResourceType,
    listRecoveryPointsByBackupVault_nextToken,
    listRecoveryPointsByBackupVault_maxResults,
    listRecoveryPointsByBackupVault_backupVaultName,

    -- * Destructuring the Response
    ListRecoveryPointsByBackupVaultResponse (..),
    newListRecoveryPointsByBackupVaultResponse,

    -- * Response Lenses
    listRecoveryPointsByBackupVaultResponse_recoveryPoints,
    listRecoveryPointsByBackupVaultResponse_nextToken,
    listRecoveryPointsByBackupVaultResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecoveryPointsByBackupVault' smart constructor.
data ListRecoveryPointsByBackupVault = ListRecoveryPointsByBackupVault'
  { -- | Returns only recovery points that match the specified resource Amazon
    -- Resource Name (ARN).
    byResourceArn :: Prelude.Maybe Prelude.Text,
    -- | Returns only recovery points that were created after the specified
    -- timestamp.
    byCreatedAfter :: Prelude.Maybe Core.POSIX,
    -- | Returns only recovery points that were created before the specified
    -- timestamp.
    byCreatedBefore :: Prelude.Maybe Core.POSIX,
    -- | Returns only recovery points that match the specified backup plan ID.
    byBackupPlanId :: Prelude.Maybe Prelude.Text,
    -- | Returns only recovery points that match the specified resource type.
    byResourceType :: Prelude.Maybe Prelude.Text,
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    --
    -- Backup vault name might not be available when a supported service
    -- creates the backup.
    backupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecoveryPointsByBackupVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byResourceArn', 'listRecoveryPointsByBackupVault_byResourceArn' - Returns only recovery points that match the specified resource Amazon
-- Resource Name (ARN).
--
-- 'byCreatedAfter', 'listRecoveryPointsByBackupVault_byCreatedAfter' - Returns only recovery points that were created after the specified
-- timestamp.
--
-- 'byCreatedBefore', 'listRecoveryPointsByBackupVault_byCreatedBefore' - Returns only recovery points that were created before the specified
-- timestamp.
--
-- 'byBackupPlanId', 'listRecoveryPointsByBackupVault_byBackupPlanId' - Returns only recovery points that match the specified backup plan ID.
--
-- 'byResourceType', 'listRecoveryPointsByBackupVault_byResourceType' - Returns only recovery points that match the specified resource type.
--
-- 'nextToken', 'listRecoveryPointsByBackupVault_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'maxResults', 'listRecoveryPointsByBackupVault_maxResults' - The maximum number of items to be returned.
--
-- 'backupVaultName', 'listRecoveryPointsByBackupVault_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- Backup vault name might not be available when a supported service
-- creates the backup.
newListRecoveryPointsByBackupVault ::
  -- | 'backupVaultName'
  Prelude.Text ->
  ListRecoveryPointsByBackupVault
newListRecoveryPointsByBackupVault pBackupVaultName_ =
  ListRecoveryPointsByBackupVault'
    { byResourceArn =
        Prelude.Nothing,
      byCreatedAfter = Prelude.Nothing,
      byCreatedBefore = Prelude.Nothing,
      byBackupPlanId = Prelude.Nothing,
      byResourceType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      backupVaultName = pBackupVaultName_
    }

-- | Returns only recovery points that match the specified resource Amazon
-- Resource Name (ARN).
listRecoveryPointsByBackupVault_byResourceArn :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVault_byResourceArn = Lens.lens (\ListRecoveryPointsByBackupVault' {byResourceArn} -> byResourceArn) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byResourceArn = a} :: ListRecoveryPointsByBackupVault)

-- | Returns only recovery points that were created after the specified
-- timestamp.
listRecoveryPointsByBackupVault_byCreatedAfter :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.UTCTime)
listRecoveryPointsByBackupVault_byCreatedAfter = Lens.lens (\ListRecoveryPointsByBackupVault' {byCreatedAfter} -> byCreatedAfter) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byCreatedAfter = a} :: ListRecoveryPointsByBackupVault) Prelude.. Lens.mapping Core._Time

-- | Returns only recovery points that were created before the specified
-- timestamp.
listRecoveryPointsByBackupVault_byCreatedBefore :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.UTCTime)
listRecoveryPointsByBackupVault_byCreatedBefore = Lens.lens (\ListRecoveryPointsByBackupVault' {byCreatedBefore} -> byCreatedBefore) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byCreatedBefore = a} :: ListRecoveryPointsByBackupVault) Prelude.. Lens.mapping Core._Time

-- | Returns only recovery points that match the specified backup plan ID.
listRecoveryPointsByBackupVault_byBackupPlanId :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVault_byBackupPlanId = Lens.lens (\ListRecoveryPointsByBackupVault' {byBackupPlanId} -> byBackupPlanId) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byBackupPlanId = a} :: ListRecoveryPointsByBackupVault)

-- | Returns only recovery points that match the specified resource type.
listRecoveryPointsByBackupVault_byResourceType :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVault_byResourceType = Lens.lens (\ListRecoveryPointsByBackupVault' {byResourceType} -> byResourceType) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byResourceType = a} :: ListRecoveryPointsByBackupVault)

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listRecoveryPointsByBackupVault_nextToken :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVault_nextToken = Lens.lens (\ListRecoveryPointsByBackupVault' {nextToken} -> nextToken) (\s@ListRecoveryPointsByBackupVault' {} a -> s {nextToken = a} :: ListRecoveryPointsByBackupVault)

-- | The maximum number of items to be returned.
listRecoveryPointsByBackupVault_maxResults :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Natural)
listRecoveryPointsByBackupVault_maxResults = Lens.lens (\ListRecoveryPointsByBackupVault' {maxResults} -> maxResults) (\s@ListRecoveryPointsByBackupVault' {} a -> s {maxResults = a} :: ListRecoveryPointsByBackupVault)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- Backup vault name might not be available when a supported service
-- creates the backup.
listRecoveryPointsByBackupVault_backupVaultName :: Lens.Lens' ListRecoveryPointsByBackupVault Prelude.Text
listRecoveryPointsByBackupVault_backupVaultName = Lens.lens (\ListRecoveryPointsByBackupVault' {backupVaultName} -> backupVaultName) (\s@ListRecoveryPointsByBackupVault' {} a -> s {backupVaultName = a} :: ListRecoveryPointsByBackupVault)

instance
  Core.AWSRequest
    ListRecoveryPointsByBackupVault
  where
  type
    AWSResponse ListRecoveryPointsByBackupVault =
      ListRecoveryPointsByBackupVaultResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecoveryPointsByBackupVaultResponse'
            Prelude.<$> (x Core..?> "RecoveryPoints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListRecoveryPointsByBackupVault
  where
  hashWithSalt
    salt'
    ListRecoveryPointsByBackupVault' {..} =
      salt' `Prelude.hashWithSalt` backupVaultName
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` byResourceType
        `Prelude.hashWithSalt` byBackupPlanId
        `Prelude.hashWithSalt` byCreatedBefore
        `Prelude.hashWithSalt` byCreatedAfter
        `Prelude.hashWithSalt` byResourceArn

instance
  Prelude.NFData
    ListRecoveryPointsByBackupVault
  where
  rnf ListRecoveryPointsByBackupVault' {..} =
    Prelude.rnf byResourceArn
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf byResourceType
      `Prelude.seq` Prelude.rnf byBackupPlanId
      `Prelude.seq` Prelude.rnf byCreatedBefore
      `Prelude.seq` Prelude.rnf byCreatedAfter

instance
  Core.ToHeaders
    ListRecoveryPointsByBackupVault
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListRecoveryPointsByBackupVault where
  toPath ListRecoveryPointsByBackupVault' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Core.toBS backupVaultName,
        "/recovery-points/"
      ]

instance Core.ToQuery ListRecoveryPointsByBackupVault where
  toQuery ListRecoveryPointsByBackupVault' {..} =
    Prelude.mconcat
      [ "resourceArn" Core.=: byResourceArn,
        "createdAfter" Core.=: byCreatedAfter,
        "createdBefore" Core.=: byCreatedBefore,
        "backupPlanId" Core.=: byBackupPlanId,
        "resourceType" Core.=: byResourceType,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListRecoveryPointsByBackupVaultResponse' smart constructor.
data ListRecoveryPointsByBackupVaultResponse = ListRecoveryPointsByBackupVaultResponse'
  { -- | An array of objects that contain detailed information about recovery
    -- points saved in a backup vault.
    recoveryPoints :: Prelude.Maybe [RecoveryPointByBackupVault],
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecoveryPointsByBackupVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryPoints', 'listRecoveryPointsByBackupVaultResponse_recoveryPoints' - An array of objects that contain detailed information about recovery
-- points saved in a backup vault.
--
-- 'nextToken', 'listRecoveryPointsByBackupVaultResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'httpStatus', 'listRecoveryPointsByBackupVaultResponse_httpStatus' - The response's http status code.
newListRecoveryPointsByBackupVaultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecoveryPointsByBackupVaultResponse
newListRecoveryPointsByBackupVaultResponse
  pHttpStatus_ =
    ListRecoveryPointsByBackupVaultResponse'
      { recoveryPoints =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that contain detailed information about recovery
-- points saved in a backup vault.
listRecoveryPointsByBackupVaultResponse_recoveryPoints :: Lens.Lens' ListRecoveryPointsByBackupVaultResponse (Prelude.Maybe [RecoveryPointByBackupVault])
listRecoveryPointsByBackupVaultResponse_recoveryPoints = Lens.lens (\ListRecoveryPointsByBackupVaultResponse' {recoveryPoints} -> recoveryPoints) (\s@ListRecoveryPointsByBackupVaultResponse' {} a -> s {recoveryPoints = a} :: ListRecoveryPointsByBackupVaultResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listRecoveryPointsByBackupVaultResponse_nextToken :: Lens.Lens' ListRecoveryPointsByBackupVaultResponse (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVaultResponse_nextToken = Lens.lens (\ListRecoveryPointsByBackupVaultResponse' {nextToken} -> nextToken) (\s@ListRecoveryPointsByBackupVaultResponse' {} a -> s {nextToken = a} :: ListRecoveryPointsByBackupVaultResponse)

-- | The response's http status code.
listRecoveryPointsByBackupVaultResponse_httpStatus :: Lens.Lens' ListRecoveryPointsByBackupVaultResponse Prelude.Int
listRecoveryPointsByBackupVaultResponse_httpStatus = Lens.lens (\ListRecoveryPointsByBackupVaultResponse' {httpStatus} -> httpStatus) (\s@ListRecoveryPointsByBackupVaultResponse' {} a -> s {httpStatus = a} :: ListRecoveryPointsByBackupVaultResponse)

instance
  Prelude.NFData
    ListRecoveryPointsByBackupVaultResponse
  where
  rnf ListRecoveryPointsByBackupVaultResponse' {..} =
    Prelude.rnf recoveryPoints
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
