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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about the recovery points stored in a
-- backup vault.
--
-- This operation returns paginated results.
module Amazonka.Backup.ListRecoveryPointsByBackupVault
  ( -- * Creating a Request
    ListRecoveryPointsByBackupVault (..),
    newListRecoveryPointsByBackupVault,

    -- * Request Lenses
    listRecoveryPointsByBackupVault_byBackupPlanId,
    listRecoveryPointsByBackupVault_nextToken,
    listRecoveryPointsByBackupVault_byCreatedAfter,
    listRecoveryPointsByBackupVault_byResourceType,
    listRecoveryPointsByBackupVault_byCreatedBefore,
    listRecoveryPointsByBackupVault_maxResults,
    listRecoveryPointsByBackupVault_byResourceArn,
    listRecoveryPointsByBackupVault_backupVaultName,

    -- * Destructuring the Response
    ListRecoveryPointsByBackupVaultResponse (..),
    newListRecoveryPointsByBackupVaultResponse,

    -- * Response Lenses
    listRecoveryPointsByBackupVaultResponse_nextToken,
    listRecoveryPointsByBackupVaultResponse_recoveryPoints,
    listRecoveryPointsByBackupVaultResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecoveryPointsByBackupVault' smart constructor.
data ListRecoveryPointsByBackupVault = ListRecoveryPointsByBackupVault'
  { -- | Returns only recovery points that match the specified backup plan ID.
    byBackupPlanId :: Prelude.Maybe Prelude.Text,
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns only recovery points that were created after the specified
    -- timestamp.
    byCreatedAfter :: Prelude.Maybe Data.POSIX,
    -- | Returns only recovery points that match the specified resource type.
    byResourceType :: Prelude.Maybe Prelude.Text,
    -- | Returns only recovery points that were created before the specified
    -- timestamp.
    byCreatedBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Returns only recovery points that match the specified resource Amazon
    -- Resource Name (ARN).
    byResourceArn :: Prelude.Maybe Prelude.Text,
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
-- 'byBackupPlanId', 'listRecoveryPointsByBackupVault_byBackupPlanId' - Returns only recovery points that match the specified backup plan ID.
--
-- 'nextToken', 'listRecoveryPointsByBackupVault_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'byCreatedAfter', 'listRecoveryPointsByBackupVault_byCreatedAfter' - Returns only recovery points that were created after the specified
-- timestamp.
--
-- 'byResourceType', 'listRecoveryPointsByBackupVault_byResourceType' - Returns only recovery points that match the specified resource type.
--
-- 'byCreatedBefore', 'listRecoveryPointsByBackupVault_byCreatedBefore' - Returns only recovery points that were created before the specified
-- timestamp.
--
-- 'maxResults', 'listRecoveryPointsByBackupVault_maxResults' - The maximum number of items to be returned.
--
-- 'byResourceArn', 'listRecoveryPointsByBackupVault_byResourceArn' - Returns only recovery points that match the specified resource Amazon
-- Resource Name (ARN).
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
    { byBackupPlanId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      byCreatedAfter = Prelude.Nothing,
      byResourceType = Prelude.Nothing,
      byCreatedBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      byResourceArn = Prelude.Nothing,
      backupVaultName = pBackupVaultName_
    }

-- | Returns only recovery points that match the specified backup plan ID.
listRecoveryPointsByBackupVault_byBackupPlanId :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVault_byBackupPlanId = Lens.lens (\ListRecoveryPointsByBackupVault' {byBackupPlanId} -> byBackupPlanId) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byBackupPlanId = a} :: ListRecoveryPointsByBackupVault)

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listRecoveryPointsByBackupVault_nextToken :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVault_nextToken = Lens.lens (\ListRecoveryPointsByBackupVault' {nextToken} -> nextToken) (\s@ListRecoveryPointsByBackupVault' {} a -> s {nextToken = a} :: ListRecoveryPointsByBackupVault)

-- | Returns only recovery points that were created after the specified
-- timestamp.
listRecoveryPointsByBackupVault_byCreatedAfter :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.UTCTime)
listRecoveryPointsByBackupVault_byCreatedAfter = Lens.lens (\ListRecoveryPointsByBackupVault' {byCreatedAfter} -> byCreatedAfter) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byCreatedAfter = a} :: ListRecoveryPointsByBackupVault) Prelude.. Lens.mapping Data._Time

-- | Returns only recovery points that match the specified resource type.
listRecoveryPointsByBackupVault_byResourceType :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVault_byResourceType = Lens.lens (\ListRecoveryPointsByBackupVault' {byResourceType} -> byResourceType) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byResourceType = a} :: ListRecoveryPointsByBackupVault)

-- | Returns only recovery points that were created before the specified
-- timestamp.
listRecoveryPointsByBackupVault_byCreatedBefore :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.UTCTime)
listRecoveryPointsByBackupVault_byCreatedBefore = Lens.lens (\ListRecoveryPointsByBackupVault' {byCreatedBefore} -> byCreatedBefore) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byCreatedBefore = a} :: ListRecoveryPointsByBackupVault) Prelude.. Lens.mapping Data._Time

-- | The maximum number of items to be returned.
listRecoveryPointsByBackupVault_maxResults :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Natural)
listRecoveryPointsByBackupVault_maxResults = Lens.lens (\ListRecoveryPointsByBackupVault' {maxResults} -> maxResults) (\s@ListRecoveryPointsByBackupVault' {} a -> s {maxResults = a} :: ListRecoveryPointsByBackupVault)

-- | Returns only recovery points that match the specified resource Amazon
-- Resource Name (ARN).
listRecoveryPointsByBackupVault_byResourceArn :: Lens.Lens' ListRecoveryPointsByBackupVault (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVault_byResourceArn = Lens.lens (\ListRecoveryPointsByBackupVault' {byResourceArn} -> byResourceArn) (\s@ListRecoveryPointsByBackupVault' {} a -> s {byResourceArn = a} :: ListRecoveryPointsByBackupVault)

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
  Core.AWSPager
    ListRecoveryPointsByBackupVault
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecoveryPointsByBackupVaultResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRecoveryPointsByBackupVaultResponse_recoveryPoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRecoveryPointsByBackupVault_nextToken
          Lens..~ rs
          Lens.^? listRecoveryPointsByBackupVaultResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListRecoveryPointsByBackupVault
  where
  type
    AWSResponse ListRecoveryPointsByBackupVault =
      ListRecoveryPointsByBackupVaultResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecoveryPointsByBackupVaultResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RecoveryPoints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListRecoveryPointsByBackupVault
  where
  hashWithSalt
    _salt
    ListRecoveryPointsByBackupVault' {..} =
      _salt `Prelude.hashWithSalt` byBackupPlanId
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` byCreatedAfter
        `Prelude.hashWithSalt` byResourceType
        `Prelude.hashWithSalt` byCreatedBefore
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` byResourceArn
        `Prelude.hashWithSalt` backupVaultName

instance
  Prelude.NFData
    ListRecoveryPointsByBackupVault
  where
  rnf ListRecoveryPointsByBackupVault' {..} =
    Prelude.rnf byBackupPlanId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf byCreatedAfter
      `Prelude.seq` Prelude.rnf byResourceType
      `Prelude.seq` Prelude.rnf byCreatedBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf byResourceArn
      `Prelude.seq` Prelude.rnf backupVaultName

instance
  Data.ToHeaders
    ListRecoveryPointsByBackupVault
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

instance Data.ToPath ListRecoveryPointsByBackupVault where
  toPath ListRecoveryPointsByBackupVault' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/recovery-points/"
      ]

instance Data.ToQuery ListRecoveryPointsByBackupVault where
  toQuery ListRecoveryPointsByBackupVault' {..} =
    Prelude.mconcat
      [ "backupPlanId" Data.=: byBackupPlanId,
        "nextToken" Data.=: nextToken,
        "createdAfter" Data.=: byCreatedAfter,
        "resourceType" Data.=: byResourceType,
        "createdBefore" Data.=: byCreatedBefore,
        "maxResults" Data.=: maxResults,
        "resourceArn" Data.=: byResourceArn
      ]

-- | /See:/ 'newListRecoveryPointsByBackupVaultResponse' smart constructor.
data ListRecoveryPointsByBackupVaultResponse = ListRecoveryPointsByBackupVaultResponse'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain detailed information about recovery
    -- points saved in a backup vault.
    recoveryPoints :: Prelude.Maybe [RecoveryPointByBackupVault],
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
-- 'nextToken', 'listRecoveryPointsByBackupVaultResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'recoveryPoints', 'listRecoveryPointsByBackupVaultResponse_recoveryPoints' - An array of objects that contain detailed information about recovery
-- points saved in a backup vault.
--
-- 'httpStatus', 'listRecoveryPointsByBackupVaultResponse_httpStatus' - The response's http status code.
newListRecoveryPointsByBackupVaultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecoveryPointsByBackupVaultResponse
newListRecoveryPointsByBackupVaultResponse
  pHttpStatus_ =
    ListRecoveryPointsByBackupVaultResponse'
      { nextToken =
          Prelude.Nothing,
        recoveryPoints = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listRecoveryPointsByBackupVaultResponse_nextToken :: Lens.Lens' ListRecoveryPointsByBackupVaultResponse (Prelude.Maybe Prelude.Text)
listRecoveryPointsByBackupVaultResponse_nextToken = Lens.lens (\ListRecoveryPointsByBackupVaultResponse' {nextToken} -> nextToken) (\s@ListRecoveryPointsByBackupVaultResponse' {} a -> s {nextToken = a} :: ListRecoveryPointsByBackupVaultResponse)

-- | An array of objects that contain detailed information about recovery
-- points saved in a backup vault.
listRecoveryPointsByBackupVaultResponse_recoveryPoints :: Lens.Lens' ListRecoveryPointsByBackupVaultResponse (Prelude.Maybe [RecoveryPointByBackupVault])
listRecoveryPointsByBackupVaultResponse_recoveryPoints = Lens.lens (\ListRecoveryPointsByBackupVaultResponse' {recoveryPoints} -> recoveryPoints) (\s@ListRecoveryPointsByBackupVaultResponse' {} a -> s {recoveryPoints = a} :: ListRecoveryPointsByBackupVaultResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecoveryPointsByBackupVaultResponse_httpStatus :: Lens.Lens' ListRecoveryPointsByBackupVaultResponse Prelude.Int
listRecoveryPointsByBackupVaultResponse_httpStatus = Lens.lens (\ListRecoveryPointsByBackupVaultResponse' {httpStatus} -> httpStatus) (\s@ListRecoveryPointsByBackupVaultResponse' {} a -> s {httpStatus = a} :: ListRecoveryPointsByBackupVaultResponse)

instance
  Prelude.NFData
    ListRecoveryPointsByBackupVaultResponse
  where
  rnf ListRecoveryPointsByBackupVaultResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recoveryPoints
      `Prelude.seq` Prelude.rnf httpStatus
