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
-- Module      : Amazonka.Backup.ListBackupJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing backup jobs for an authenticated account for
-- the last 30 days. For a longer period of time, consider using these
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/monitoring.html monitoring tools>.
--
-- This operation returns paginated results.
module Amazonka.Backup.ListBackupJobs
  ( -- * Creating a Request
    ListBackupJobs (..),
    newListBackupJobs,

    -- * Request Lenses
    listBackupJobs_byAccountId,
    listBackupJobs_nextToken,
    listBackupJobs_byCreatedAfter,
    listBackupJobs_byCompleteAfter,
    listBackupJobs_byCompleteBefore,
    listBackupJobs_byResourceType,
    listBackupJobs_byCreatedBefore,
    listBackupJobs_maxResults,
    listBackupJobs_byState,
    listBackupJobs_byBackupVaultName,
    listBackupJobs_byResourceArn,

    -- * Destructuring the Response
    ListBackupJobsResponse (..),
    newListBackupJobsResponse,

    -- * Response Lenses
    listBackupJobsResponse_nextToken,
    listBackupJobsResponse_backupJobs,
    listBackupJobsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBackupJobs' smart constructor.
data ListBackupJobs = ListBackupJobs'
  { -- | The account ID to list the jobs from. Returns only backup jobs
    -- associated with the specified account ID.
    --
    -- If used from an Organizations management account, passing @*@ returns
    -- all jobs across the organization.
    byAccountId :: Prelude.Maybe Prelude.Text,
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns only backup jobs that were created after the specified date.
    byCreatedAfter :: Prelude.Maybe Data.POSIX,
    -- | Returns only backup jobs completed after a date expressed in Unix format
    -- and Coordinated Universal Time (UTC).
    byCompleteAfter :: Prelude.Maybe Data.POSIX,
    -- | Returns only backup jobs completed before a date expressed in Unix
    -- format and Coordinated Universal Time (UTC).
    byCompleteBefore :: Prelude.Maybe Data.POSIX,
    -- | Returns only backup jobs for the specified resources:
    --
    -- -   @Aurora@ for Amazon Aurora
    --
    -- -   @DocumentDB@ for Amazon DocumentDB (with MongoDB compatibility)
    --
    -- -   @DynamoDB@ for Amazon DynamoDB
    --
    -- -   @EBS@ for Amazon Elastic Block Store
    --
    -- -   @EC2@ for Amazon Elastic Compute Cloud
    --
    -- -   @EFS@ for Amazon Elastic File System
    --
    -- -   @FSx@ for Amazon FSx
    --
    -- -   @Neptune@ for Amazon Neptune
    --
    -- -   @RDS@ for Amazon Relational Database Service
    --
    -- -   @Storage Gateway@ for Storage Gateway
    --
    -- -   @S3@ for Amazon S3
    --
    -- -   @VirtualMachine@ for virtual machines
    byResourceType :: Prelude.Maybe Prelude.Text,
    -- | Returns only backup jobs that were created before the specified date.
    byCreatedBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Returns only backup jobs that are in the specified state.
    byState :: Prelude.Maybe BackupJobState,
    -- | Returns only backup jobs that will be stored in the specified backup
    -- vault. Backup vaults are identified by names that are unique to the
    -- account used to create them and the Amazon Web Services Region where
    -- they are created. They consist of lowercase letters, numbers, and
    -- hyphens.
    byBackupVaultName :: Prelude.Maybe Prelude.Text,
    -- | Returns only backup jobs that match the specified resource Amazon
    -- Resource Name (ARN).
    byResourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackupJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byAccountId', 'listBackupJobs_byAccountId' - The account ID to list the jobs from. Returns only backup jobs
-- associated with the specified account ID.
--
-- If used from an Organizations management account, passing @*@ returns
-- all jobs across the organization.
--
-- 'nextToken', 'listBackupJobs_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'byCreatedAfter', 'listBackupJobs_byCreatedAfter' - Returns only backup jobs that were created after the specified date.
--
-- 'byCompleteAfter', 'listBackupJobs_byCompleteAfter' - Returns only backup jobs completed after a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
--
-- 'byCompleteBefore', 'listBackupJobs_byCompleteBefore' - Returns only backup jobs completed before a date expressed in Unix
-- format and Coordinated Universal Time (UTC).
--
-- 'byResourceType', 'listBackupJobs_byResourceType' - Returns only backup jobs for the specified resources:
--
-- -   @Aurora@ for Amazon Aurora
--
-- -   @DocumentDB@ for Amazon DocumentDB (with MongoDB compatibility)
--
-- -   @DynamoDB@ for Amazon DynamoDB
--
-- -   @EBS@ for Amazon Elastic Block Store
--
-- -   @EC2@ for Amazon Elastic Compute Cloud
--
-- -   @EFS@ for Amazon Elastic File System
--
-- -   @FSx@ for Amazon FSx
--
-- -   @Neptune@ for Amazon Neptune
--
-- -   @RDS@ for Amazon Relational Database Service
--
-- -   @Storage Gateway@ for Storage Gateway
--
-- -   @S3@ for Amazon S3
--
-- -   @VirtualMachine@ for virtual machines
--
-- 'byCreatedBefore', 'listBackupJobs_byCreatedBefore' - Returns only backup jobs that were created before the specified date.
--
-- 'maxResults', 'listBackupJobs_maxResults' - The maximum number of items to be returned.
--
-- 'byState', 'listBackupJobs_byState' - Returns only backup jobs that are in the specified state.
--
-- 'byBackupVaultName', 'listBackupJobs_byBackupVaultName' - Returns only backup jobs that will be stored in the specified backup
-- vault. Backup vaults are identified by names that are unique to the
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of lowercase letters, numbers, and
-- hyphens.
--
-- 'byResourceArn', 'listBackupJobs_byResourceArn' - Returns only backup jobs that match the specified resource Amazon
-- Resource Name (ARN).
newListBackupJobs ::
  ListBackupJobs
newListBackupJobs =
  ListBackupJobs'
    { byAccountId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      byCreatedAfter = Prelude.Nothing,
      byCompleteAfter = Prelude.Nothing,
      byCompleteBefore = Prelude.Nothing,
      byResourceType = Prelude.Nothing,
      byCreatedBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      byState = Prelude.Nothing,
      byBackupVaultName = Prelude.Nothing,
      byResourceArn = Prelude.Nothing
    }

-- | The account ID to list the jobs from. Returns only backup jobs
-- associated with the specified account ID.
--
-- If used from an Organizations management account, passing @*@ returns
-- all jobs across the organization.
listBackupJobs_byAccountId :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_byAccountId = Lens.lens (\ListBackupJobs' {byAccountId} -> byAccountId) (\s@ListBackupJobs' {} a -> s {byAccountId = a} :: ListBackupJobs)

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupJobs_nextToken :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_nextToken = Lens.lens (\ListBackupJobs' {nextToken} -> nextToken) (\s@ListBackupJobs' {} a -> s {nextToken = a} :: ListBackupJobs)

-- | Returns only backup jobs that were created after the specified date.
listBackupJobs_byCreatedAfter :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.UTCTime)
listBackupJobs_byCreatedAfter = Lens.lens (\ListBackupJobs' {byCreatedAfter} -> byCreatedAfter) (\s@ListBackupJobs' {} a -> s {byCreatedAfter = a} :: ListBackupJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only backup jobs completed after a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
listBackupJobs_byCompleteAfter :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.UTCTime)
listBackupJobs_byCompleteAfter = Lens.lens (\ListBackupJobs' {byCompleteAfter} -> byCompleteAfter) (\s@ListBackupJobs' {} a -> s {byCompleteAfter = a} :: ListBackupJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only backup jobs completed before a date expressed in Unix
-- format and Coordinated Universal Time (UTC).
listBackupJobs_byCompleteBefore :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.UTCTime)
listBackupJobs_byCompleteBefore = Lens.lens (\ListBackupJobs' {byCompleteBefore} -> byCompleteBefore) (\s@ListBackupJobs' {} a -> s {byCompleteBefore = a} :: ListBackupJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only backup jobs for the specified resources:
--
-- -   @Aurora@ for Amazon Aurora
--
-- -   @DocumentDB@ for Amazon DocumentDB (with MongoDB compatibility)
--
-- -   @DynamoDB@ for Amazon DynamoDB
--
-- -   @EBS@ for Amazon Elastic Block Store
--
-- -   @EC2@ for Amazon Elastic Compute Cloud
--
-- -   @EFS@ for Amazon Elastic File System
--
-- -   @FSx@ for Amazon FSx
--
-- -   @Neptune@ for Amazon Neptune
--
-- -   @RDS@ for Amazon Relational Database Service
--
-- -   @Storage Gateway@ for Storage Gateway
--
-- -   @S3@ for Amazon S3
--
-- -   @VirtualMachine@ for virtual machines
listBackupJobs_byResourceType :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_byResourceType = Lens.lens (\ListBackupJobs' {byResourceType} -> byResourceType) (\s@ListBackupJobs' {} a -> s {byResourceType = a} :: ListBackupJobs)

-- | Returns only backup jobs that were created before the specified date.
listBackupJobs_byCreatedBefore :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.UTCTime)
listBackupJobs_byCreatedBefore = Lens.lens (\ListBackupJobs' {byCreatedBefore} -> byCreatedBefore) (\s@ListBackupJobs' {} a -> s {byCreatedBefore = a} :: ListBackupJobs) Prelude.. Lens.mapping Data._Time

-- | The maximum number of items to be returned.
listBackupJobs_maxResults :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Natural)
listBackupJobs_maxResults = Lens.lens (\ListBackupJobs' {maxResults} -> maxResults) (\s@ListBackupJobs' {} a -> s {maxResults = a} :: ListBackupJobs)

-- | Returns only backup jobs that are in the specified state.
listBackupJobs_byState :: Lens.Lens' ListBackupJobs (Prelude.Maybe BackupJobState)
listBackupJobs_byState = Lens.lens (\ListBackupJobs' {byState} -> byState) (\s@ListBackupJobs' {} a -> s {byState = a} :: ListBackupJobs)

-- | Returns only backup jobs that will be stored in the specified backup
-- vault. Backup vaults are identified by names that are unique to the
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of lowercase letters, numbers, and
-- hyphens.
listBackupJobs_byBackupVaultName :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_byBackupVaultName = Lens.lens (\ListBackupJobs' {byBackupVaultName} -> byBackupVaultName) (\s@ListBackupJobs' {} a -> s {byBackupVaultName = a} :: ListBackupJobs)

-- | Returns only backup jobs that match the specified resource Amazon
-- Resource Name (ARN).
listBackupJobs_byResourceArn :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_byResourceArn = Lens.lens (\ListBackupJobs' {byResourceArn} -> byResourceArn) (\s@ListBackupJobs' {} a -> s {byResourceArn = a} :: ListBackupJobs)

instance Core.AWSPager ListBackupJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBackupJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBackupJobsResponse_backupJobs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBackupJobs_nextToken
          Lens..~ rs
          Lens.^? listBackupJobsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListBackupJobs where
  type
    AWSResponse ListBackupJobs =
      ListBackupJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBackupJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "BackupJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBackupJobs where
  hashWithSalt _salt ListBackupJobs' {..} =
    _salt `Prelude.hashWithSalt` byAccountId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` byCreatedAfter
      `Prelude.hashWithSalt` byCompleteAfter
      `Prelude.hashWithSalt` byCompleteBefore
      `Prelude.hashWithSalt` byResourceType
      `Prelude.hashWithSalt` byCreatedBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` byState
      `Prelude.hashWithSalt` byBackupVaultName
      `Prelude.hashWithSalt` byResourceArn

instance Prelude.NFData ListBackupJobs where
  rnf ListBackupJobs' {..} =
    Prelude.rnf byAccountId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf byCreatedAfter
      `Prelude.seq` Prelude.rnf byCompleteAfter
      `Prelude.seq` Prelude.rnf byCompleteBefore
      `Prelude.seq` Prelude.rnf byResourceType
      `Prelude.seq` Prelude.rnf byCreatedBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf byState
      `Prelude.seq` Prelude.rnf byBackupVaultName
      `Prelude.seq` Prelude.rnf byResourceArn

instance Data.ToHeaders ListBackupJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBackupJobs where
  toPath = Prelude.const "/backup-jobs/"

instance Data.ToQuery ListBackupJobs where
  toQuery ListBackupJobs' {..} =
    Prelude.mconcat
      [ "accountId" Data.=: byAccountId,
        "nextToken" Data.=: nextToken,
        "createdAfter" Data.=: byCreatedAfter,
        "completeAfter" Data.=: byCompleteAfter,
        "completeBefore" Data.=: byCompleteBefore,
        "resourceType" Data.=: byResourceType,
        "createdBefore" Data.=: byCreatedBefore,
        "maxResults" Data.=: maxResults,
        "state" Data.=: byState,
        "backupVaultName" Data.=: byBackupVaultName,
        "resourceArn" Data.=: byResourceArn
      ]

-- | /See:/ 'newListBackupJobsResponse' smart constructor.
data ListBackupJobsResponse = ListBackupJobsResponse'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of structures containing metadata about your backup jobs
    -- returned in JSON format.
    backupJobs :: Prelude.Maybe [BackupJob],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackupJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBackupJobsResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'backupJobs', 'listBackupJobsResponse_backupJobs' - An array of structures containing metadata about your backup jobs
-- returned in JSON format.
--
-- 'httpStatus', 'listBackupJobsResponse_httpStatus' - The response's http status code.
newListBackupJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBackupJobsResponse
newListBackupJobsResponse pHttpStatus_ =
  ListBackupJobsResponse'
    { nextToken =
        Prelude.Nothing,
      backupJobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupJobsResponse_nextToken :: Lens.Lens' ListBackupJobsResponse (Prelude.Maybe Prelude.Text)
listBackupJobsResponse_nextToken = Lens.lens (\ListBackupJobsResponse' {nextToken} -> nextToken) (\s@ListBackupJobsResponse' {} a -> s {nextToken = a} :: ListBackupJobsResponse)

-- | An array of structures containing metadata about your backup jobs
-- returned in JSON format.
listBackupJobsResponse_backupJobs :: Lens.Lens' ListBackupJobsResponse (Prelude.Maybe [BackupJob])
listBackupJobsResponse_backupJobs = Lens.lens (\ListBackupJobsResponse' {backupJobs} -> backupJobs) (\s@ListBackupJobsResponse' {} a -> s {backupJobs = a} :: ListBackupJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBackupJobsResponse_httpStatus :: Lens.Lens' ListBackupJobsResponse Prelude.Int
listBackupJobsResponse_httpStatus = Lens.lens (\ListBackupJobsResponse' {httpStatus} -> httpStatus) (\s@ListBackupJobsResponse' {} a -> s {httpStatus = a} :: ListBackupJobsResponse)

instance Prelude.NFData ListBackupJobsResponse where
  rnf ListBackupJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf backupJobs
      `Prelude.seq` Prelude.rnf httpStatus
