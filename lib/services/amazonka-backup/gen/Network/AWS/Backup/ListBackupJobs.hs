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
-- Module      : Network.AWS.Backup.ListBackupJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing backup jobs for an authenticated account for
-- the last 30 days. For a longer period of time, consider using these
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/monitoring.html monitoring tools>.
module Network.AWS.Backup.ListBackupJobs
  ( -- * Creating a Request
    ListBackupJobs (..),
    newListBackupJobs,

    -- * Request Lenses
    listBackupJobs_byResourceArn,
    listBackupJobs_byCreatedAfter,
    listBackupJobs_byAccountId,
    listBackupJobs_byCreatedBefore,
    listBackupJobs_byBackupVaultName,
    listBackupJobs_byResourceType,
    listBackupJobs_nextToken,
    listBackupJobs_byState,
    listBackupJobs_maxResults,

    -- * Destructuring the Response
    ListBackupJobsResponse (..),
    newListBackupJobsResponse,

    -- * Response Lenses
    listBackupJobsResponse_backupJobs,
    listBackupJobsResponse_nextToken,
    listBackupJobsResponse_httpStatus,
  )
where

import Network.AWS.Backup.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBackupJobs' smart constructor.
data ListBackupJobs = ListBackupJobs'
  { -- | Returns only backup jobs that match the specified resource Amazon
    -- Resource Name (ARN).
    byResourceArn :: Prelude.Maybe Prelude.Text,
    -- | Returns only backup jobs that were created after the specified date.
    byCreatedAfter :: Prelude.Maybe Core.POSIX,
    -- | The account ID to list the jobs from. Returns only backup jobs
    -- associated with the specified account ID.
    --
    -- If used from an Organizations management account, passing @*@ returns
    -- all jobs across the organization.
    byAccountId :: Prelude.Maybe Prelude.Text,
    -- | Returns only backup jobs that were created before the specified date.
    byCreatedBefore :: Prelude.Maybe Core.POSIX,
    -- | Returns only backup jobs that will be stored in the specified backup
    -- vault. Backup vaults are identified by names that are unique to the
    -- account used to create them and the Amazon Web Services Region where
    -- they are created. They consist of lowercase letters, numbers, and
    -- hyphens.
    byBackupVaultName :: Prelude.Maybe Prelude.Text,
    -- | Returns only backup jobs for the specified resources:
    --
    -- -   @DynamoDB@ for Amazon DynamoDB
    --
    -- -   @EBS@ for Amazon Elastic Block Store
    --
    -- -   @EC2@ for Amazon Elastic Compute Cloud
    --
    -- -   @EFS@ for Amazon Elastic File System
    --
    -- -   @RDS@ for Amazon Relational Database Service
    --
    -- -   @Aurora@ for Amazon Aurora
    --
    -- -   @Storage Gateway@ for Storage Gateway
    byResourceType :: Prelude.Maybe Prelude.Text,
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns only backup jobs that are in the specified state.
    byState :: Prelude.Maybe BackupJobState,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'byResourceArn', 'listBackupJobs_byResourceArn' - Returns only backup jobs that match the specified resource Amazon
-- Resource Name (ARN).
--
-- 'byCreatedAfter', 'listBackupJobs_byCreatedAfter' - Returns only backup jobs that were created after the specified date.
--
-- 'byAccountId', 'listBackupJobs_byAccountId' - The account ID to list the jobs from. Returns only backup jobs
-- associated with the specified account ID.
--
-- If used from an Organizations management account, passing @*@ returns
-- all jobs across the organization.
--
-- 'byCreatedBefore', 'listBackupJobs_byCreatedBefore' - Returns only backup jobs that were created before the specified date.
--
-- 'byBackupVaultName', 'listBackupJobs_byBackupVaultName' - Returns only backup jobs that will be stored in the specified backup
-- vault. Backup vaults are identified by names that are unique to the
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of lowercase letters, numbers, and
-- hyphens.
--
-- 'byResourceType', 'listBackupJobs_byResourceType' - Returns only backup jobs for the specified resources:
--
-- -   @DynamoDB@ for Amazon DynamoDB
--
-- -   @EBS@ for Amazon Elastic Block Store
--
-- -   @EC2@ for Amazon Elastic Compute Cloud
--
-- -   @EFS@ for Amazon Elastic File System
--
-- -   @RDS@ for Amazon Relational Database Service
--
-- -   @Aurora@ for Amazon Aurora
--
-- -   @Storage Gateway@ for Storage Gateway
--
-- 'nextToken', 'listBackupJobs_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'byState', 'listBackupJobs_byState' - Returns only backup jobs that are in the specified state.
--
-- 'maxResults', 'listBackupJobs_maxResults' - The maximum number of items to be returned.
newListBackupJobs ::
  ListBackupJobs
newListBackupJobs =
  ListBackupJobs'
    { byResourceArn = Prelude.Nothing,
      byCreatedAfter = Prelude.Nothing,
      byAccountId = Prelude.Nothing,
      byCreatedBefore = Prelude.Nothing,
      byBackupVaultName = Prelude.Nothing,
      byResourceType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      byState = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Returns only backup jobs that match the specified resource Amazon
-- Resource Name (ARN).
listBackupJobs_byResourceArn :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_byResourceArn = Lens.lens (\ListBackupJobs' {byResourceArn} -> byResourceArn) (\s@ListBackupJobs' {} a -> s {byResourceArn = a} :: ListBackupJobs)

-- | Returns only backup jobs that were created after the specified date.
listBackupJobs_byCreatedAfter :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.UTCTime)
listBackupJobs_byCreatedAfter = Lens.lens (\ListBackupJobs' {byCreatedAfter} -> byCreatedAfter) (\s@ListBackupJobs' {} a -> s {byCreatedAfter = a} :: ListBackupJobs) Prelude.. Lens.mapping Core._Time

-- | The account ID to list the jobs from. Returns only backup jobs
-- associated with the specified account ID.
--
-- If used from an Organizations management account, passing @*@ returns
-- all jobs across the organization.
listBackupJobs_byAccountId :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_byAccountId = Lens.lens (\ListBackupJobs' {byAccountId} -> byAccountId) (\s@ListBackupJobs' {} a -> s {byAccountId = a} :: ListBackupJobs)

-- | Returns only backup jobs that were created before the specified date.
listBackupJobs_byCreatedBefore :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.UTCTime)
listBackupJobs_byCreatedBefore = Lens.lens (\ListBackupJobs' {byCreatedBefore} -> byCreatedBefore) (\s@ListBackupJobs' {} a -> s {byCreatedBefore = a} :: ListBackupJobs) Prelude.. Lens.mapping Core._Time

-- | Returns only backup jobs that will be stored in the specified backup
-- vault. Backup vaults are identified by names that are unique to the
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of lowercase letters, numbers, and
-- hyphens.
listBackupJobs_byBackupVaultName :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_byBackupVaultName = Lens.lens (\ListBackupJobs' {byBackupVaultName} -> byBackupVaultName) (\s@ListBackupJobs' {} a -> s {byBackupVaultName = a} :: ListBackupJobs)

-- | Returns only backup jobs for the specified resources:
--
-- -   @DynamoDB@ for Amazon DynamoDB
--
-- -   @EBS@ for Amazon Elastic Block Store
--
-- -   @EC2@ for Amazon Elastic Compute Cloud
--
-- -   @EFS@ for Amazon Elastic File System
--
-- -   @RDS@ for Amazon Relational Database Service
--
-- -   @Aurora@ for Amazon Aurora
--
-- -   @Storage Gateway@ for Storage Gateway
listBackupJobs_byResourceType :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_byResourceType = Lens.lens (\ListBackupJobs' {byResourceType} -> byResourceType) (\s@ListBackupJobs' {} a -> s {byResourceType = a} :: ListBackupJobs)

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupJobs_nextToken :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Text)
listBackupJobs_nextToken = Lens.lens (\ListBackupJobs' {nextToken} -> nextToken) (\s@ListBackupJobs' {} a -> s {nextToken = a} :: ListBackupJobs)

-- | Returns only backup jobs that are in the specified state.
listBackupJobs_byState :: Lens.Lens' ListBackupJobs (Prelude.Maybe BackupJobState)
listBackupJobs_byState = Lens.lens (\ListBackupJobs' {byState} -> byState) (\s@ListBackupJobs' {} a -> s {byState = a} :: ListBackupJobs)

-- | The maximum number of items to be returned.
listBackupJobs_maxResults :: Lens.Lens' ListBackupJobs (Prelude.Maybe Prelude.Natural)
listBackupJobs_maxResults = Lens.lens (\ListBackupJobs' {maxResults} -> maxResults) (\s@ListBackupJobs' {} a -> s {maxResults = a} :: ListBackupJobs)

instance Core.AWSRequest ListBackupJobs where
  type
    AWSResponse ListBackupJobs =
      ListBackupJobsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBackupJobsResponse'
            Prelude.<$> (x Core..?> "BackupJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBackupJobs

instance Prelude.NFData ListBackupJobs

instance Core.ToHeaders ListBackupJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListBackupJobs where
  toPath = Prelude.const "/backup-jobs/"

instance Core.ToQuery ListBackupJobs where
  toQuery ListBackupJobs' {..} =
    Prelude.mconcat
      [ "resourceArn" Core.=: byResourceArn,
        "createdAfter" Core.=: byCreatedAfter,
        "accountId" Core.=: byAccountId,
        "createdBefore" Core.=: byCreatedBefore,
        "backupVaultName" Core.=: byBackupVaultName,
        "resourceType" Core.=: byResourceType,
        "nextToken" Core.=: nextToken,
        "state" Core.=: byState,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListBackupJobsResponse' smart constructor.
data ListBackupJobsResponse = ListBackupJobsResponse'
  { -- | An array of structures containing metadata about your backup jobs
    -- returned in JSON format.
    backupJobs :: Prelude.Maybe [BackupJob],
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
-- Create a value of 'ListBackupJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupJobs', 'listBackupJobsResponse_backupJobs' - An array of structures containing metadata about your backup jobs
-- returned in JSON format.
--
-- 'nextToken', 'listBackupJobsResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'httpStatus', 'listBackupJobsResponse_httpStatus' - The response's http status code.
newListBackupJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBackupJobsResponse
newListBackupJobsResponse pHttpStatus_ =
  ListBackupJobsResponse'
    { backupJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of structures containing metadata about your backup jobs
-- returned in JSON format.
listBackupJobsResponse_backupJobs :: Lens.Lens' ListBackupJobsResponse (Prelude.Maybe [BackupJob])
listBackupJobsResponse_backupJobs = Lens.lens (\ListBackupJobsResponse' {backupJobs} -> backupJobs) (\s@ListBackupJobsResponse' {} a -> s {backupJobs = a} :: ListBackupJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupJobsResponse_nextToken :: Lens.Lens' ListBackupJobsResponse (Prelude.Maybe Prelude.Text)
listBackupJobsResponse_nextToken = Lens.lens (\ListBackupJobsResponse' {nextToken} -> nextToken) (\s@ListBackupJobsResponse' {} a -> s {nextToken = a} :: ListBackupJobsResponse)

-- | The response's http status code.
listBackupJobsResponse_httpStatus :: Lens.Lens' ListBackupJobsResponse Prelude.Int
listBackupJobsResponse_httpStatus = Lens.lens (\ListBackupJobsResponse' {httpStatus} -> httpStatus) (\s@ListBackupJobsResponse' {} a -> s {httpStatus = a} :: ListBackupJobsResponse)

instance Prelude.NFData ListBackupJobsResponse
