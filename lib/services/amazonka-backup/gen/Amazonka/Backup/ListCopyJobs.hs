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
-- Module      : Amazonka.Backup.ListCopyJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about your copy jobs.
--
-- This operation returns paginated results.
module Amazonka.Backup.ListCopyJobs
  ( -- * Creating a Request
    ListCopyJobs (..),
    newListCopyJobs,

    -- * Request Lenses
    listCopyJobs_byAccountId,
    listCopyJobs_byCompleteAfter,
    listCopyJobs_byCompleteBefore,
    listCopyJobs_byCreatedAfter,
    listCopyJobs_byCreatedBefore,
    listCopyJobs_byDestinationVaultArn,
    listCopyJobs_byParentJobId,
    listCopyJobs_byResourceArn,
    listCopyJobs_byResourceType,
    listCopyJobs_byState,
    listCopyJobs_maxResults,
    listCopyJobs_nextToken,

    -- * Destructuring the Response
    ListCopyJobsResponse (..),
    newListCopyJobsResponse,

    -- * Response Lenses
    listCopyJobsResponse_copyJobs,
    listCopyJobsResponse_nextToken,
    listCopyJobsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCopyJobs' smart constructor.
data ListCopyJobs = ListCopyJobs'
  { -- | The account ID to list the jobs from. Returns only copy jobs associated
    -- with the specified account ID.
    byAccountId :: Prelude.Maybe Prelude.Text,
    -- | Returns only copy jobs completed after a date expressed in Unix format
    -- and Coordinated Universal Time (UTC).
    byCompleteAfter :: Prelude.Maybe Data.POSIX,
    -- | Returns only copy jobs completed before a date expressed in Unix format
    -- and Coordinated Universal Time (UTC).
    byCompleteBefore :: Prelude.Maybe Data.POSIX,
    -- | Returns only copy jobs that were created after the specified date.
    byCreatedAfter :: Prelude.Maybe Data.POSIX,
    -- | Returns only copy jobs that were created before the specified date.
    byCreatedBefore :: Prelude.Maybe Data.POSIX,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a source backup
    -- vault to copy from; for example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    byDestinationVaultArn :: Prelude.Maybe Prelude.Text,
    -- | This is a filter to list child (nested) jobs based on parent job ID.
    byParentJobId :: Prelude.Maybe Prelude.Text,
    -- | Returns only copy jobs that match the specified resource Amazon Resource
    -- Name (ARN).
    byResourceArn :: Prelude.Maybe Prelude.Text,
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
    -- | Returns only copy jobs that are in the specified state.
    byState :: Prelude.Maybe CopyJobState,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return maxResults number of items, NextToken
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCopyJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byAccountId', 'listCopyJobs_byAccountId' - The account ID to list the jobs from. Returns only copy jobs associated
-- with the specified account ID.
--
-- 'byCompleteAfter', 'listCopyJobs_byCompleteAfter' - Returns only copy jobs completed after a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
--
-- 'byCompleteBefore', 'listCopyJobs_byCompleteBefore' - Returns only copy jobs completed before a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
--
-- 'byCreatedAfter', 'listCopyJobs_byCreatedAfter' - Returns only copy jobs that were created after the specified date.
--
-- 'byCreatedBefore', 'listCopyJobs_byCreatedBefore' - Returns only copy jobs that were created before the specified date.
--
-- 'byDestinationVaultArn', 'listCopyJobs_byDestinationVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a source backup
-- vault to copy from; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'byParentJobId', 'listCopyJobs_byParentJobId' - This is a filter to list child (nested) jobs based on parent job ID.
--
-- 'byResourceArn', 'listCopyJobs_byResourceArn' - Returns only copy jobs that match the specified resource Amazon Resource
-- Name (ARN).
--
-- 'byResourceType', 'listCopyJobs_byResourceType' - Returns only backup jobs for the specified resources:
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
-- 'byState', 'listCopyJobs_byState' - Returns only copy jobs that are in the specified state.
--
-- 'maxResults', 'listCopyJobs_maxResults' - The maximum number of items to be returned.
--
-- 'nextToken', 'listCopyJobs_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return maxResults number of items, NextToken
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
newListCopyJobs ::
  ListCopyJobs
newListCopyJobs =
  ListCopyJobs'
    { byAccountId = Prelude.Nothing,
      byCompleteAfter = Prelude.Nothing,
      byCompleteBefore = Prelude.Nothing,
      byCreatedAfter = Prelude.Nothing,
      byCreatedBefore = Prelude.Nothing,
      byDestinationVaultArn = Prelude.Nothing,
      byParentJobId = Prelude.Nothing,
      byResourceArn = Prelude.Nothing,
      byResourceType = Prelude.Nothing,
      byState = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The account ID to list the jobs from. Returns only copy jobs associated
-- with the specified account ID.
listCopyJobs_byAccountId :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.Text)
listCopyJobs_byAccountId = Lens.lens (\ListCopyJobs' {byAccountId} -> byAccountId) (\s@ListCopyJobs' {} a -> s {byAccountId = a} :: ListCopyJobs)

-- | Returns only copy jobs completed after a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
listCopyJobs_byCompleteAfter :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.UTCTime)
listCopyJobs_byCompleteAfter = Lens.lens (\ListCopyJobs' {byCompleteAfter} -> byCompleteAfter) (\s@ListCopyJobs' {} a -> s {byCompleteAfter = a} :: ListCopyJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only copy jobs completed before a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
listCopyJobs_byCompleteBefore :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.UTCTime)
listCopyJobs_byCompleteBefore = Lens.lens (\ListCopyJobs' {byCompleteBefore} -> byCompleteBefore) (\s@ListCopyJobs' {} a -> s {byCompleteBefore = a} :: ListCopyJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only copy jobs that were created after the specified date.
listCopyJobs_byCreatedAfter :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.UTCTime)
listCopyJobs_byCreatedAfter = Lens.lens (\ListCopyJobs' {byCreatedAfter} -> byCreatedAfter) (\s@ListCopyJobs' {} a -> s {byCreatedAfter = a} :: ListCopyJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only copy jobs that were created before the specified date.
listCopyJobs_byCreatedBefore :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.UTCTime)
listCopyJobs_byCreatedBefore = Lens.lens (\ListCopyJobs' {byCreatedBefore} -> byCreatedBefore) (\s@ListCopyJobs' {} a -> s {byCreatedBefore = a} :: ListCopyJobs) Prelude.. Lens.mapping Data._Time

-- | An Amazon Resource Name (ARN) that uniquely identifies a source backup
-- vault to copy from; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
listCopyJobs_byDestinationVaultArn :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.Text)
listCopyJobs_byDestinationVaultArn = Lens.lens (\ListCopyJobs' {byDestinationVaultArn} -> byDestinationVaultArn) (\s@ListCopyJobs' {} a -> s {byDestinationVaultArn = a} :: ListCopyJobs)

-- | This is a filter to list child (nested) jobs based on parent job ID.
listCopyJobs_byParentJobId :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.Text)
listCopyJobs_byParentJobId = Lens.lens (\ListCopyJobs' {byParentJobId} -> byParentJobId) (\s@ListCopyJobs' {} a -> s {byParentJobId = a} :: ListCopyJobs)

-- | Returns only copy jobs that match the specified resource Amazon Resource
-- Name (ARN).
listCopyJobs_byResourceArn :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.Text)
listCopyJobs_byResourceArn = Lens.lens (\ListCopyJobs' {byResourceArn} -> byResourceArn) (\s@ListCopyJobs' {} a -> s {byResourceArn = a} :: ListCopyJobs)

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
listCopyJobs_byResourceType :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.Text)
listCopyJobs_byResourceType = Lens.lens (\ListCopyJobs' {byResourceType} -> byResourceType) (\s@ListCopyJobs' {} a -> s {byResourceType = a} :: ListCopyJobs)

-- | Returns only copy jobs that are in the specified state.
listCopyJobs_byState :: Lens.Lens' ListCopyJobs (Prelude.Maybe CopyJobState)
listCopyJobs_byState = Lens.lens (\ListCopyJobs' {byState} -> byState) (\s@ListCopyJobs' {} a -> s {byState = a} :: ListCopyJobs)

-- | The maximum number of items to be returned.
listCopyJobs_maxResults :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.Natural)
listCopyJobs_maxResults = Lens.lens (\ListCopyJobs' {maxResults} -> maxResults) (\s@ListCopyJobs' {} a -> s {maxResults = a} :: ListCopyJobs)

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return maxResults number of items, NextToken
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listCopyJobs_nextToken :: Lens.Lens' ListCopyJobs (Prelude.Maybe Prelude.Text)
listCopyJobs_nextToken = Lens.lens (\ListCopyJobs' {nextToken} -> nextToken) (\s@ListCopyJobs' {} a -> s {nextToken = a} :: ListCopyJobs)

instance Core.AWSPager ListCopyJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCopyJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCopyJobsResponse_copyJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listCopyJobs_nextToken
              Lens..~ rs
              Lens.^? listCopyJobsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListCopyJobs where
  type AWSResponse ListCopyJobs = ListCopyJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCopyJobsResponse'
            Prelude.<$> (x Data..?> "CopyJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCopyJobs where
  hashWithSalt _salt ListCopyJobs' {..} =
    _salt
      `Prelude.hashWithSalt` byAccountId
      `Prelude.hashWithSalt` byCompleteAfter
      `Prelude.hashWithSalt` byCompleteBefore
      `Prelude.hashWithSalt` byCreatedAfter
      `Prelude.hashWithSalt` byCreatedBefore
      `Prelude.hashWithSalt` byDestinationVaultArn
      `Prelude.hashWithSalt` byParentJobId
      `Prelude.hashWithSalt` byResourceArn
      `Prelude.hashWithSalt` byResourceType
      `Prelude.hashWithSalt` byState
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCopyJobs where
  rnf ListCopyJobs' {..} =
    Prelude.rnf byAccountId `Prelude.seq`
      Prelude.rnf byCompleteAfter `Prelude.seq`
        Prelude.rnf byCompleteBefore `Prelude.seq`
          Prelude.rnf byCreatedAfter `Prelude.seq`
            Prelude.rnf byCreatedBefore `Prelude.seq`
              Prelude.rnf byDestinationVaultArn `Prelude.seq`
                Prelude.rnf byParentJobId `Prelude.seq`
                  Prelude.rnf byResourceArn `Prelude.seq`
                    Prelude.rnf byResourceType `Prelude.seq`
                      Prelude.rnf byState `Prelude.seq`
                        Prelude.rnf maxResults `Prelude.seq`
                          Prelude.rnf nextToken

instance Data.ToHeaders ListCopyJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCopyJobs where
  toPath = Prelude.const "/copy-jobs/"

instance Data.ToQuery ListCopyJobs where
  toQuery ListCopyJobs' {..} =
    Prelude.mconcat
      [ "accountId" Data.=: byAccountId,
        "completeAfter" Data.=: byCompleteAfter,
        "completeBefore" Data.=: byCompleteBefore,
        "createdAfter" Data.=: byCreatedAfter,
        "createdBefore" Data.=: byCreatedBefore,
        "destinationVaultArn" Data.=: byDestinationVaultArn,
        "parentJobId" Data.=: byParentJobId,
        "resourceArn" Data.=: byResourceArn,
        "resourceType" Data.=: byResourceType,
        "state" Data.=: byState,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListCopyJobsResponse' smart constructor.
data ListCopyJobsResponse = ListCopyJobsResponse'
  { -- | An array of structures containing metadata about your copy jobs returned
    -- in JSON format.
    copyJobs :: Prelude.Maybe [CopyJob],
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return maxResults number of items, NextToken
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCopyJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyJobs', 'listCopyJobsResponse_copyJobs' - An array of structures containing metadata about your copy jobs returned
-- in JSON format.
--
-- 'nextToken', 'listCopyJobsResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return maxResults number of items, NextToken
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'httpStatus', 'listCopyJobsResponse_httpStatus' - The response's http status code.
newListCopyJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCopyJobsResponse
newListCopyJobsResponse pHttpStatus_ =
  ListCopyJobsResponse'
    { copyJobs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of structures containing metadata about your copy jobs returned
-- in JSON format.
listCopyJobsResponse_copyJobs :: Lens.Lens' ListCopyJobsResponse (Prelude.Maybe [CopyJob])
listCopyJobsResponse_copyJobs = Lens.lens (\ListCopyJobsResponse' {copyJobs} -> copyJobs) (\s@ListCopyJobsResponse' {} a -> s {copyJobs = a} :: ListCopyJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return maxResults number of items, NextToken
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listCopyJobsResponse_nextToken :: Lens.Lens' ListCopyJobsResponse (Prelude.Maybe Prelude.Text)
listCopyJobsResponse_nextToken = Lens.lens (\ListCopyJobsResponse' {nextToken} -> nextToken) (\s@ListCopyJobsResponse' {} a -> s {nextToken = a} :: ListCopyJobsResponse)

-- | The response's http status code.
listCopyJobsResponse_httpStatus :: Lens.Lens' ListCopyJobsResponse Prelude.Int
listCopyJobsResponse_httpStatus = Lens.lens (\ListCopyJobsResponse' {httpStatus} -> httpStatus) (\s@ListCopyJobsResponse' {} a -> s {httpStatus = a} :: ListCopyJobsResponse)

instance Prelude.NFData ListCopyJobsResponse where
  rnf ListCopyJobsResponse' {..} =
    Prelude.rnf copyJobs `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
