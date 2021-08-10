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
-- Module      : Network.AWS.DynamoDB.ListBackups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List backups associated with an AWS account. To list backups for a given
-- table, specify @TableName@. @ListBackups@ returns a paginated list of
-- results with at most 1 MB worth of items in a page. You can also specify
-- a maximum number of entries to be returned in a page.
--
-- In the request, start time is inclusive, but end time is exclusive. Note
-- that these boundaries are for the time at which the original backup was
-- requested.
--
-- You can call @ListBackups@ a maximum of five times per second.
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.ListBackups
  ( -- * Creating a Request
    ListBackups (..),
    newListBackups,

    -- * Request Lenses
    listBackups_tableName,
    listBackups_backupType,
    listBackups_timeRangeLowerBound,
    listBackups_limit,
    listBackups_exclusiveStartBackupArn,
    listBackups_timeRangeUpperBound,

    -- * Destructuring the Response
    ListBackupsResponse (..),
    newListBackupsResponse,

    -- * Response Lenses
    listBackupsResponse_lastEvaluatedBackupArn,
    listBackupsResponse_backupSummaries,
    listBackupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBackups' smart constructor.
data ListBackups = ListBackups'
  { -- | The backups from the table specified by @TableName@ are listed.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The backups from the table specified by @BackupType@ are listed.
    --
    -- Where @BackupType@ can be:
    --
    -- -   @USER@ - On-demand backup created by you.
    --
    -- -   @SYSTEM@ - On-demand backup automatically created by DynamoDB.
    --
    -- -   @ALL@ - All types of on-demand backups (USER and SYSTEM).
    backupType :: Prelude.Maybe BackupTypeFilter,
    -- | Only backups created after this time are listed. @TimeRangeLowerBound@
    -- is inclusive.
    timeRangeLowerBound :: Prelude.Maybe Core.POSIX,
    -- | Maximum number of backups to return at once.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | @LastEvaluatedBackupArn@ is the Amazon Resource Name (ARN) of the backup
    -- last evaluated when the current page of results was returned, inclusive
    -- of the current page of results. This value may be specified as the
    -- @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to
    -- fetch the next page of results.
    exclusiveStartBackupArn :: Prelude.Maybe Prelude.Text,
    -- | Only backups created before this time are listed. @TimeRangeUpperBound@
    -- is exclusive.
    timeRangeUpperBound :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'listBackups_tableName' - The backups from the table specified by @TableName@ are listed.
--
-- 'backupType', 'listBackups_backupType' - The backups from the table specified by @BackupType@ are listed.
--
-- Where @BackupType@ can be:
--
-- -   @USER@ - On-demand backup created by you.
--
-- -   @SYSTEM@ - On-demand backup automatically created by DynamoDB.
--
-- -   @ALL@ - All types of on-demand backups (USER and SYSTEM).
--
-- 'timeRangeLowerBound', 'listBackups_timeRangeLowerBound' - Only backups created after this time are listed. @TimeRangeLowerBound@
-- is inclusive.
--
-- 'limit', 'listBackups_limit' - Maximum number of backups to return at once.
--
-- 'exclusiveStartBackupArn', 'listBackups_exclusiveStartBackupArn' - @LastEvaluatedBackupArn@ is the Amazon Resource Name (ARN) of the backup
-- last evaluated when the current page of results was returned, inclusive
-- of the current page of results. This value may be specified as the
-- @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to
-- fetch the next page of results.
--
-- 'timeRangeUpperBound', 'listBackups_timeRangeUpperBound' - Only backups created before this time are listed. @TimeRangeUpperBound@
-- is exclusive.
newListBackups ::
  ListBackups
newListBackups =
  ListBackups'
    { tableName = Prelude.Nothing,
      backupType = Prelude.Nothing,
      timeRangeLowerBound = Prelude.Nothing,
      limit = Prelude.Nothing,
      exclusiveStartBackupArn = Prelude.Nothing,
      timeRangeUpperBound = Prelude.Nothing
    }

-- | The backups from the table specified by @TableName@ are listed.
listBackups_tableName :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.Text)
listBackups_tableName = Lens.lens (\ListBackups' {tableName} -> tableName) (\s@ListBackups' {} a -> s {tableName = a} :: ListBackups)

-- | The backups from the table specified by @BackupType@ are listed.
--
-- Where @BackupType@ can be:
--
-- -   @USER@ - On-demand backup created by you.
--
-- -   @SYSTEM@ - On-demand backup automatically created by DynamoDB.
--
-- -   @ALL@ - All types of on-demand backups (USER and SYSTEM).
listBackups_backupType :: Lens.Lens' ListBackups (Prelude.Maybe BackupTypeFilter)
listBackups_backupType = Lens.lens (\ListBackups' {backupType} -> backupType) (\s@ListBackups' {} a -> s {backupType = a} :: ListBackups)

-- | Only backups created after this time are listed. @TimeRangeLowerBound@
-- is inclusive.
listBackups_timeRangeLowerBound :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.UTCTime)
listBackups_timeRangeLowerBound = Lens.lens (\ListBackups' {timeRangeLowerBound} -> timeRangeLowerBound) (\s@ListBackups' {} a -> s {timeRangeLowerBound = a} :: ListBackups) Prelude.. Lens.mapping Core._Time

-- | Maximum number of backups to return at once.
listBackups_limit :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.Natural)
listBackups_limit = Lens.lens (\ListBackups' {limit} -> limit) (\s@ListBackups' {} a -> s {limit = a} :: ListBackups)

-- | @LastEvaluatedBackupArn@ is the Amazon Resource Name (ARN) of the backup
-- last evaluated when the current page of results was returned, inclusive
-- of the current page of results. This value may be specified as the
-- @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to
-- fetch the next page of results.
listBackups_exclusiveStartBackupArn :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.Text)
listBackups_exclusiveStartBackupArn = Lens.lens (\ListBackups' {exclusiveStartBackupArn} -> exclusiveStartBackupArn) (\s@ListBackups' {} a -> s {exclusiveStartBackupArn = a} :: ListBackups)

-- | Only backups created before this time are listed. @TimeRangeUpperBound@
-- is exclusive.
listBackups_timeRangeUpperBound :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.UTCTime)
listBackups_timeRangeUpperBound = Lens.lens (\ListBackups' {timeRangeUpperBound} -> timeRangeUpperBound) (\s@ListBackups' {} a -> s {timeRangeUpperBound = a} :: ListBackups) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListBackups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBackupsResponse_lastEvaluatedBackupArn
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBackupsResponse_backupSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBackups_exclusiveStartBackupArn
          Lens..~ rs
          Lens.^? listBackupsResponse_lastEvaluatedBackupArn
            Prelude.. Lens._Just

instance Core.AWSRequest ListBackups where
  type AWSResponse ListBackups = ListBackupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBackupsResponse'
            Prelude.<$> (x Core..?> "LastEvaluatedBackupArn")
            Prelude.<*> ( x Core..?> "BackupSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBackups

instance Prelude.NFData ListBackups

instance Core.ToHeaders ListBackups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.ListBackups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBackups where
  toJSON ListBackups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TableName" Core..=) Prelude.<$> tableName,
            ("BackupType" Core..=) Prelude.<$> backupType,
            ("TimeRangeLowerBound" Core..=)
              Prelude.<$> timeRangeLowerBound,
            ("Limit" Core..=) Prelude.<$> limit,
            ("ExclusiveStartBackupArn" Core..=)
              Prelude.<$> exclusiveStartBackupArn,
            ("TimeRangeUpperBound" Core..=)
              Prelude.<$> timeRangeUpperBound
          ]
      )

instance Core.ToPath ListBackups where
  toPath = Prelude.const "/"

instance Core.ToQuery ListBackups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBackupsResponse' smart constructor.
data ListBackupsResponse = ListBackupsResponse'
  { -- | The ARN of the backup last evaluated when the current page of results
    -- was returned, inclusive of the current page of results. This value may
    -- be specified as the @ExclusiveStartBackupArn@ of a new @ListBackups@
    -- operation in order to fetch the next page of results.
    --
    -- If @LastEvaluatedBackupArn@ is empty, then the last page of results has
    -- been processed and there are no more results to be retrieved.
    --
    -- If @LastEvaluatedBackupArn@ is not empty, this may or may not indicate
    -- that there is more data to be returned. All results are guaranteed to
    -- have been returned if and only if no value for @LastEvaluatedBackupArn@
    -- is returned.
    lastEvaluatedBackupArn :: Prelude.Maybe Prelude.Text,
    -- | List of @BackupSummary@ objects.
    backupSummaries :: Prelude.Maybe [BackupSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastEvaluatedBackupArn', 'listBackupsResponse_lastEvaluatedBackupArn' - The ARN of the backup last evaluated when the current page of results
-- was returned, inclusive of the current page of results. This value may
-- be specified as the @ExclusiveStartBackupArn@ of a new @ListBackups@
-- operation in order to fetch the next page of results.
--
-- If @LastEvaluatedBackupArn@ is empty, then the last page of results has
-- been processed and there are no more results to be retrieved.
--
-- If @LastEvaluatedBackupArn@ is not empty, this may or may not indicate
-- that there is more data to be returned. All results are guaranteed to
-- have been returned if and only if no value for @LastEvaluatedBackupArn@
-- is returned.
--
-- 'backupSummaries', 'listBackupsResponse_backupSummaries' - List of @BackupSummary@ objects.
--
-- 'httpStatus', 'listBackupsResponse_httpStatus' - The response's http status code.
newListBackupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBackupsResponse
newListBackupsResponse pHttpStatus_ =
  ListBackupsResponse'
    { lastEvaluatedBackupArn =
        Prelude.Nothing,
      backupSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the backup last evaluated when the current page of results
-- was returned, inclusive of the current page of results. This value may
-- be specified as the @ExclusiveStartBackupArn@ of a new @ListBackups@
-- operation in order to fetch the next page of results.
--
-- If @LastEvaluatedBackupArn@ is empty, then the last page of results has
-- been processed and there are no more results to be retrieved.
--
-- If @LastEvaluatedBackupArn@ is not empty, this may or may not indicate
-- that there is more data to be returned. All results are guaranteed to
-- have been returned if and only if no value for @LastEvaluatedBackupArn@
-- is returned.
listBackupsResponse_lastEvaluatedBackupArn :: Lens.Lens' ListBackupsResponse (Prelude.Maybe Prelude.Text)
listBackupsResponse_lastEvaluatedBackupArn = Lens.lens (\ListBackupsResponse' {lastEvaluatedBackupArn} -> lastEvaluatedBackupArn) (\s@ListBackupsResponse' {} a -> s {lastEvaluatedBackupArn = a} :: ListBackupsResponse)

-- | List of @BackupSummary@ objects.
listBackupsResponse_backupSummaries :: Lens.Lens' ListBackupsResponse (Prelude.Maybe [BackupSummary])
listBackupsResponse_backupSummaries = Lens.lens (\ListBackupsResponse' {backupSummaries} -> backupSummaries) (\s@ListBackupsResponse' {} a -> s {backupSummaries = a} :: ListBackupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBackupsResponse_httpStatus :: Lens.Lens' ListBackupsResponse Prelude.Int
listBackupsResponse_httpStatus = Lens.lens (\ListBackupsResponse' {httpStatus} -> httpStatus) (\s@ListBackupsResponse' {} a -> s {httpStatus = a} :: ListBackupsResponse)

instance Prelude.NFData ListBackupsResponse
