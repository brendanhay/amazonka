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
-- Module      : Amazonka.DynamoDB.ListBackups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List backups associated with an Amazon Web Services account. To list
-- backups for a given table, specify @TableName@. @ListBackups@ returns a
-- paginated list of results with at most 1 MB worth of items in a page.
-- You can also specify a maximum number of entries to be returned in a
-- page.
--
-- In the request, start time is inclusive, but end time is exclusive. Note
-- that these boundaries are for the time at which the original backup was
-- requested.
--
-- You can call @ListBackups@ a maximum of five times per second.
--
-- This operation returns paginated results.
module Amazonka.DynamoDB.ListBackups
  ( -- * Creating a Request
    ListBackups (..),
    newListBackups,

    -- * Request Lenses
    listBackups_backupType,
    listBackups_exclusiveStartBackupArn,
    listBackups_limit,
    listBackups_tableName,
    listBackups_timeRangeLowerBound,
    listBackups_timeRangeUpperBound,

    -- * Destructuring the Response
    ListBackupsResponse (..),
    newListBackupsResponse,

    -- * Response Lenses
    listBackupsResponse_backupSummaries,
    listBackupsResponse_lastEvaluatedBackupArn,
    listBackupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBackups' smart constructor.
data ListBackups = ListBackups'
  { -- | The backups from the table specified by @BackupType@ are listed.
    --
    -- Where @BackupType@ can be:
    --
    -- -   @USER@ - On-demand backup created by you. (The default setting if no
    --     other backup types are specified.)
    --
    -- -   @SYSTEM@ - On-demand backup automatically created by DynamoDB.
    --
    -- -   @ALL@ - All types of on-demand backups (USER and SYSTEM).
    backupType :: Prelude.Maybe BackupTypeFilter,
    -- | @LastEvaluatedBackupArn@ is the Amazon Resource Name (ARN) of the backup
    -- last evaluated when the current page of results was returned, inclusive
    -- of the current page of results. This value may be specified as the
    -- @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to
    -- fetch the next page of results.
    exclusiveStartBackupArn :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of backups to return at once.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The backups from the table specified by @TableName@ are listed.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | Only backups created after this time are listed. @TimeRangeLowerBound@
    -- is inclusive.
    timeRangeLowerBound :: Prelude.Maybe Data.POSIX,
    -- | Only backups created before this time are listed. @TimeRangeUpperBound@
    -- is exclusive.
    timeRangeUpperBound :: Prelude.Maybe Data.POSIX
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
-- 'backupType', 'listBackups_backupType' - The backups from the table specified by @BackupType@ are listed.
--
-- Where @BackupType@ can be:
--
-- -   @USER@ - On-demand backup created by you. (The default setting if no
--     other backup types are specified.)
--
-- -   @SYSTEM@ - On-demand backup automatically created by DynamoDB.
--
-- -   @ALL@ - All types of on-demand backups (USER and SYSTEM).
--
-- 'exclusiveStartBackupArn', 'listBackups_exclusiveStartBackupArn' - @LastEvaluatedBackupArn@ is the Amazon Resource Name (ARN) of the backup
-- last evaluated when the current page of results was returned, inclusive
-- of the current page of results. This value may be specified as the
-- @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to
-- fetch the next page of results.
--
-- 'limit', 'listBackups_limit' - Maximum number of backups to return at once.
--
-- 'tableName', 'listBackups_tableName' - The backups from the table specified by @TableName@ are listed.
--
-- 'timeRangeLowerBound', 'listBackups_timeRangeLowerBound' - Only backups created after this time are listed. @TimeRangeLowerBound@
-- is inclusive.
--
-- 'timeRangeUpperBound', 'listBackups_timeRangeUpperBound' - Only backups created before this time are listed. @TimeRangeUpperBound@
-- is exclusive.
newListBackups ::
  ListBackups
newListBackups =
  ListBackups'
    { backupType = Prelude.Nothing,
      exclusiveStartBackupArn = Prelude.Nothing,
      limit = Prelude.Nothing,
      tableName = Prelude.Nothing,
      timeRangeLowerBound = Prelude.Nothing,
      timeRangeUpperBound = Prelude.Nothing
    }

-- | The backups from the table specified by @BackupType@ are listed.
--
-- Where @BackupType@ can be:
--
-- -   @USER@ - On-demand backup created by you. (The default setting if no
--     other backup types are specified.)
--
-- -   @SYSTEM@ - On-demand backup automatically created by DynamoDB.
--
-- -   @ALL@ - All types of on-demand backups (USER and SYSTEM).
listBackups_backupType :: Lens.Lens' ListBackups (Prelude.Maybe BackupTypeFilter)
listBackups_backupType = Lens.lens (\ListBackups' {backupType} -> backupType) (\s@ListBackups' {} a -> s {backupType = a} :: ListBackups)

-- | @LastEvaluatedBackupArn@ is the Amazon Resource Name (ARN) of the backup
-- last evaluated when the current page of results was returned, inclusive
-- of the current page of results. This value may be specified as the
-- @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to
-- fetch the next page of results.
listBackups_exclusiveStartBackupArn :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.Text)
listBackups_exclusiveStartBackupArn = Lens.lens (\ListBackups' {exclusiveStartBackupArn} -> exclusiveStartBackupArn) (\s@ListBackups' {} a -> s {exclusiveStartBackupArn = a} :: ListBackups)

-- | Maximum number of backups to return at once.
listBackups_limit :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.Natural)
listBackups_limit = Lens.lens (\ListBackups' {limit} -> limit) (\s@ListBackups' {} a -> s {limit = a} :: ListBackups)

-- | The backups from the table specified by @TableName@ are listed.
listBackups_tableName :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.Text)
listBackups_tableName = Lens.lens (\ListBackups' {tableName} -> tableName) (\s@ListBackups' {} a -> s {tableName = a} :: ListBackups)

-- | Only backups created after this time are listed. @TimeRangeLowerBound@
-- is inclusive.
listBackups_timeRangeLowerBound :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.UTCTime)
listBackups_timeRangeLowerBound = Lens.lens (\ListBackups' {timeRangeLowerBound} -> timeRangeLowerBound) (\s@ListBackups' {} a -> s {timeRangeLowerBound = a} :: ListBackups) Prelude.. Lens.mapping Data._Time

-- | Only backups created before this time are listed. @TimeRangeUpperBound@
-- is exclusive.
listBackups_timeRangeUpperBound :: Lens.Lens' ListBackups (Prelude.Maybe Prelude.UTCTime)
listBackups_timeRangeUpperBound = Lens.lens (\ListBackups' {timeRangeUpperBound} -> timeRangeUpperBound) (\s@ListBackups' {} a -> s {timeRangeUpperBound = a} :: ListBackups) Prelude.. Lens.mapping Data._Time

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& listBackups_exclusiveStartBackupArn
          Lens..~ rs
          Lens.^? listBackupsResponse_lastEvaluatedBackupArn
          Prelude.. Lens._Just

instance Core.AWSRequest ListBackups where
  type AWSResponse ListBackups = ListBackupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBackupsResponse'
            Prelude.<$> ( x
                            Data..?> "BackupSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "LastEvaluatedBackupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBackups where
  hashWithSalt _salt ListBackups' {..} =
    _salt
      `Prelude.hashWithSalt` backupType
      `Prelude.hashWithSalt` exclusiveStartBackupArn
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` timeRangeLowerBound
      `Prelude.hashWithSalt` timeRangeUpperBound

instance Prelude.NFData ListBackups where
  rnf ListBackups' {..} =
    Prelude.rnf backupType
      `Prelude.seq` Prelude.rnf exclusiveStartBackupArn
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf timeRangeLowerBound
      `Prelude.seq` Prelude.rnf timeRangeUpperBound

instance Data.ToHeaders ListBackups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ListBackups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBackups where
  toJSON ListBackups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackupType" Data..=) Prelude.<$> backupType,
            ("ExclusiveStartBackupArn" Data..=)
              Prelude.<$> exclusiveStartBackupArn,
            ("Limit" Data..=) Prelude.<$> limit,
            ("TableName" Data..=) Prelude.<$> tableName,
            ("TimeRangeLowerBound" Data..=)
              Prelude.<$> timeRangeLowerBound,
            ("TimeRangeUpperBound" Data..=)
              Prelude.<$> timeRangeUpperBound
          ]
      )

instance Data.ToPath ListBackups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListBackups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBackupsResponse' smart constructor.
data ListBackupsResponse = ListBackupsResponse'
  { -- | List of @BackupSummary@ objects.
    backupSummaries :: Prelude.Maybe [BackupSummary],
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
    lastEvaluatedBackupArn :: Prelude.Maybe Prelude.Text,
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
-- 'backupSummaries', 'listBackupsResponse_backupSummaries' - List of @BackupSummary@ objects.
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
-- 'httpStatus', 'listBackupsResponse_httpStatus' - The response's http status code.
newListBackupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBackupsResponse
newListBackupsResponse pHttpStatus_ =
  ListBackupsResponse'
    { backupSummaries =
        Prelude.Nothing,
      lastEvaluatedBackupArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of @BackupSummary@ objects.
listBackupsResponse_backupSummaries :: Lens.Lens' ListBackupsResponse (Prelude.Maybe [BackupSummary])
listBackupsResponse_backupSummaries = Lens.lens (\ListBackupsResponse' {backupSummaries} -> backupSummaries) (\s@ListBackupsResponse' {} a -> s {backupSummaries = a} :: ListBackupsResponse) Prelude.. Lens.mapping Lens.coerced

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

-- | The response's http status code.
listBackupsResponse_httpStatus :: Lens.Lens' ListBackupsResponse Prelude.Int
listBackupsResponse_httpStatus = Lens.lens (\ListBackupsResponse' {httpStatus} -> httpStatus) (\s@ListBackupsResponse' {} a -> s {httpStatus = a} :: ListBackupsResponse)

instance Prelude.NFData ListBackupsResponse where
  rnf ListBackupsResponse' {..} =
    Prelude.rnf backupSummaries
      `Prelude.seq` Prelude.rnf lastEvaluatedBackupArn
      `Prelude.seq` Prelude.rnf httpStatus
