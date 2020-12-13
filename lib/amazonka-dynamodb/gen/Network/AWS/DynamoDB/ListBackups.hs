{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List backups associated with an AWS account. To list backups for a given table, specify @TableName@ . @ListBackups@ returns a paginated list of results with at most 1 MB worth of items in a page. You can also specify a maximum number of entries to be returned in a page.
--
-- In the request, start time is inclusive, but end time is exclusive. Note that these boundaries are for the time at which the original backup was requested.
-- You can call @ListBackups@ a maximum of five times per second.
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.ListBackups
  ( -- * Creating a request
    ListBackups (..),
    mkListBackups,

    -- ** Request lenses
    lbTimeRangeUpperBound,
    lbTimeRangeLowerBound,
    lbLimit,
    lbExclusiveStartBackupARN,
    lbBackupType,
    lbTableName,

    -- * Destructuring the response
    ListBackupsResponse (..),
    mkListBackupsResponse,

    -- ** Response lenses
    lbrsBackupSummaries,
    lbrsLastEvaluatedBackupARN,
    lbrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBackups' smart constructor.
data ListBackups = ListBackups'
  { -- | Only backups created before this time are listed. @TimeRangeUpperBound@ is exclusive.
    timeRangeUpperBound :: Lude.Maybe Lude.Timestamp,
    -- | Only backups created after this time are listed. @TimeRangeLowerBound@ is inclusive.
    timeRangeLowerBound :: Lude.Maybe Lude.Timestamp,
    -- | Maximum number of backups to return at once.
    limit :: Lude.Maybe Lude.Natural,
    -- | @LastEvaluatedBackupArn@ is the Amazon Resource Name (ARN) of the backup last evaluated when the current page of results was returned, inclusive of the current page of results. This value may be specified as the @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to fetch the next page of results.
    exclusiveStartBackupARN :: Lude.Maybe Lude.Text,
    -- | The backups from the table specified by @BackupType@ are listed.
    --
    -- Where @BackupType@ can be:
    --
    --     * @USER@ - On-demand backup created by you.
    --
    --
    --     * @SYSTEM@ - On-demand backup automatically created by DynamoDB.
    --
    --
    --     * @ALL@ - All types of on-demand backups (USER and SYSTEM).
    backupType :: Lude.Maybe BackupTypeFilter,
    -- | The backups from the table specified by @TableName@ are listed.
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBackups' with the minimum fields required to make a request.
--
-- * 'timeRangeUpperBound' - Only backups created before this time are listed. @TimeRangeUpperBound@ is exclusive.
-- * 'timeRangeLowerBound' - Only backups created after this time are listed. @TimeRangeLowerBound@ is inclusive.
-- * 'limit' - Maximum number of backups to return at once.
-- * 'exclusiveStartBackupARN' - @LastEvaluatedBackupArn@ is the Amazon Resource Name (ARN) of the backup last evaluated when the current page of results was returned, inclusive of the current page of results. This value may be specified as the @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to fetch the next page of results.
-- * 'backupType' - The backups from the table specified by @BackupType@ are listed.
--
-- Where @BackupType@ can be:
--
--     * @USER@ - On-demand backup created by you.
--
--
--     * @SYSTEM@ - On-demand backup automatically created by DynamoDB.
--
--
--     * @ALL@ - All types of on-demand backups (USER and SYSTEM).
--
--
-- * 'tableName' - The backups from the table specified by @TableName@ are listed.
mkListBackups ::
  ListBackups
mkListBackups =
  ListBackups'
    { timeRangeUpperBound = Lude.Nothing,
      timeRangeLowerBound = Lude.Nothing,
      limit = Lude.Nothing,
      exclusiveStartBackupARN = Lude.Nothing,
      backupType = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | Only backups created before this time are listed. @TimeRangeUpperBound@ is exclusive.
--
-- /Note:/ Consider using 'timeRangeUpperBound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbTimeRangeUpperBound :: Lens.Lens' ListBackups (Lude.Maybe Lude.Timestamp)
lbTimeRangeUpperBound = Lens.lens (timeRangeUpperBound :: ListBackups -> Lude.Maybe Lude.Timestamp) (\s a -> s {timeRangeUpperBound = a} :: ListBackups)
{-# DEPRECATED lbTimeRangeUpperBound "Use generic-lens or generic-optics with 'timeRangeUpperBound' instead." #-}

-- | Only backups created after this time are listed. @TimeRangeLowerBound@ is inclusive.
--
-- /Note:/ Consider using 'timeRangeLowerBound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbTimeRangeLowerBound :: Lens.Lens' ListBackups (Lude.Maybe Lude.Timestamp)
lbTimeRangeLowerBound = Lens.lens (timeRangeLowerBound :: ListBackups -> Lude.Maybe Lude.Timestamp) (\s a -> s {timeRangeLowerBound = a} :: ListBackups)
{-# DEPRECATED lbTimeRangeLowerBound "Use generic-lens or generic-optics with 'timeRangeLowerBound' instead." #-}

-- | Maximum number of backups to return at once.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLimit :: Lens.Lens' ListBackups (Lude.Maybe Lude.Natural)
lbLimit = Lens.lens (limit :: ListBackups -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListBackups)
{-# DEPRECATED lbLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | @LastEvaluatedBackupArn@ is the Amazon Resource Name (ARN) of the backup last evaluated when the current page of results was returned, inclusive of the current page of results. This value may be specified as the @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to fetch the next page of results.
--
-- /Note:/ Consider using 'exclusiveStartBackupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbExclusiveStartBackupARN :: Lens.Lens' ListBackups (Lude.Maybe Lude.Text)
lbExclusiveStartBackupARN = Lens.lens (exclusiveStartBackupARN :: ListBackups -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartBackupARN = a} :: ListBackups)
{-# DEPRECATED lbExclusiveStartBackupARN "Use generic-lens or generic-optics with 'exclusiveStartBackupARN' instead." #-}

-- | The backups from the table specified by @BackupType@ are listed.
--
-- Where @BackupType@ can be:
--
--     * @USER@ - On-demand backup created by you.
--
--
--     * @SYSTEM@ - On-demand backup automatically created by DynamoDB.
--
--
--     * @ALL@ - All types of on-demand backups (USER and SYSTEM).
--
--
--
-- /Note:/ Consider using 'backupType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbBackupType :: Lens.Lens' ListBackups (Lude.Maybe BackupTypeFilter)
lbBackupType = Lens.lens (backupType :: ListBackups -> Lude.Maybe BackupTypeFilter) (\s a -> s {backupType = a} :: ListBackups)
{-# DEPRECATED lbBackupType "Use generic-lens or generic-optics with 'backupType' instead." #-}

-- | The backups from the table specified by @TableName@ are listed.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbTableName :: Lens.Lens' ListBackups (Lude.Maybe Lude.Text)
lbTableName = Lens.lens (tableName :: ListBackups -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: ListBackups)
{-# DEPRECATED lbTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Page.AWSPager ListBackups where
  page rq rs
    | Page.stop (rs Lens.^. lbrsLastEvaluatedBackupARN) = Lude.Nothing
    | Page.stop (rs Lens.^. lbrsBackupSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbExclusiveStartBackupARN
          Lens..~ rs Lens.^. lbrsLastEvaluatedBackupARN

instance Lude.AWSRequest ListBackups where
  type Rs ListBackups = ListBackupsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBackupsResponse'
            Lude.<$> (x Lude..?> "BackupSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "LastEvaluatedBackupArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBackups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.ListBackups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBackups where
  toJSON ListBackups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TimeRangeUpperBound" Lude..=) Lude.<$> timeRangeUpperBound,
            ("TimeRangeLowerBound" Lude..=) Lude.<$> timeRangeLowerBound,
            ("Limit" Lude..=) Lude.<$> limit,
            ("ExclusiveStartBackupArn" Lude..=)
              Lude.<$> exclusiveStartBackupARN,
            ("BackupType" Lude..=) Lude.<$> backupType,
            ("TableName" Lude..=) Lude.<$> tableName
          ]
      )

instance Lude.ToPath ListBackups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBackups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListBackupsResponse' smart constructor.
data ListBackupsResponse = ListBackupsResponse'
  { -- | List of @BackupSummary@ objects.
    backupSummaries :: Lude.Maybe [BackupSummary],
    -- | The ARN of the backup last evaluated when the current page of results was returned, inclusive of the current page of results. This value may be specified as the @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to fetch the next page of results.
    --
    -- If @LastEvaluatedBackupArn@ is empty, then the last page of results has been processed and there are no more results to be retrieved.
    -- If @LastEvaluatedBackupArn@ is not empty, this may or may not indicate that there is more data to be returned. All results are guaranteed to have been returned if and only if no value for @LastEvaluatedBackupArn@ is returned.
    lastEvaluatedBackupARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBackupsResponse' with the minimum fields required to make a request.
--
-- * 'backupSummaries' - List of @BackupSummary@ objects.
-- * 'lastEvaluatedBackupARN' - The ARN of the backup last evaluated when the current page of results was returned, inclusive of the current page of results. This value may be specified as the @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to fetch the next page of results.
--
-- If @LastEvaluatedBackupArn@ is empty, then the last page of results has been processed and there are no more results to be retrieved.
-- If @LastEvaluatedBackupArn@ is not empty, this may or may not indicate that there is more data to be returned. All results are guaranteed to have been returned if and only if no value for @LastEvaluatedBackupArn@ is returned.
-- * 'responseStatus' - The response status code.
mkListBackupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBackupsResponse
mkListBackupsResponse pResponseStatus_ =
  ListBackupsResponse'
    { backupSummaries = Lude.Nothing,
      lastEvaluatedBackupARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of @BackupSummary@ objects.
--
-- /Note:/ Consider using 'backupSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsBackupSummaries :: Lens.Lens' ListBackupsResponse (Lude.Maybe [BackupSummary])
lbrsBackupSummaries = Lens.lens (backupSummaries :: ListBackupsResponse -> Lude.Maybe [BackupSummary]) (\s a -> s {backupSummaries = a} :: ListBackupsResponse)
{-# DEPRECATED lbrsBackupSummaries "Use generic-lens or generic-optics with 'backupSummaries' instead." #-}

-- | The ARN of the backup last evaluated when the current page of results was returned, inclusive of the current page of results. This value may be specified as the @ExclusiveStartBackupArn@ of a new @ListBackups@ operation in order to fetch the next page of results.
--
-- If @LastEvaluatedBackupArn@ is empty, then the last page of results has been processed and there are no more results to be retrieved.
-- If @LastEvaluatedBackupArn@ is not empty, this may or may not indicate that there is more data to be returned. All results are guaranteed to have been returned if and only if no value for @LastEvaluatedBackupArn@ is returned.
--
-- /Note:/ Consider using 'lastEvaluatedBackupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsLastEvaluatedBackupARN :: Lens.Lens' ListBackupsResponse (Lude.Maybe Lude.Text)
lbrsLastEvaluatedBackupARN = Lens.lens (lastEvaluatedBackupARN :: ListBackupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastEvaluatedBackupARN = a} :: ListBackupsResponse)
{-# DEPRECATED lbrsLastEvaluatedBackupARN "Use generic-lens or generic-optics with 'lastEvaluatedBackupARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsResponseStatus :: Lens.Lens' ListBackupsResponse Lude.Int
lbrsResponseStatus = Lens.lens (responseStatus :: ListBackupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBackupsResponse)
{-# DEPRECATED lbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
