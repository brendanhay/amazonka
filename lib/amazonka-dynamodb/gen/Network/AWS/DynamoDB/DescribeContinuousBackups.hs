{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeContinuousBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the status of continuous backups and point in time recovery on the specified table. Continuous backups are @ENABLED@ on all tables at table creation. If point in time recovery is enabled, @PointInTimeRecoveryStatus@ will be set to ENABLED.
--
-- After continuous backups and point in time recovery are enabled, you can restore to any point in time within @EarliestRestorableDateTime@ and @LatestRestorableDateTime@ .
-- @LatestRestorableDateTime@ is typically 5 minutes before the current time. You can restore your table to any point in time during the last 35 days.
-- You can call @DescribeContinuousBackups@ at a maximum rate of 10 times per second.
module Network.AWS.DynamoDB.DescribeContinuousBackups
  ( -- * Creating a request
    DescribeContinuousBackups (..),
    mkDescribeContinuousBackups,

    -- ** Request lenses
    dcbTableName,

    -- * Destructuring the response
    DescribeContinuousBackupsResponse (..),
    mkDescribeContinuousBackupsResponse,

    -- ** Response lenses
    dcbrsContinuousBackupsDescription,
    dcbrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeContinuousBackups' smart constructor.
newtype DescribeContinuousBackups = DescribeContinuousBackups'
  { -- | Name of the table for which the customer wants to check the continuous backups and point in time recovery settings.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeContinuousBackups' with the minimum fields required to make a request.
--
-- * 'tableName' - Name of the table for which the customer wants to check the continuous backups and point in time recovery settings.
mkDescribeContinuousBackups ::
  -- | 'tableName'
  Lude.Text ->
  DescribeContinuousBackups
mkDescribeContinuousBackups pTableName_ =
  DescribeContinuousBackups' {tableName = pTableName_}

-- | Name of the table for which the customer wants to check the continuous backups and point in time recovery settings.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbTableName :: Lens.Lens' DescribeContinuousBackups Lude.Text
dcbTableName = Lens.lens (tableName :: DescribeContinuousBackups -> Lude.Text) (\s a -> s {tableName = a} :: DescribeContinuousBackups)
{-# DEPRECATED dcbTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DescribeContinuousBackups where
  type
    Rs DescribeContinuousBackups =
      DescribeContinuousBackupsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeContinuousBackupsResponse'
            Lude.<$> (x Lude..?> "ContinuousBackupsDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeContinuousBackups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DescribeContinuousBackups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeContinuousBackups where
  toJSON DescribeContinuousBackups' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TableName" Lude..= tableName)])

instance Lude.ToPath DescribeContinuousBackups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeContinuousBackups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeContinuousBackupsResponse' smart constructor.
data DescribeContinuousBackupsResponse = DescribeContinuousBackupsResponse'
  { -- | Represents the continuous backups and point in time recovery settings on the table.
    continuousBackupsDescription :: Lude.Maybe ContinuousBackupsDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeContinuousBackupsResponse' with the minimum fields required to make a request.
--
-- * 'continuousBackupsDescription' - Represents the continuous backups and point in time recovery settings on the table.
-- * 'responseStatus' - The response status code.
mkDescribeContinuousBackupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeContinuousBackupsResponse
mkDescribeContinuousBackupsResponse pResponseStatus_ =
  DescribeContinuousBackupsResponse'
    { continuousBackupsDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the continuous backups and point in time recovery settings on the table.
--
-- /Note:/ Consider using 'continuousBackupsDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrsContinuousBackupsDescription :: Lens.Lens' DescribeContinuousBackupsResponse (Lude.Maybe ContinuousBackupsDescription)
dcbrsContinuousBackupsDescription = Lens.lens (continuousBackupsDescription :: DescribeContinuousBackupsResponse -> Lude.Maybe ContinuousBackupsDescription) (\s a -> s {continuousBackupsDescription = a} :: DescribeContinuousBackupsResponse)
{-# DEPRECATED dcbrsContinuousBackupsDescription "Use generic-lens or generic-optics with 'continuousBackupsDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbrsResponseStatus :: Lens.Lens' DescribeContinuousBackupsResponse Lude.Int
dcbrsResponseStatus = Lens.lens (responseStatus :: DescribeContinuousBackupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeContinuousBackupsResponse)
{-# DEPRECATED dcbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
