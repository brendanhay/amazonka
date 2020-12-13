{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateContinuousBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @UpdateContinuousBackups@ enables or disables point in time recovery for the specified table. A successful @UpdateContinuousBackups@ call returns the current @ContinuousBackupsDescription@ . Continuous backups are @ENABLED@ on all tables at table creation. If point in time recovery is enabled, @PointInTimeRecoveryStatus@ will be set to ENABLED.
--
-- Once continuous backups and point in time recovery are enabled, you can restore to any point in time within @EarliestRestorableDateTime@ and @LatestRestorableDateTime@ .
-- @LatestRestorableDateTime@ is typically 5 minutes before the current time. You can restore your table to any point in time during the last 35 days.
module Network.AWS.DynamoDB.UpdateContinuousBackups
  ( -- * Creating a request
    UpdateContinuousBackups (..),
    mkUpdateContinuousBackups,

    -- ** Request lenses
    ucbPointInTimeRecoverySpecification,
    ucbTableName,

    -- * Destructuring the response
    UpdateContinuousBackupsResponse (..),
    mkUpdateContinuousBackupsResponse,

    -- ** Response lenses
    ucbrsContinuousBackupsDescription,
    ucbrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateContinuousBackups' smart constructor.
data UpdateContinuousBackups = UpdateContinuousBackups'
  { -- | Represents the settings used to enable point in time recovery.
    pointInTimeRecoverySpecification :: PointInTimeRecoverySpecification,
    -- | The name of the table.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContinuousBackups' with the minimum fields required to make a request.
--
-- * 'pointInTimeRecoverySpecification' - Represents the settings used to enable point in time recovery.
-- * 'tableName' - The name of the table.
mkUpdateContinuousBackups ::
  -- | 'pointInTimeRecoverySpecification'
  PointInTimeRecoverySpecification ->
  -- | 'tableName'
  Lude.Text ->
  UpdateContinuousBackups
mkUpdateContinuousBackups
  pPointInTimeRecoverySpecification_
  pTableName_ =
    UpdateContinuousBackups'
      { pointInTimeRecoverySpecification =
          pPointInTimeRecoverySpecification_,
        tableName = pTableName_
      }

-- | Represents the settings used to enable point in time recovery.
--
-- /Note:/ Consider using 'pointInTimeRecoverySpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucbPointInTimeRecoverySpecification :: Lens.Lens' UpdateContinuousBackups PointInTimeRecoverySpecification
ucbPointInTimeRecoverySpecification = Lens.lens (pointInTimeRecoverySpecification :: UpdateContinuousBackups -> PointInTimeRecoverySpecification) (\s a -> s {pointInTimeRecoverySpecification = a} :: UpdateContinuousBackups)
{-# DEPRECATED ucbPointInTimeRecoverySpecification "Use generic-lens or generic-optics with 'pointInTimeRecoverySpecification' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucbTableName :: Lens.Lens' UpdateContinuousBackups Lude.Text
ucbTableName = Lens.lens (tableName :: UpdateContinuousBackups -> Lude.Text) (\s a -> s {tableName = a} :: UpdateContinuousBackups)
{-# DEPRECATED ucbTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest UpdateContinuousBackups where
  type Rs UpdateContinuousBackups = UpdateContinuousBackupsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateContinuousBackupsResponse'
            Lude.<$> (x Lude..?> "ContinuousBackupsDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateContinuousBackups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.UpdateContinuousBackups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateContinuousBackups where
  toJSON UpdateContinuousBackups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "PointInTimeRecoverySpecification"
                  Lude..= pointInTimeRecoverySpecification
              ),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath UpdateContinuousBackups where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateContinuousBackups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateContinuousBackupsResponse' smart constructor.
data UpdateContinuousBackupsResponse = UpdateContinuousBackupsResponse'
  { -- | Represents the continuous backups and point in time recovery settings on the table.
    continuousBackupsDescription :: Lude.Maybe ContinuousBackupsDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContinuousBackupsResponse' with the minimum fields required to make a request.
--
-- * 'continuousBackupsDescription' - Represents the continuous backups and point in time recovery settings on the table.
-- * 'responseStatus' - The response status code.
mkUpdateContinuousBackupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateContinuousBackupsResponse
mkUpdateContinuousBackupsResponse pResponseStatus_ =
  UpdateContinuousBackupsResponse'
    { continuousBackupsDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the continuous backups and point in time recovery settings on the table.
--
-- /Note:/ Consider using 'continuousBackupsDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucbrsContinuousBackupsDescription :: Lens.Lens' UpdateContinuousBackupsResponse (Lude.Maybe ContinuousBackupsDescription)
ucbrsContinuousBackupsDescription = Lens.lens (continuousBackupsDescription :: UpdateContinuousBackupsResponse -> Lude.Maybe ContinuousBackupsDescription) (\s a -> s {continuousBackupsDescription = a} :: UpdateContinuousBackupsResponse)
{-# DEPRECATED ucbrsContinuousBackupsDescription "Use generic-lens or generic-optics with 'continuousBackupsDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucbrsResponseStatus :: Lens.Lens' UpdateContinuousBackupsResponse Lude.Int
ucbrsResponseStatus = Lens.lens (responseStatus :: UpdateContinuousBackupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateContinuousBackupsResponse)
{-# DEPRECATED ucbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
