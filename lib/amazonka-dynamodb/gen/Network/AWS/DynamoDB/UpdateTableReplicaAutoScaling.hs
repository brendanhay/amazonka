{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates auto scaling settings on your global tables at once.
module Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
  ( -- * Creating a request
    UpdateTableReplicaAutoScaling (..),
    mkUpdateTableReplicaAutoScaling,

    -- ** Request lenses
    utrasReplicaUpdates,
    utrasProvisionedWriteCapacityAutoScalingUpdate,
    utrasGlobalSecondaryIndexUpdates,
    utrasTableName,

    -- * Destructuring the response
    UpdateTableReplicaAutoScalingResponse (..),
    mkUpdateTableReplicaAutoScalingResponse,

    -- ** Response lenses
    utrasrsTableAutoScalingDescription,
    utrasrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTableReplicaAutoScaling' smart constructor.
data UpdateTableReplicaAutoScaling = UpdateTableReplicaAutoScaling'
  { -- | Represents the auto scaling settings of replicas of the table that will be modified.
    replicaUpdates :: Lude.Maybe (Lude.NonEmpty ReplicaAutoScalingUpdate),
    provisionedWriteCapacityAutoScalingUpdate :: Lude.Maybe AutoScalingSettingsUpdate,
    -- | Represents the auto scaling settings of the global secondary indexes of the replica to be updated.
    globalSecondaryIndexUpdates :: Lude.Maybe (Lude.NonEmpty GlobalSecondaryIndexAutoScalingUpdate),
    -- | The name of the global table to be updated.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTableReplicaAutoScaling' with the minimum fields required to make a request.
--
-- * 'replicaUpdates' - Represents the auto scaling settings of replicas of the table that will be modified.
-- * 'provisionedWriteCapacityAutoScalingUpdate' -
-- * 'globalSecondaryIndexUpdates' - Represents the auto scaling settings of the global secondary indexes of the replica to be updated.
-- * 'tableName' - The name of the global table to be updated.
mkUpdateTableReplicaAutoScaling ::
  -- | 'tableName'
  Lude.Text ->
  UpdateTableReplicaAutoScaling
mkUpdateTableReplicaAutoScaling pTableName_ =
  UpdateTableReplicaAutoScaling'
    { replicaUpdates = Lude.Nothing,
      provisionedWriteCapacityAutoScalingUpdate = Lude.Nothing,
      globalSecondaryIndexUpdates = Lude.Nothing,
      tableName = pTableName_
    }

-- | Represents the auto scaling settings of replicas of the table that will be modified.
--
-- /Note:/ Consider using 'replicaUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasReplicaUpdates :: Lens.Lens' UpdateTableReplicaAutoScaling (Lude.Maybe (Lude.NonEmpty ReplicaAutoScalingUpdate))
utrasReplicaUpdates = Lens.lens (replicaUpdates :: UpdateTableReplicaAutoScaling -> Lude.Maybe (Lude.NonEmpty ReplicaAutoScalingUpdate)) (\s a -> s {replicaUpdates = a} :: UpdateTableReplicaAutoScaling)
{-# DEPRECATED utrasReplicaUpdates "Use generic-lens or generic-optics with 'replicaUpdates' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasProvisionedWriteCapacityAutoScalingUpdate :: Lens.Lens' UpdateTableReplicaAutoScaling (Lude.Maybe AutoScalingSettingsUpdate)
utrasProvisionedWriteCapacityAutoScalingUpdate = Lens.lens (provisionedWriteCapacityAutoScalingUpdate :: UpdateTableReplicaAutoScaling -> Lude.Maybe AutoScalingSettingsUpdate) (\s a -> s {provisionedWriteCapacityAutoScalingUpdate = a} :: UpdateTableReplicaAutoScaling)
{-# DEPRECATED utrasProvisionedWriteCapacityAutoScalingUpdate "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingUpdate' instead." #-}

-- | Represents the auto scaling settings of the global secondary indexes of the replica to be updated.
--
-- /Note:/ Consider using 'globalSecondaryIndexUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasGlobalSecondaryIndexUpdates :: Lens.Lens' UpdateTableReplicaAutoScaling (Lude.Maybe (Lude.NonEmpty GlobalSecondaryIndexAutoScalingUpdate))
utrasGlobalSecondaryIndexUpdates = Lens.lens (globalSecondaryIndexUpdates :: UpdateTableReplicaAutoScaling -> Lude.Maybe (Lude.NonEmpty GlobalSecondaryIndexAutoScalingUpdate)) (\s a -> s {globalSecondaryIndexUpdates = a} :: UpdateTableReplicaAutoScaling)
{-# DEPRECATED utrasGlobalSecondaryIndexUpdates "Use generic-lens or generic-optics with 'globalSecondaryIndexUpdates' instead." #-}

-- | The name of the global table to be updated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasTableName :: Lens.Lens' UpdateTableReplicaAutoScaling Lude.Text
utrasTableName = Lens.lens (tableName :: UpdateTableReplicaAutoScaling -> Lude.Text) (\s a -> s {tableName = a} :: UpdateTableReplicaAutoScaling)
{-# DEPRECATED utrasTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest UpdateTableReplicaAutoScaling where
  type
    Rs UpdateTableReplicaAutoScaling =
      UpdateTableReplicaAutoScalingResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTableReplicaAutoScalingResponse'
            Lude.<$> (x Lude..?> "TableAutoScalingDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTableReplicaAutoScaling where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DynamoDB_20120810.UpdateTableReplicaAutoScaling" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTableReplicaAutoScaling where
  toJSON UpdateTableReplicaAutoScaling' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ReplicaUpdates" Lude..=) Lude.<$> replicaUpdates,
            ("ProvisionedWriteCapacityAutoScalingUpdate" Lude..=)
              Lude.<$> provisionedWriteCapacityAutoScalingUpdate,
            ("GlobalSecondaryIndexUpdates" Lude..=)
              Lude.<$> globalSecondaryIndexUpdates,
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath UpdateTableReplicaAutoScaling where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTableReplicaAutoScaling where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTableReplicaAutoScalingResponse' smart constructor.
data UpdateTableReplicaAutoScalingResponse = UpdateTableReplicaAutoScalingResponse'
  { -- | Returns information about the auto scaling settings of a table with replicas.
    tableAutoScalingDescription :: Lude.Maybe TableAutoScalingDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTableReplicaAutoScalingResponse' with the minimum fields required to make a request.
--
-- * 'tableAutoScalingDescription' - Returns information about the auto scaling settings of a table with replicas.
-- * 'responseStatus' - The response status code.
mkUpdateTableReplicaAutoScalingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTableReplicaAutoScalingResponse
mkUpdateTableReplicaAutoScalingResponse pResponseStatus_ =
  UpdateTableReplicaAutoScalingResponse'
    { tableAutoScalingDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns information about the auto scaling settings of a table with replicas.
--
-- /Note:/ Consider using 'tableAutoScalingDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasrsTableAutoScalingDescription :: Lens.Lens' UpdateTableReplicaAutoScalingResponse (Lude.Maybe TableAutoScalingDescription)
utrasrsTableAutoScalingDescription = Lens.lens (tableAutoScalingDescription :: UpdateTableReplicaAutoScalingResponse -> Lude.Maybe TableAutoScalingDescription) (\s a -> s {tableAutoScalingDescription = a} :: UpdateTableReplicaAutoScalingResponse)
{-# DEPRECATED utrasrsTableAutoScalingDescription "Use generic-lens or generic-optics with 'tableAutoScalingDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrasrsResponseStatus :: Lens.Lens' UpdateTableReplicaAutoScalingResponse Lude.Int
utrasrsResponseStatus = Lens.lens (responseStatus :: UpdateTableReplicaAutoScalingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTableReplicaAutoScalingResponse)
{-# DEPRECATED utrasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
