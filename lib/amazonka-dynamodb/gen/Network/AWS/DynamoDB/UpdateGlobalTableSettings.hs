{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateGlobalTableSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a global table.
module Network.AWS.DynamoDB.UpdateGlobalTableSettings
  ( -- * Creating a request
    UpdateGlobalTableSettings (..),
    mkUpdateGlobalTableSettings,

    -- ** Request lenses
    ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
    ugtsGlobalTableBillingMode,
    ugtsGlobalTableProvisionedWriteCapacityUnits,
    ugtsReplicaSettingsUpdate,
    ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate,
    ugtsGlobalTableName,

    -- * Destructuring the response
    UpdateGlobalTableSettingsResponse (..),
    mkUpdateGlobalTableSettingsResponse,

    -- ** Response lenses
    ugtsrsReplicaSettings,
    ugtsrsGlobalTableName,
    ugtsrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGlobalTableSettings' smart constructor.
data UpdateGlobalTableSettings = UpdateGlobalTableSettings'
  { globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate ::
      Lude.Maybe AutoScalingSettingsUpdate,
    globalTableBillingMode ::
      Lude.Maybe BillingMode,
    globalTableProvisionedWriteCapacityUnits ::
      Lude.Maybe Lude.Natural,
    replicaSettingsUpdate ::
      Lude.Maybe
        (Lude.NonEmpty ReplicaSettingsUpdate),
    globalTableGlobalSecondaryIndexSettingsUpdate ::
      Lude.Maybe
        ( Lude.NonEmpty
            GlobalTableGlobalSecondaryIndexSettingsUpdate
        ),
    globalTableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGlobalTableSettings' with the minimum fields required to make a request.
--
-- * 'globalTableBillingMode' - The billing mode of the global table. If @GlobalTableBillingMode@ is not specified, the global table defaults to @PROVISIONED@ capacity billing mode.
--
--
--     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
--
--
--     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
--
--
-- * 'globalTableGlobalSecondaryIndexSettingsUpdate' - Represents the settings of a global secondary index for a global table that will be modified.
-- * 'globalTableName' - The name of the global table
-- * 'globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing provisioned write capacity for the global table.
-- * 'globalTableProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
-- * 'replicaSettingsUpdate' - Represents the settings for a global table in a Region that will be modified.
mkUpdateGlobalTableSettings ::
  -- | 'globalTableName'
  Lude.Text ->
  UpdateGlobalTableSettings
mkUpdateGlobalTableSettings pGlobalTableName_ =
  UpdateGlobalTableSettings'
    { globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate =
        Lude.Nothing,
      globalTableBillingMode = Lude.Nothing,
      globalTableProvisionedWriteCapacityUnits = Lude.Nothing,
      replicaSettingsUpdate = Lude.Nothing,
      globalTableGlobalSecondaryIndexSettingsUpdate = Lude.Nothing,
      globalTableName = pGlobalTableName_
    }

-- | Auto scaling settings for managing provisioned write capacity for the global table.
--
-- /Note:/ Consider using 'globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Lude.Maybe AutoScalingSettingsUpdate)
ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate = Lens.lens (globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate :: UpdateGlobalTableSettings -> Lude.Maybe AutoScalingSettingsUpdate) (\s a -> s {globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate = a} :: UpdateGlobalTableSettings)
{-# DEPRECATED ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate "Use generic-lens or generic-optics with 'globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate' instead." #-}

-- | The billing mode of the global table. If @GlobalTableBillingMode@ is not specified, the global table defaults to @PROVISIONED@ capacity billing mode.
--
--
--     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
--
--
--     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
--
--
--
-- /Note:/ Consider using 'globalTableBillingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsGlobalTableBillingMode :: Lens.Lens' UpdateGlobalTableSettings (Lude.Maybe BillingMode)
ugtsGlobalTableBillingMode = Lens.lens (globalTableBillingMode :: UpdateGlobalTableSettings -> Lude.Maybe BillingMode) (\s a -> s {globalTableBillingMode = a} :: UpdateGlobalTableSettings)
{-# DEPRECATED ugtsGlobalTableBillingMode "Use generic-lens or generic-optics with 'globalTableBillingMode' instead." #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
--
-- /Note:/ Consider using 'globalTableProvisionedWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsGlobalTableProvisionedWriteCapacityUnits :: Lens.Lens' UpdateGlobalTableSettings (Lude.Maybe Lude.Natural)
ugtsGlobalTableProvisionedWriteCapacityUnits = Lens.lens (globalTableProvisionedWriteCapacityUnits :: UpdateGlobalTableSettings -> Lude.Maybe Lude.Natural) (\s a -> s {globalTableProvisionedWriteCapacityUnits = a} :: UpdateGlobalTableSettings)
{-# DEPRECATED ugtsGlobalTableProvisionedWriteCapacityUnits "Use generic-lens or generic-optics with 'globalTableProvisionedWriteCapacityUnits' instead." #-}

-- | Represents the settings for a global table in a Region that will be modified.
--
-- /Note:/ Consider using 'replicaSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsReplicaSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Lude.Maybe (Lude.NonEmpty ReplicaSettingsUpdate))
ugtsReplicaSettingsUpdate = Lens.lens (replicaSettingsUpdate :: UpdateGlobalTableSettings -> Lude.Maybe (Lude.NonEmpty ReplicaSettingsUpdate)) (\s a -> s {replicaSettingsUpdate = a} :: UpdateGlobalTableSettings)
{-# DEPRECATED ugtsReplicaSettingsUpdate "Use generic-lens or generic-optics with 'replicaSettingsUpdate' instead." #-}

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
-- /Note:/ Consider using 'globalTableGlobalSecondaryIndexSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Lude.Maybe (Lude.NonEmpty GlobalTableGlobalSecondaryIndexSettingsUpdate))
ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate = Lens.lens (globalTableGlobalSecondaryIndexSettingsUpdate :: UpdateGlobalTableSettings -> Lude.Maybe (Lude.NonEmpty GlobalTableGlobalSecondaryIndexSettingsUpdate)) (\s a -> s {globalTableGlobalSecondaryIndexSettingsUpdate = a} :: UpdateGlobalTableSettings)
{-# DEPRECATED ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate "Use generic-lens or generic-optics with 'globalTableGlobalSecondaryIndexSettingsUpdate' instead." #-}

-- | The name of the global table
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsGlobalTableName :: Lens.Lens' UpdateGlobalTableSettings Lude.Text
ugtsGlobalTableName = Lens.lens (globalTableName :: UpdateGlobalTableSettings -> Lude.Text) (\s a -> s {globalTableName = a} :: UpdateGlobalTableSettings)
{-# DEPRECATED ugtsGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

instance Lude.AWSRequest UpdateGlobalTableSettings where
  type
    Rs UpdateGlobalTableSettings =
      UpdateGlobalTableSettingsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGlobalTableSettingsResponse'
            Lude.<$> (x Lude..?> "ReplicaSettings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "GlobalTableName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGlobalTableSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.UpdateGlobalTableSettings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGlobalTableSettings where
  toJSON UpdateGlobalTableSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ( "GlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate"
                Lude..=
            )
              Lude.<$> globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
            ("GlobalTableBillingMode" Lude..=) Lude.<$> globalTableBillingMode,
            ("GlobalTableProvisionedWriteCapacityUnits" Lude..=)
              Lude.<$> globalTableProvisionedWriteCapacityUnits,
            ("ReplicaSettingsUpdate" Lude..=) Lude.<$> replicaSettingsUpdate,
            ("GlobalTableGlobalSecondaryIndexSettingsUpdate" Lude..=)
              Lude.<$> globalTableGlobalSecondaryIndexSettingsUpdate,
            Lude.Just ("GlobalTableName" Lude..= globalTableName)
          ]
      )

instance Lude.ToPath UpdateGlobalTableSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGlobalTableSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGlobalTableSettingsResponse' smart constructor.
data UpdateGlobalTableSettingsResponse = UpdateGlobalTableSettingsResponse'
  { replicaSettings ::
      Lude.Maybe
        [ReplicaSettingsDescription],
    globalTableName ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGlobalTableSettingsResponse' with the minimum fields required to make a request.
--
-- * 'globalTableName' - The name of the global table.
-- * 'replicaSettings' - The Region-specific settings for the global table.
-- * 'responseStatus' - The response status code.
mkUpdateGlobalTableSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGlobalTableSettingsResponse
mkUpdateGlobalTableSettingsResponse pResponseStatus_ =
  UpdateGlobalTableSettingsResponse'
    { replicaSettings =
        Lude.Nothing,
      globalTableName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Region-specific settings for the global table.
--
-- /Note:/ Consider using 'replicaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsrsReplicaSettings :: Lens.Lens' UpdateGlobalTableSettingsResponse (Lude.Maybe [ReplicaSettingsDescription])
ugtsrsReplicaSettings = Lens.lens (replicaSettings :: UpdateGlobalTableSettingsResponse -> Lude.Maybe [ReplicaSettingsDescription]) (\s a -> s {replicaSettings = a} :: UpdateGlobalTableSettingsResponse)
{-# DEPRECATED ugtsrsReplicaSettings "Use generic-lens or generic-optics with 'replicaSettings' instead." #-}

-- | The name of the global table.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsrsGlobalTableName :: Lens.Lens' UpdateGlobalTableSettingsResponse (Lude.Maybe Lude.Text)
ugtsrsGlobalTableName = Lens.lens (globalTableName :: UpdateGlobalTableSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {globalTableName = a} :: UpdateGlobalTableSettingsResponse)
{-# DEPRECATED ugtsrsGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsrsResponseStatus :: Lens.Lens' UpdateGlobalTableSettingsResponse Lude.Int
ugtsrsResponseStatus = Lens.lens (responseStatus :: UpdateGlobalTableSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGlobalTableSettingsResponse)
{-# DEPRECATED ugtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
