{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the provisioned throughput settings, global secondary indexes, or DynamoDB Streams settings for a given table.
--
-- You can only perform one of the following operations at once:
--
--     * Modify the provisioned throughput settings of the table.
--
--
--     * Enable or disable DynamoDB Streams on the table.
--
--
--     * Remove a global secondary index from the table.
--
--
--     * Create a new global secondary index on the table. After the index begins backfilling, you can use @UpdateTable@ to perform other operations.
--
--
-- @UpdateTable@ is an asynchronous operation; while it is executing, the table status changes from @ACTIVE@ to @UPDATING@ . While it is @UPDATING@ , you cannot issue another @UpdateTable@ request. When the table returns to the @ACTIVE@ state, the @UpdateTable@ operation is complete.
module Network.AWS.DynamoDB.UpdateTable
  ( -- * Creating a request
    UpdateTable (..),
    mkUpdateTable,

    -- ** Request lenses
    utAttributeDefinitions,
    utProvisionedThroughput,
    utSSESpecification,
    utReplicaUpdates,
    utGlobalSecondaryIndexUpdates,
    utBillingMode,
    utStreamSpecification,
    utTableName,

    -- * Destructuring the response
    UpdateTableResponse (..),
    mkUpdateTableResponse,

    -- ** Response lenses
    utrsTableDescription,
    utrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an @UpdateTable@ operation.
--
-- /See:/ 'mkUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { attributeDefinitions ::
      Lude.Maybe [AttributeDefinition],
    provisionedThroughput :: Lude.Maybe ProvisionedThroughput,
    sSESpecification :: Lude.Maybe SSESpecification,
    replicaUpdates ::
      Lude.Maybe (Lude.NonEmpty ReplicationGroupUpdate),
    globalSecondaryIndexUpdates ::
      Lude.Maybe [GlobalSecondaryIndexUpdate],
    billingMode :: Lude.Maybe BillingMode,
    streamSpecification :: Lude.Maybe StreamSpecification,
    tableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTable' with the minimum fields required to make a request.
--
-- * 'attributeDefinitions' - An array of attributes that describe the key schema for the table and indexes. If you are adding a new global secondary index to the table, @AttributeDefinitions@ must include the key element(s) of the new index.
-- * 'billingMode' - Controls how you are charged for read and write throughput and how you manage capacity. When switching from pay-per-request to provisioned capacity, initial provisioned capacity values must be set. The initial provisioned capacity values are estimated based on the consumed read and write capacity of your table and global secondary indexes over the past 30 minutes.
--
--
--     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
--
--
--     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
--
--
-- * 'globalSecondaryIndexUpdates' - An array of one or more global secondary indexes for the table. For each index in the array, you can request one action:
--
--
--     * @Create@ - add a new global secondary index to the table.
--
--
--     * @Update@ - modify the provisioned throughput settings of an existing global secondary index.
--
--
--     * @Delete@ - remove a global secondary index from the table.
--
--
-- You can create or delete only one global secondary index per @UpdateTable@ operation.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing Global Secondary Indexes> in the /Amazon DynamoDB Developer Guide/ .
-- * 'provisionedThroughput' - The new provisioned throughput settings for the specified table or index.
-- * 'replicaUpdates' - A list of replica update actions (create, delete, or update) for the table.
-- * 'sSESpecification' - The new server-side encryption settings for the specified table.
-- * 'streamSpecification' - Represents the DynamoDB Streams configuration for the table.
-- * 'tableName' - The name of the table to be updated.
mkUpdateTable ::
  -- | 'tableName'
  Lude.Text ->
  UpdateTable
mkUpdateTable pTableName_ =
  UpdateTable'
    { attributeDefinitions = Lude.Nothing,
      provisionedThroughput = Lude.Nothing,
      sSESpecification = Lude.Nothing,
      replicaUpdates = Lude.Nothing,
      globalSecondaryIndexUpdates = Lude.Nothing,
      billingMode = Lude.Nothing,
      streamSpecification = Lude.Nothing,
      tableName = pTableName_
    }

-- | An array of attributes that describe the key schema for the table and indexes. If you are adding a new global secondary index to the table, @AttributeDefinitions@ must include the key element(s) of the new index.
--
-- /Note:/ Consider using 'attributeDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utAttributeDefinitions :: Lens.Lens' UpdateTable (Lude.Maybe [AttributeDefinition])
utAttributeDefinitions = Lens.lens (attributeDefinitions :: UpdateTable -> Lude.Maybe [AttributeDefinition]) (\s a -> s {attributeDefinitions = a} :: UpdateTable)
{-# DEPRECATED utAttributeDefinitions "Use generic-lens or generic-optics with 'attributeDefinitions' instead." #-}

-- | The new provisioned throughput settings for the specified table or index.
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utProvisionedThroughput :: Lens.Lens' UpdateTable (Lude.Maybe ProvisionedThroughput)
utProvisionedThroughput = Lens.lens (provisionedThroughput :: UpdateTable -> Lude.Maybe ProvisionedThroughput) (\s a -> s {provisionedThroughput = a} :: UpdateTable)
{-# DEPRECATED utProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

-- | The new server-side encryption settings for the specified table.
--
-- /Note:/ Consider using 'sSESpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSSESpecification :: Lens.Lens' UpdateTable (Lude.Maybe SSESpecification)
utSSESpecification = Lens.lens (sSESpecification :: UpdateTable -> Lude.Maybe SSESpecification) (\s a -> s {sSESpecification = a} :: UpdateTable)
{-# DEPRECATED utSSESpecification "Use generic-lens or generic-optics with 'sSESpecification' instead." #-}

-- | A list of replica update actions (create, delete, or update) for the table.
--
-- /Note:/ Consider using 'replicaUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utReplicaUpdates :: Lens.Lens' UpdateTable (Lude.Maybe (Lude.NonEmpty ReplicationGroupUpdate))
utReplicaUpdates = Lens.lens (replicaUpdates :: UpdateTable -> Lude.Maybe (Lude.NonEmpty ReplicationGroupUpdate)) (\s a -> s {replicaUpdates = a} :: UpdateTable)
{-# DEPRECATED utReplicaUpdates "Use generic-lens or generic-optics with 'replicaUpdates' instead." #-}

-- | An array of one or more global secondary indexes for the table. For each index in the array, you can request one action:
--
--
--     * @Create@ - add a new global secondary index to the table.
--
--
--     * @Update@ - modify the provisioned throughput settings of an existing global secondary index.
--
--
--     * @Delete@ - remove a global secondary index from the table.
--
--
-- You can create or delete only one global secondary index per @UpdateTable@ operation.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing Global Secondary Indexes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'globalSecondaryIndexUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utGlobalSecondaryIndexUpdates :: Lens.Lens' UpdateTable (Lude.Maybe [GlobalSecondaryIndexUpdate])
utGlobalSecondaryIndexUpdates = Lens.lens (globalSecondaryIndexUpdates :: UpdateTable -> Lude.Maybe [GlobalSecondaryIndexUpdate]) (\s a -> s {globalSecondaryIndexUpdates = a} :: UpdateTable)
{-# DEPRECATED utGlobalSecondaryIndexUpdates "Use generic-lens or generic-optics with 'globalSecondaryIndexUpdates' instead." #-}

-- | Controls how you are charged for read and write throughput and how you manage capacity. When switching from pay-per-request to provisioned capacity, initial provisioned capacity values must be set. The initial provisioned capacity values are estimated based on the consumed read and write capacity of your table and global secondary indexes over the past 30 minutes.
--
--
--     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
--
--
--     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
--
--
--
-- /Note:/ Consider using 'billingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utBillingMode :: Lens.Lens' UpdateTable (Lude.Maybe BillingMode)
utBillingMode = Lens.lens (billingMode :: UpdateTable -> Lude.Maybe BillingMode) (\s a -> s {billingMode = a} :: UpdateTable)
{-# DEPRECATED utBillingMode "Use generic-lens or generic-optics with 'billingMode' instead." #-}

-- | Represents the DynamoDB Streams configuration for the table.
--
-- /Note:/ Consider using 'streamSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utStreamSpecification :: Lens.Lens' UpdateTable (Lude.Maybe StreamSpecification)
utStreamSpecification = Lens.lens (streamSpecification :: UpdateTable -> Lude.Maybe StreamSpecification) (\s a -> s {streamSpecification = a} :: UpdateTable)
{-# DEPRECATED utStreamSpecification "Use generic-lens or generic-optics with 'streamSpecification' instead." #-}

-- | The name of the table to be updated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTableName :: Lens.Lens' UpdateTable Lude.Text
utTableName = Lens.lens (tableName :: UpdateTable -> Lude.Text) (\s a -> s {tableName = a} :: UpdateTable)
{-# DEPRECATED utTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest UpdateTable where
  type Rs UpdateTable = UpdateTableResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTableResponse'
            Lude.<$> (x Lude..?> "TableDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTable where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.UpdateTable" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTable where
  toJSON UpdateTable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AttributeDefinitions" Lude..=) Lude.<$> attributeDefinitions,
            ("ProvisionedThroughput" Lude..=) Lude.<$> provisionedThroughput,
            ("SSESpecification" Lude..=) Lude.<$> sSESpecification,
            ("ReplicaUpdates" Lude..=) Lude.<$> replicaUpdates,
            ("GlobalSecondaryIndexUpdates" Lude..=)
              Lude.<$> globalSecondaryIndexUpdates,
            ("BillingMode" Lude..=) Lude.<$> billingMode,
            ("StreamSpecification" Lude..=) Lude.<$> streamSpecification,
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath UpdateTable where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTable where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an @UpdateTable@ operation.
--
-- /See:/ 'mkUpdateTableResponse' smart constructor.
data UpdateTableResponse = UpdateTableResponse'
  { tableDescription ::
      Lude.Maybe TableDescription,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tableDescription' - Represents the properties of the table.
mkUpdateTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTableResponse
mkUpdateTableResponse pResponseStatus_ =
  UpdateTableResponse'
    { tableDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the properties of the table.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsTableDescription :: Lens.Lens' UpdateTableResponse (Lude.Maybe TableDescription)
utrsTableDescription = Lens.lens (tableDescription :: UpdateTableResponse -> Lude.Maybe TableDescription) (\s a -> s {tableDescription = a} :: UpdateTableResponse)
{-# DEPRECATED utrsTableDescription "Use generic-lens or generic-optics with 'tableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsResponseStatus :: Lens.Lens' UpdateTableResponse Lude.Int
utrsResponseStatus = Lens.lens (responseStatus :: UpdateTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTableResponse)
{-# DEPRECATED utrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
