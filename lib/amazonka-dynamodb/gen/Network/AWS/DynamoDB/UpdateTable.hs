{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    utTableName,
    utAttributeDefinitions,
    utBillingMode,
    utGlobalSecondaryIndexUpdates,
    utProvisionedThroughput,
    utReplicaUpdates,
    utSSESpecification,
    utStreamSpecification,

    -- * Destructuring the response
    UpdateTableResponse (..),
    mkUpdateTableResponse,

    -- ** Response lenses
    utrrsTableDescription,
    utrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdateTable@ operation.
--
-- /See:/ 'mkUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { -- | The name of the table to be updated.
    tableName :: Types.TableName,
    -- | An array of attributes that describe the key schema for the table and indexes. If you are adding a new global secondary index to the table, @AttributeDefinitions@ must include the key element(s) of the new index.
    attributeDefinitions :: Core.Maybe [Types.AttributeDefinition],
    -- | Controls how you are charged for read and write throughput and how you manage capacity. When switching from pay-per-request to provisioned capacity, initial provisioned capacity values must be set. The initial provisioned capacity values are estimated based on the consumed read and write capacity of your table and global secondary indexes over the past 30 minutes.
    --
    --
    --     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
    --
    --
    --     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
    billingMode :: Core.Maybe Types.BillingMode,
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
    globalSecondaryIndexUpdates :: Core.Maybe [Types.GlobalSecondaryIndexUpdate],
    -- | The new provisioned throughput settings for the specified table or index.
    provisionedThroughput :: Core.Maybe Types.ProvisionedThroughput,
    -- | A list of replica update actions (create, delete, or update) for the table.
    replicaUpdates :: Core.Maybe (Core.NonEmpty Types.ReplicationGroupUpdate),
    -- | The new server-side encryption settings for the specified table.
    sSESpecification :: Core.Maybe Types.SSESpecification,
    -- | Represents the DynamoDB Streams configuration for the table.
    streamSpecification :: Core.Maybe Types.StreamSpecification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTable' value with any optional fields omitted.
mkUpdateTable ::
  -- | 'tableName'
  Types.TableName ->
  UpdateTable
mkUpdateTable tableName =
  UpdateTable'
    { tableName,
      attributeDefinitions = Core.Nothing,
      billingMode = Core.Nothing,
      globalSecondaryIndexUpdates = Core.Nothing,
      provisionedThroughput = Core.Nothing,
      replicaUpdates = Core.Nothing,
      sSESpecification = Core.Nothing,
      streamSpecification = Core.Nothing
    }

-- | The name of the table to be updated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTableName :: Lens.Lens' UpdateTable Types.TableName
utTableName = Lens.field @"tableName"
{-# DEPRECATED utTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | An array of attributes that describe the key schema for the table and indexes. If you are adding a new global secondary index to the table, @AttributeDefinitions@ must include the key element(s) of the new index.
--
-- /Note:/ Consider using 'attributeDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utAttributeDefinitions :: Lens.Lens' UpdateTable (Core.Maybe [Types.AttributeDefinition])
utAttributeDefinitions = Lens.field @"attributeDefinitions"
{-# DEPRECATED utAttributeDefinitions "Use generic-lens or generic-optics with 'attributeDefinitions' instead." #-}

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
utBillingMode :: Lens.Lens' UpdateTable (Core.Maybe Types.BillingMode)
utBillingMode = Lens.field @"billingMode"
{-# DEPRECATED utBillingMode "Use generic-lens or generic-optics with 'billingMode' instead." #-}

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
utGlobalSecondaryIndexUpdates :: Lens.Lens' UpdateTable (Core.Maybe [Types.GlobalSecondaryIndexUpdate])
utGlobalSecondaryIndexUpdates = Lens.field @"globalSecondaryIndexUpdates"
{-# DEPRECATED utGlobalSecondaryIndexUpdates "Use generic-lens or generic-optics with 'globalSecondaryIndexUpdates' instead." #-}

-- | The new provisioned throughput settings for the specified table or index.
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utProvisionedThroughput :: Lens.Lens' UpdateTable (Core.Maybe Types.ProvisionedThroughput)
utProvisionedThroughput = Lens.field @"provisionedThroughput"
{-# DEPRECATED utProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

-- | A list of replica update actions (create, delete, or update) for the table.
--
-- /Note:/ Consider using 'replicaUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utReplicaUpdates :: Lens.Lens' UpdateTable (Core.Maybe (Core.NonEmpty Types.ReplicationGroupUpdate))
utReplicaUpdates = Lens.field @"replicaUpdates"
{-# DEPRECATED utReplicaUpdates "Use generic-lens or generic-optics with 'replicaUpdates' instead." #-}

-- | The new server-side encryption settings for the specified table.
--
-- /Note:/ Consider using 'sSESpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSSESpecification :: Lens.Lens' UpdateTable (Core.Maybe Types.SSESpecification)
utSSESpecification = Lens.field @"sSESpecification"
{-# DEPRECATED utSSESpecification "Use generic-lens or generic-optics with 'sSESpecification' instead." #-}

-- | Represents the DynamoDB Streams configuration for the table.
--
-- /Note:/ Consider using 'streamSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utStreamSpecification :: Lens.Lens' UpdateTable (Core.Maybe Types.StreamSpecification)
utStreamSpecification = Lens.field @"streamSpecification"
{-# DEPRECATED utStreamSpecification "Use generic-lens or generic-optics with 'streamSpecification' instead." #-}

instance Core.FromJSON UpdateTable where
  toJSON UpdateTable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TableName" Core..= tableName),
            ("AttributeDefinitions" Core..=) Core.<$> attributeDefinitions,
            ("BillingMode" Core..=) Core.<$> billingMode,
            ("GlobalSecondaryIndexUpdates" Core..=)
              Core.<$> globalSecondaryIndexUpdates,
            ("ProvisionedThroughput" Core..=) Core.<$> provisionedThroughput,
            ("ReplicaUpdates" Core..=) Core.<$> replicaUpdates,
            ("SSESpecification" Core..=) Core.<$> sSESpecification,
            ("StreamSpecification" Core..=) Core.<$> streamSpecification
          ]
      )

instance Core.AWSRequest UpdateTable where
  type Rs UpdateTable = UpdateTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.UpdateTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTableResponse'
            Core.<$> (x Core..:? "TableDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of an @UpdateTable@ operation.
--
-- /See:/ 'mkUpdateTableResponse' smart constructor.
data UpdateTableResponse = UpdateTableResponse'
  { -- | Represents the properties of the table.
    tableDescription :: Core.Maybe Types.TableDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateTableResponse' value with any optional fields omitted.
mkUpdateTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTableResponse
mkUpdateTableResponse responseStatus =
  UpdateTableResponse'
    { tableDescription = Core.Nothing,
      responseStatus
    }

-- | Represents the properties of the table.
--
-- /Note:/ Consider using 'tableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsTableDescription :: Lens.Lens' UpdateTableResponse (Core.Maybe Types.TableDescription)
utrrsTableDescription = Lens.field @"tableDescription"
{-# DEPRECATED utrrsTableDescription "Use generic-lens or generic-optics with 'tableDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsResponseStatus :: Lens.Lens' UpdateTableResponse Core.Int
utrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
