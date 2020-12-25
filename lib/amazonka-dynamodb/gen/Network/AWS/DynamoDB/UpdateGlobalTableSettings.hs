{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ugtsGlobalTableName,
    ugtsGlobalTableBillingMode,
    ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate,
    ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
    ugtsGlobalTableProvisionedWriteCapacityUnits,
    ugtsReplicaSettingsUpdate,

    -- * Destructuring the response
    UpdateGlobalTableSettingsResponse (..),
    mkUpdateGlobalTableSettingsResponse,

    -- ** Response lenses
    ugtsrrsGlobalTableName,
    ugtsrrsReplicaSettings,
    ugtsrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGlobalTableSettings' smart constructor.
data UpdateGlobalTableSettings = UpdateGlobalTableSettings'
  { -- | The name of the global table
    globalTableName :: Types.GlobalTableName,
    -- | The billing mode of the global table. If @GlobalTableBillingMode@ is not specified, the global table defaults to @PROVISIONED@ capacity billing mode.
    --
    --
    --     * @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable workloads. @PROVISIONED@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode> .
    --
    --
    --     * @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode> .
    globalTableBillingMode :: Core.Maybe Types.BillingMode,
    -- | Represents the settings of a global secondary index for a global table that will be modified.
    globalTableGlobalSecondaryIndexSettingsUpdate :: Core.Maybe (Core.NonEmpty Types.GlobalTableGlobalSecondaryIndexSettingsUpdate),
    -- | Auto scaling settings for managing provisioned write capacity for the global table.
    globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate :: Core.Maybe Types.AutoScalingSettingsUpdate,
    -- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
    globalTableProvisionedWriteCapacityUnits :: Core.Maybe Core.Natural,
    -- | Represents the settings for a global table in a Region that will be modified.
    replicaSettingsUpdate :: Core.Maybe (Core.NonEmpty Types.ReplicaSettingsUpdate)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGlobalTableSettings' value with any optional fields omitted.
mkUpdateGlobalTableSettings ::
  -- | 'globalTableName'
  Types.GlobalTableName ->
  UpdateGlobalTableSettings
mkUpdateGlobalTableSettings globalTableName =
  UpdateGlobalTableSettings'
    { globalTableName,
      globalTableBillingMode = Core.Nothing,
      globalTableGlobalSecondaryIndexSettingsUpdate = Core.Nothing,
      globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate =
        Core.Nothing,
      globalTableProvisionedWriteCapacityUnits = Core.Nothing,
      replicaSettingsUpdate = Core.Nothing
    }

-- | The name of the global table
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsGlobalTableName :: Lens.Lens' UpdateGlobalTableSettings Types.GlobalTableName
ugtsGlobalTableName = Lens.field @"globalTableName"
{-# DEPRECATED ugtsGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

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
ugtsGlobalTableBillingMode :: Lens.Lens' UpdateGlobalTableSettings (Core.Maybe Types.BillingMode)
ugtsGlobalTableBillingMode = Lens.field @"globalTableBillingMode"
{-# DEPRECATED ugtsGlobalTableBillingMode "Use generic-lens or generic-optics with 'globalTableBillingMode' instead." #-}

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
-- /Note:/ Consider using 'globalTableGlobalSecondaryIndexSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Core.Maybe (Core.NonEmpty Types.GlobalTableGlobalSecondaryIndexSettingsUpdate))
ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate = Lens.field @"globalTableGlobalSecondaryIndexSettingsUpdate"
{-# DEPRECATED ugtsGlobalTableGlobalSecondaryIndexSettingsUpdate "Use generic-lens or generic-optics with 'globalTableGlobalSecondaryIndexSettingsUpdate' instead." #-}

-- | Auto scaling settings for managing provisioned write capacity for the global table.
--
-- /Note:/ Consider using 'globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Core.Maybe Types.AutoScalingSettingsUpdate)
ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate = Lens.field @"globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate"
{-# DEPRECATED ugtsGlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate "Use generic-lens or generic-optics with 'globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate' instead." #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
--
-- /Note:/ Consider using 'globalTableProvisionedWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsGlobalTableProvisionedWriteCapacityUnits :: Lens.Lens' UpdateGlobalTableSettings (Core.Maybe Core.Natural)
ugtsGlobalTableProvisionedWriteCapacityUnits = Lens.field @"globalTableProvisionedWriteCapacityUnits"
{-# DEPRECATED ugtsGlobalTableProvisionedWriteCapacityUnits "Use generic-lens or generic-optics with 'globalTableProvisionedWriteCapacityUnits' instead." #-}

-- | Represents the settings for a global table in a Region that will be modified.
--
-- /Note:/ Consider using 'replicaSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsReplicaSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Core.Maybe (Core.NonEmpty Types.ReplicaSettingsUpdate))
ugtsReplicaSettingsUpdate = Lens.field @"replicaSettingsUpdate"
{-# DEPRECATED ugtsReplicaSettingsUpdate "Use generic-lens or generic-optics with 'replicaSettingsUpdate' instead." #-}

instance Core.FromJSON UpdateGlobalTableSettings where
  toJSON UpdateGlobalTableSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GlobalTableName" Core..= globalTableName),
            ("GlobalTableBillingMode" Core..=) Core.<$> globalTableBillingMode,
            ("GlobalTableGlobalSecondaryIndexSettingsUpdate" Core..=)
              Core.<$> globalTableGlobalSecondaryIndexSettingsUpdate,
            ( "GlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate"
                Core..=
            )
              Core.<$> globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
            ("GlobalTableProvisionedWriteCapacityUnits" Core..=)
              Core.<$> globalTableProvisionedWriteCapacityUnits,
            ("ReplicaSettingsUpdate" Core..=) Core.<$> replicaSettingsUpdate
          ]
      )

instance Core.AWSRequest UpdateGlobalTableSettings where
  type
    Rs UpdateGlobalTableSettings =
      UpdateGlobalTableSettingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DynamoDB_20120810.UpdateGlobalTableSettings")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGlobalTableSettingsResponse'
            Core.<$> (x Core..:? "GlobalTableName")
            Core.<*> (x Core..:? "ReplicaSettings")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateGlobalTableSettingsResponse' smart constructor.
data UpdateGlobalTableSettingsResponse = UpdateGlobalTableSettingsResponse'
  { -- | The name of the global table.
    globalTableName :: Core.Maybe Types.GlobalTableName,
    -- | The Region-specific settings for the global table.
    replicaSettings :: Core.Maybe [Types.ReplicaSettingsDescription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateGlobalTableSettingsResponse' value with any optional fields omitted.
mkUpdateGlobalTableSettingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGlobalTableSettingsResponse
mkUpdateGlobalTableSettingsResponse responseStatus =
  UpdateGlobalTableSettingsResponse'
    { globalTableName =
        Core.Nothing,
      replicaSettings = Core.Nothing,
      responseStatus
    }

-- | The name of the global table.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsrrsGlobalTableName :: Lens.Lens' UpdateGlobalTableSettingsResponse (Core.Maybe Types.GlobalTableName)
ugtsrrsGlobalTableName = Lens.field @"globalTableName"
{-# DEPRECATED ugtsrrsGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

-- | The Region-specific settings for the global table.
--
-- /Note:/ Consider using 'replicaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsrrsReplicaSettings :: Lens.Lens' UpdateGlobalTableSettingsResponse (Core.Maybe [Types.ReplicaSettingsDescription])
ugtsrrsReplicaSettings = Lens.field @"replicaSettings"
{-# DEPRECATED ugtsrrsReplicaSettings "Use generic-lens or generic-optics with 'replicaSettings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugtsrrsResponseStatus :: Lens.Lens' UpdateGlobalTableSettingsResponse Core.Int
ugtsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ugtsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
