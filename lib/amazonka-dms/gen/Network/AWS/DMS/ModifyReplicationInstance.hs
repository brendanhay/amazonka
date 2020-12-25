{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ModifyReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the replication instance to apply new settings. You can change one or more parameters by specifying these parameters and the new values in the request.
--
-- Some settings are applied during the maintenance window.
module Network.AWS.DMS.ModifyReplicationInstance
  ( -- * Creating a request
    ModifyReplicationInstance (..),
    mkModifyReplicationInstance,

    -- ** Request lenses
    mriReplicationInstanceArn,
    mriAllocatedStorage,
    mriAllowMajorVersionUpgrade,
    mriApplyImmediately,
    mriAutoMinorVersionUpgrade,
    mriEngineVersion,
    mriMultiAZ,
    mriPreferredMaintenanceWindow,
    mriReplicationInstanceClass,
    mriReplicationInstanceIdentifier,
    mriVpcSecurityGroupIds,

    -- * Destructuring the response
    ModifyReplicationInstanceResponse (..),
    mkModifyReplicationInstanceResponse,

    -- ** Response lenses
    mrirrsReplicationInstance,
    mrirrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyReplicationInstance' smart constructor.
data ModifyReplicationInstance = ModifyReplicationInstance'
  { -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Types.String,
    -- | The amount of storage (in gigabytes) to be allocated for the replication instance.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | Indicates that major version upgrades are allowed. Changing this parameter does not result in an outage, and the change is asynchronously applied as soon as possible.
    --
    -- This parameter must be set to @true@ when specifying a value for the @EngineVersion@ parameter that is a different major version than the replication instance's current version.
    allowMajorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | Indicates whether the changes should be applied immediately or during the next maintenance window.
    applyImmediately :: Core.Maybe Core.Bool,
    -- | A value that indicates that minor version upgrades are applied automatically to the replication instance during the maintenance window. Changing this parameter doesn't result in an outage, except in the case dsecribed following. The change is asynchronously applied as soon as possible.
    --
    -- An outage does result if these factors apply:
    --
    --     * This parameter is set to @true@ during the maintenance window.
    --
    --
    --     * A newer minor version is available.
    --
    --
    --     * AWS DMS has enabled automatic patching for the given engine version.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The engine version number of the replication instance.
    --
    -- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
    engineVersion :: Core.Maybe Types.String,
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
    multiAZ :: Core.Maybe Core.Bool,
    -- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter does not result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
    --
    -- Default: Uses existing setting
    -- Format: ddd:hh24:mi-ddd:hh24:mi
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    -- Constraints: Must be at least 30 minutes
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
    --
    -- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
    replicationInstanceClass :: Core.Maybe Types.String,
    -- | The replication instance identifier. This parameter is stored as a lowercase string.
    replicationInstanceIdentifier :: Core.Maybe Types.String,
    -- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReplicationInstance' value with any optional fields omitted.
mkModifyReplicationInstance ::
  -- | 'replicationInstanceArn'
  Types.String ->
  ModifyReplicationInstance
mkModifyReplicationInstance replicationInstanceArn =
  ModifyReplicationInstance'
    { replicationInstanceArn,
      allocatedStorage = Core.Nothing,
      allowMajorVersionUpgrade = Core.Nothing,
      applyImmediately = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      engineVersion = Core.Nothing,
      multiAZ = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      replicationInstanceClass = Core.Nothing,
      replicationInstanceIdentifier = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReplicationInstanceArn :: Lens.Lens' ModifyReplicationInstance Types.String
mriReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# DEPRECATED mriReplicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead." #-}

-- | The amount of storage (in gigabytes) to be allocated for the replication instance.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriAllocatedStorage :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Int)
mriAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED mriAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Indicates that major version upgrades are allowed. Changing this parameter does not result in an outage, and the change is asynchronously applied as soon as possible.
--
-- This parameter must be set to @true@ when specifying a value for the @EngineVersion@ parameter that is a different major version than the replication instance's current version.
--
-- /Note:/ Consider using 'allowMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriAllowMajorVersionUpgrade :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Bool)
mriAllowMajorVersionUpgrade = Lens.field @"allowMajorVersionUpgrade"
{-# DEPRECATED mriAllowMajorVersionUpgrade "Use generic-lens or generic-optics with 'allowMajorVersionUpgrade' instead." #-}

-- | Indicates whether the changes should be applied immediately or during the next maintenance window.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriApplyImmediately :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Bool)
mriApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED mriApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | A value that indicates that minor version upgrades are applied automatically to the replication instance during the maintenance window. Changing this parameter doesn't result in an outage, except in the case dsecribed following. The change is asynchronously applied as soon as possible.
--
-- An outage does result if these factors apply:
--
--     * This parameter is set to @true@ during the maintenance window.
--
--
--     * A newer minor version is available.
--
--
--     * AWS DMS has enabled automatic patching for the given engine version.
--
--
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriAutoMinorVersionUpgrade :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Bool)
mriAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED mriAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The engine version number of the replication instance.
--
-- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriEngineVersion :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Types.String)
mriEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED mriEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriMultiAZ :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Bool)
mriMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED mriMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter does not result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
-- Format: ddd:hh24:mi-ddd:hh24:mi
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriPreferredMaintenanceWindow :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Types.String)
mriPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED mriPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- /Note:/ Consider using 'replicationInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReplicationInstanceClass :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Types.String)
mriReplicationInstanceClass = Lens.field @"replicationInstanceClass"
{-# DEPRECATED mriReplicationInstanceClass "Use generic-lens or generic-optics with 'replicationInstanceClass' instead." #-}

-- | The replication instance identifier. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'replicationInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReplicationInstanceIdentifier :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Types.String)
mriReplicationInstanceIdentifier = Lens.field @"replicationInstanceIdentifier"
{-# DEPRECATED mriReplicationInstanceIdentifier "Use generic-lens or generic-optics with 'replicationInstanceIdentifier' instead." #-}

-- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriVpcSecurityGroupIds :: Lens.Lens' ModifyReplicationInstance (Core.Maybe [Types.String])
mriVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED mriVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.FromJSON ModifyReplicationInstance where
  toJSON ModifyReplicationInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ReplicationInstanceArn" Core..= replicationInstanceArn),
            ("AllocatedStorage" Core..=) Core.<$> allocatedStorage,
            ("AllowMajorVersionUpgrade" Core..=)
              Core.<$> allowMajorVersionUpgrade,
            ("ApplyImmediately" Core..=) Core.<$> applyImmediately,
            ("AutoMinorVersionUpgrade" Core..=)
              Core.<$> autoMinorVersionUpgrade,
            ("EngineVersion" Core..=) Core.<$> engineVersion,
            ("MultiAZ" Core..=) Core.<$> multiAZ,
            ("PreferredMaintenanceWindow" Core..=)
              Core.<$> preferredMaintenanceWindow,
            ("ReplicationInstanceClass" Core..=)
              Core.<$> replicationInstanceClass,
            ("ReplicationInstanceIdentifier" Core..=)
              Core.<$> replicationInstanceIdentifier,
            ("VpcSecurityGroupIds" Core..=) Core.<$> vpcSecurityGroupIds
          ]
      )

instance Core.AWSRequest ModifyReplicationInstance where
  type
    Rs ModifyReplicationInstance =
      ModifyReplicationInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.ModifyReplicationInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyReplicationInstanceResponse'
            Core.<$> (x Core..:? "ReplicationInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkModifyReplicationInstanceResponse' smart constructor.
data ModifyReplicationInstanceResponse = ModifyReplicationInstanceResponse'
  { -- | The modified replication instance.
    replicationInstance :: Core.Maybe Types.ReplicationInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyReplicationInstanceResponse' value with any optional fields omitted.
mkModifyReplicationInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyReplicationInstanceResponse
mkModifyReplicationInstanceResponse responseStatus =
  ModifyReplicationInstanceResponse'
    { replicationInstance =
        Core.Nothing,
      responseStatus
    }

-- | The modified replication instance.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirrsReplicationInstance :: Lens.Lens' ModifyReplicationInstanceResponse (Core.Maybe Types.ReplicationInstance)
mrirrsReplicationInstance = Lens.field @"replicationInstance"
{-# DEPRECATED mrirrsReplicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirrsResponseStatus :: Lens.Lens' ModifyReplicationInstanceResponse Core.Int
mrirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mrirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
