{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
module Network.AWS.DMS.ModifyReplicationInstance
    (
    -- * Creating a request
      ModifyReplicationInstance (..)
    , mkModifyReplicationInstance
    -- ** Request lenses
    , mriReplicationInstanceArn
    , mriAllocatedStorage
    , mriAllowMajorVersionUpgrade
    , mriApplyImmediately
    , mriAutoMinorVersionUpgrade
    , mriEngineVersion
    , mriMultiAZ
    , mriPreferredMaintenanceWindow
    , mriReplicationInstanceClass
    , mriReplicationInstanceIdentifier
    , mriVpcSecurityGroupIds

    -- * Destructuring the response
    , ModifyReplicationInstanceResponse (..)
    , mkModifyReplicationInstanceResponse
    -- ** Response lenses
    , mrirrsReplicationInstance
    , mrirrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyReplicationInstance' smart constructor.
data ModifyReplicationInstance = ModifyReplicationInstance'
  { replicationInstanceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication instance.
  , allocatedStorage :: Core.Maybe Core.Int
    -- ^ The amount of storage (in gigabytes) to be allocated for the replication instance.
  , allowMajorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ Indicates that major version upgrades are allowed. Changing this parameter does not result in an outage, and the change is asynchronously applied as soon as possible.
--
-- This parameter must be set to @true@ when specifying a value for the @EngineVersion@ parameter that is a different major version than the replication instance's current version.
  , applyImmediately :: Core.Maybe Core.Bool
    -- ^ Indicates whether the changes should be applied immediately or during the next maintenance window.
  , autoMinorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ A value that indicates that minor version upgrades are applied automatically to the replication instance during the maintenance window. Changing this parameter doesn't result in an outage, except in the case dsecribed following. The change is asynchronously applied as soon as possible. 
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
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The engine version number of the replication instance.
--
-- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
  , multiAZ :: Core.Maybe Core.Bool
    -- ^ Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ . 
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter does not result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
-- Format: ddd:hh24:mi-ddd:hh24:mi
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes
  , replicationInstanceClass :: Core.Maybe Core.Text
    -- ^ The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> . 
  , replicationInstanceIdentifier :: Core.Maybe Core.Text
    -- ^ The replication instance identifier. This parameter is stored as a lowercase string.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReplicationInstance' value with any optional fields omitted.
mkModifyReplicationInstance
    :: Core.Text -- ^ 'replicationInstanceArn'
    -> ModifyReplicationInstance
mkModifyReplicationInstance replicationInstanceArn
  = ModifyReplicationInstance'{replicationInstanceArn,
                               allocatedStorage = Core.Nothing,
                               allowMajorVersionUpgrade = Core.Nothing,
                               applyImmediately = Core.Nothing,
                               autoMinorVersionUpgrade = Core.Nothing,
                               engineVersion = Core.Nothing, multiAZ = Core.Nothing,
                               preferredMaintenanceWindow = Core.Nothing,
                               replicationInstanceClass = Core.Nothing,
                               replicationInstanceIdentifier = Core.Nothing,
                               vpcSecurityGroupIds = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReplicationInstanceArn :: Lens.Lens' ModifyReplicationInstance Core.Text
mriReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE mriReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

-- | The amount of storage (in gigabytes) to be allocated for the replication instance.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriAllocatedStorage :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Int)
mriAllocatedStorage = Lens.field @"allocatedStorage"
{-# INLINEABLE mriAllocatedStorage #-}
{-# DEPRECATED allocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead"  #-}

-- | Indicates that major version upgrades are allowed. Changing this parameter does not result in an outage, and the change is asynchronously applied as soon as possible.
--
-- This parameter must be set to @true@ when specifying a value for the @EngineVersion@ parameter that is a different major version than the replication instance's current version.
--
-- /Note:/ Consider using 'allowMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriAllowMajorVersionUpgrade :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Bool)
mriAllowMajorVersionUpgrade = Lens.field @"allowMajorVersionUpgrade"
{-# INLINEABLE mriAllowMajorVersionUpgrade #-}
{-# DEPRECATED allowMajorVersionUpgrade "Use generic-lens or generic-optics with 'allowMajorVersionUpgrade' instead"  #-}

-- | Indicates whether the changes should be applied immediately or during the next maintenance window.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriApplyImmediately :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Bool)
mriApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE mriApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

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
{-# INLINEABLE mriAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

-- | The engine version number of the replication instance.
--
-- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriEngineVersion :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Text)
mriEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE mriEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ . 
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriMultiAZ :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Bool)
mriMultiAZ = Lens.field @"multiAZ"
{-# INLINEABLE mriMultiAZ #-}
{-# DEPRECATED multiAZ "Use generic-lens or generic-optics with 'multiAZ' instead"  #-}

-- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter does not result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
-- Format: ddd:hh24:mi-ddd:hh24:mi
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriPreferredMaintenanceWindow :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Text)
mriPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE mriPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> . 
--
-- /Note:/ Consider using 'replicationInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReplicationInstanceClass :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Text)
mriReplicationInstanceClass = Lens.field @"replicationInstanceClass"
{-# INLINEABLE mriReplicationInstanceClass #-}
{-# DEPRECATED replicationInstanceClass "Use generic-lens or generic-optics with 'replicationInstanceClass' instead"  #-}

-- | The replication instance identifier. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'replicationInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReplicationInstanceIdentifier :: Lens.Lens' ModifyReplicationInstance (Core.Maybe Core.Text)
mriReplicationInstanceIdentifier = Lens.field @"replicationInstanceIdentifier"
{-# INLINEABLE mriReplicationInstanceIdentifier #-}
{-# DEPRECATED replicationInstanceIdentifier "Use generic-lens or generic-optics with 'replicationInstanceIdentifier' instead"  #-}

-- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance. 
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriVpcSecurityGroupIds :: Lens.Lens' ModifyReplicationInstance (Core.Maybe [Core.Text])
mriVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE mriVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery ModifyReplicationInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyReplicationInstance where
        toHeaders ModifyReplicationInstance{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.ModifyReplicationInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyReplicationInstance where
        toJSON ModifyReplicationInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReplicationInstanceArn" Core..= replicationInstanceArn),
                  ("AllocatedStorage" Core..=) Core.<$> allocatedStorage,
                  ("AllowMajorVersionUpgrade" Core..=) Core.<$>
                    allowMajorVersionUpgrade,
                  ("ApplyImmediately" Core..=) Core.<$> applyImmediately,
                  ("AutoMinorVersionUpgrade" Core..=) Core.<$>
                    autoMinorVersionUpgrade,
                  ("EngineVersion" Core..=) Core.<$> engineVersion,
                  ("MultiAZ" Core..=) Core.<$> multiAZ,
                  ("PreferredMaintenanceWindow" Core..=) Core.<$>
                    preferredMaintenanceWindow,
                  ("ReplicationInstanceClass" Core..=) Core.<$>
                    replicationInstanceClass,
                  ("ReplicationInstanceIdentifier" Core..=) Core.<$>
                    replicationInstanceIdentifier,
                  ("VpcSecurityGroupIds" Core..=) Core.<$> vpcSecurityGroupIds])

instance Core.AWSRequest ModifyReplicationInstance where
        type Rs ModifyReplicationInstance =
             ModifyReplicationInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ModifyReplicationInstanceResponse' Core.<$>
                   (x Core..:? "ReplicationInstance") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkModifyReplicationInstanceResponse' smart constructor.
data ModifyReplicationInstanceResponse = ModifyReplicationInstanceResponse'
  { replicationInstance :: Core.Maybe Types.ReplicationInstance
    -- ^ The modified replication instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyReplicationInstanceResponse' value with any optional fields omitted.
mkModifyReplicationInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyReplicationInstanceResponse
mkModifyReplicationInstanceResponse responseStatus
  = ModifyReplicationInstanceResponse'{replicationInstance =
                                         Core.Nothing,
                                       responseStatus}

-- | The modified replication instance.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirrsReplicationInstance :: Lens.Lens' ModifyReplicationInstanceResponse (Core.Maybe Types.ReplicationInstance)
mrirrsReplicationInstance = Lens.field @"replicationInstance"
{-# INLINEABLE mrirrsReplicationInstance #-}
{-# DEPRECATED replicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirrsResponseStatus :: Lens.Lens' ModifyReplicationInstanceResponse Core.Int
mrirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mrirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
