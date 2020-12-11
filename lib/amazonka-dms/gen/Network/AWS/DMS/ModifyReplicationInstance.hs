{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    mriEngineVersion,
    mriAutoMinorVersionUpgrade,
    mriAllowMajorVersionUpgrade,
    mriPreferredMaintenanceWindow,
    mriVPCSecurityGroupIds,
    mriMultiAZ,
    mriAllocatedStorage,
    mriApplyImmediately,
    mriReplicationInstanceClass,
    mriReplicationInstanceIdentifier,
    mriReplicationInstanceARN,

    -- * Destructuring the response
    ModifyReplicationInstanceResponse (..),
    mkModifyReplicationInstanceResponse,

    -- ** Response lenses
    mrirsReplicationInstance,
    mrirsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyReplicationInstance' smart constructor.
data ModifyReplicationInstance = ModifyReplicationInstance'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    autoMinorVersionUpgrade ::
      Lude.Maybe Lude.Bool,
    allowMajorVersionUpgrade ::
      Lude.Maybe Lude.Bool,
    preferredMaintenanceWindow ::
      Lude.Maybe Lude.Text,
    vpcSecurityGroupIds ::
      Lude.Maybe [Lude.Text],
    multiAZ :: Lude.Maybe Lude.Bool,
    allocatedStorage :: Lude.Maybe Lude.Int,
    applyImmediately ::
      Lude.Maybe Lude.Bool,
    replicationInstanceClass ::
      Lude.Maybe Lude.Text,
    replicationInstanceIdentifier ::
      Lude.Maybe Lude.Text,
    replicationInstanceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReplicationInstance' with the minimum fields required to make a request.
--
-- * 'allocatedStorage' - The amount of storage (in gigabytes) to be allocated for the replication instance.
-- * 'allowMajorVersionUpgrade' - Indicates that major version upgrades are allowed. Changing this parameter does not result in an outage, and the change is asynchronously applied as soon as possible.
--
-- This parameter must be set to @true@ when specifying a value for the @EngineVersion@ parameter that is a different major version than the replication instance's current version.
-- * 'applyImmediately' - Indicates whether the changes should be applied immediately or during the next maintenance window.
-- * 'autoMinorVersionUpgrade' - A value that indicates that minor version upgrades are applied automatically to the replication instance during the maintenance window. Changing this parameter doesn't result in an outage, except in the case dsecribed following. The change is asynchronously applied as soon as possible.
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
-- * 'engineVersion' - The engine version number of the replication instance.
--
-- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
-- * 'multiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
-- * 'preferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter does not result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
-- Format: ddd:hh24:mi-ddd:hh24:mi
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
-- * 'replicationInstanceClass' - The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
-- * 'replicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a lowercase string.
-- * 'vpcSecurityGroupIds' - Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
mkModifyReplicationInstance ::
  -- | 'replicationInstanceARN'
  Lude.Text ->
  ModifyReplicationInstance
mkModifyReplicationInstance pReplicationInstanceARN_ =
  ModifyReplicationInstance'
    { engineVersion = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      allowMajorVersionUpgrade = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      vpcSecurityGroupIds = Lude.Nothing,
      multiAZ = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      applyImmediately = Lude.Nothing,
      replicationInstanceClass = Lude.Nothing,
      replicationInstanceIdentifier = Lude.Nothing,
      replicationInstanceARN = pReplicationInstanceARN_
    }

-- | The engine version number of the replication instance.
--
-- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriEngineVersion :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe Lude.Text)
mriEngineVersion = Lens.lens (engineVersion :: ModifyReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

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
mriAutoMinorVersionUpgrade :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe Lude.Bool)
mriAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: ModifyReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | Indicates that major version upgrades are allowed. Changing this parameter does not result in an outage, and the change is asynchronously applied as soon as possible.
--
-- This parameter must be set to @true@ when specifying a value for the @EngineVersion@ parameter that is a different major version than the replication instance's current version.
--
-- /Note:/ Consider using 'allowMajorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriAllowMajorVersionUpgrade :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe Lude.Bool)
mriAllowMajorVersionUpgrade = Lens.lens (allowMajorVersionUpgrade :: ModifyReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {allowMajorVersionUpgrade = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriAllowMajorVersionUpgrade "Use generic-lens or generic-optics with 'allowMajorVersionUpgrade' instead." #-}

-- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter does not result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
-- Format: ddd:hh24:mi-ddd:hh24:mi
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Constraints: Must be at least 30 minutes
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriPreferredMaintenanceWindow :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe Lude.Text)
mriPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: ModifyReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriVPCSecurityGroupIds :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe [Lude.Text])
mriVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: ModifyReplicationInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriMultiAZ :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe Lude.Bool)
mriMultiAZ = Lens.lens (multiAZ :: ModifyReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The amount of storage (in gigabytes) to be allocated for the replication instance.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriAllocatedStorage :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe Lude.Int)
mriAllocatedStorage = Lens.lens (allocatedStorage :: ModifyReplicationInstance -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Indicates whether the changes should be applied immediately or during the next maintenance window.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriApplyImmediately :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe Lude.Bool)
mriApplyImmediately = Lens.lens (applyImmediately :: ModifyReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {applyImmediately = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- /Note:/ Consider using 'replicationInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReplicationInstanceClass :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe Lude.Text)
mriReplicationInstanceClass = Lens.lens (replicationInstanceClass :: ModifyReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceClass = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriReplicationInstanceClass "Use generic-lens or generic-optics with 'replicationInstanceClass' instead." #-}

-- | The replication instance identifier. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'replicationInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReplicationInstanceIdentifier :: Lens.Lens' ModifyReplicationInstance (Lude.Maybe Lude.Text)
mriReplicationInstanceIdentifier = Lens.lens (replicationInstanceIdentifier :: ModifyReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceIdentifier = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriReplicationInstanceIdentifier "Use generic-lens or generic-optics with 'replicationInstanceIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReplicationInstanceARN :: Lens.Lens' ModifyReplicationInstance Lude.Text
mriReplicationInstanceARN = Lens.lens (replicationInstanceARN :: ModifyReplicationInstance -> Lude.Text) (\s a -> s {replicationInstanceARN = a} :: ModifyReplicationInstance)
{-# DEPRECATED mriReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

instance Lude.AWSRequest ModifyReplicationInstance where
  type
    Rs ModifyReplicationInstance =
      ModifyReplicationInstanceResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyReplicationInstanceResponse'
            Lude.<$> (x Lude..?> "ReplicationInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyReplicationInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.ModifyReplicationInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyReplicationInstance where
  toJSON ModifyReplicationInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EngineVersion" Lude..=) Lude.<$> engineVersion,
            ("AutoMinorVersionUpgrade" Lude..=)
              Lude.<$> autoMinorVersionUpgrade,
            ("AllowMajorVersionUpgrade" Lude..=)
              Lude.<$> allowMajorVersionUpgrade,
            ("PreferredMaintenanceWindow" Lude..=)
              Lude.<$> preferredMaintenanceWindow,
            ("VpcSecurityGroupIds" Lude..=) Lude.<$> vpcSecurityGroupIds,
            ("MultiAZ" Lude..=) Lude.<$> multiAZ,
            ("AllocatedStorage" Lude..=) Lude.<$> allocatedStorage,
            ("ApplyImmediately" Lude..=) Lude.<$> applyImmediately,
            ("ReplicationInstanceClass" Lude..=)
              Lude.<$> replicationInstanceClass,
            ("ReplicationInstanceIdentifier" Lude..=)
              Lude.<$> replicationInstanceIdentifier,
            Lude.Just
              ("ReplicationInstanceArn" Lude..= replicationInstanceARN)
          ]
      )

instance Lude.ToPath ModifyReplicationInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyReplicationInstance where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkModifyReplicationInstanceResponse' smart constructor.
data ModifyReplicationInstanceResponse = ModifyReplicationInstanceResponse'
  { replicationInstance ::
      Lude.Maybe
        ReplicationInstance,
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

-- | Creates a value of 'ModifyReplicationInstanceResponse' with the minimum fields required to make a request.
--
-- * 'replicationInstance' - The modified replication instance.
-- * 'responseStatus' - The response status code.
mkModifyReplicationInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyReplicationInstanceResponse
mkModifyReplicationInstanceResponse pResponseStatus_ =
  ModifyReplicationInstanceResponse'
    { replicationInstance =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The modified replication instance.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirsReplicationInstance :: Lens.Lens' ModifyReplicationInstanceResponse (Lude.Maybe ReplicationInstance)
mrirsReplicationInstance = Lens.lens (replicationInstance :: ModifyReplicationInstanceResponse -> Lude.Maybe ReplicationInstance) (\s a -> s {replicationInstance = a} :: ModifyReplicationInstanceResponse)
{-# DEPRECATED mrirsReplicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirsResponseStatus :: Lens.Lens' ModifyReplicationInstanceResponse Lude.Int
mrirsResponseStatus = Lens.lens (responseStatus :: ModifyReplicationInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyReplicationInstanceResponse)
{-# DEPRECATED mrirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
