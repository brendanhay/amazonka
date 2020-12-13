{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.CreateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.CreateCluster
  ( -- * Creating a request
    CreateCluster (..),
    mkCreateCluster,

    -- ** Request lenses
    ccBackupRetentionPolicy,
    ccSubnetIds,
    ccTagList,
    ccSourceBackupId,
    ccHSMType,

    -- * Destructuring the response
    CreateClusterResponse (..),
    mkCreateClusterResponse,

    -- ** Response lenses
    ccrsCluster,
    ccrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | A policy that defines how the service retains backups.
    backupRetentionPolicy :: Lude.Maybe BackupRetentionPolicy,
    -- | The identifiers (IDs) of the subnets where you are creating the cluster. You must specify at least one subnet. If you specify multiple subnets, they must meet the following criteria:
    --
    --
    --     * All subnets must be in the same virtual private cloud (VPC).
    --
    --
    --     * You can specify only one subnet per Availability Zone.
    subnetIds :: Lude.NonEmpty Lude.Text,
    -- | Tags to apply to the CloudHSM cluster during creation.
    tagList :: Lude.Maybe [Tag],
    -- | The identifier (ID) of the cluster backup to restore. Use this value to restore the cluster from a backup instead of creating a new cluster. To find the backup ID, use 'DescribeBackups' .
    sourceBackupId :: Lude.Maybe Lude.Text,
    -- | The type of HSM to use in the cluster. Currently the only allowed value is @hsm1.medium@ .
    hsmType :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- * 'backupRetentionPolicy' - A policy that defines how the service retains backups.
-- * 'subnetIds' - The identifiers (IDs) of the subnets where you are creating the cluster. You must specify at least one subnet. If you specify multiple subnets, they must meet the following criteria:
--
--
--     * All subnets must be in the same virtual private cloud (VPC).
--
--
--     * You can specify only one subnet per Availability Zone.
--
--
-- * 'tagList' - Tags to apply to the CloudHSM cluster during creation.
-- * 'sourceBackupId' - The identifier (ID) of the cluster backup to restore. Use this value to restore the cluster from a backup instead of creating a new cluster. To find the backup ID, use 'DescribeBackups' .
-- * 'hsmType' - The type of HSM to use in the cluster. Currently the only allowed value is @hsm1.medium@ .
mkCreateCluster ::
  -- | 'subnetIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'hsmType'
  Lude.Text ->
  CreateCluster
mkCreateCluster pSubnetIds_ pHSMType_ =
  CreateCluster'
    { backupRetentionPolicy = Lude.Nothing,
      subnetIds = pSubnetIds_,
      tagList = Lude.Nothing,
      sourceBackupId = Lude.Nothing,
      hsmType = pHSMType_
    }

-- | A policy that defines how the service retains backups.
--
-- /Note:/ Consider using 'backupRetentionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccBackupRetentionPolicy :: Lens.Lens' CreateCluster (Lude.Maybe BackupRetentionPolicy)
ccBackupRetentionPolicy = Lens.lens (backupRetentionPolicy :: CreateCluster -> Lude.Maybe BackupRetentionPolicy) (\s a -> s {backupRetentionPolicy = a} :: CreateCluster)
{-# DEPRECATED ccBackupRetentionPolicy "Use generic-lens or generic-optics with 'backupRetentionPolicy' instead." #-}

-- | The identifiers (IDs) of the subnets where you are creating the cluster. You must specify at least one subnet. If you specify multiple subnets, they must meet the following criteria:
--
--
--     * All subnets must be in the same virtual private cloud (VPC).
--
--
--     * You can specify only one subnet per Availability Zone.
--
--
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSubnetIds :: Lens.Lens' CreateCluster (Lude.NonEmpty Lude.Text)
ccSubnetIds = Lens.lens (subnetIds :: CreateCluster -> Lude.NonEmpty Lude.Text) (\s a -> s {subnetIds = a} :: CreateCluster)
{-# DEPRECATED ccSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | Tags to apply to the CloudHSM cluster during creation.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTagList :: Lens.Lens' CreateCluster (Lude.Maybe [Tag])
ccTagList = Lens.lens (tagList :: CreateCluster -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: CreateCluster)
{-# DEPRECATED ccTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The identifier (ID) of the cluster backup to restore. Use this value to restore the cluster from a backup instead of creating a new cluster. To find the backup ID, use 'DescribeBackups' .
--
-- /Note:/ Consider using 'sourceBackupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSourceBackupId :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccSourceBackupId = Lens.lens (sourceBackupId :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {sourceBackupId = a} :: CreateCluster)
{-# DEPRECATED ccSourceBackupId "Use generic-lens or generic-optics with 'sourceBackupId' instead." #-}

-- | The type of HSM to use in the cluster. Currently the only allowed value is @hsm1.medium@ .
--
-- /Note:/ Consider using 'hsmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccHSMType :: Lens.Lens' CreateCluster Lude.Text
ccHSMType = Lens.lens (hsmType :: CreateCluster -> Lude.Text) (\s a -> s {hsmType = a} :: CreateCluster)
{-# DEPRECATED ccHSMType "Use generic-lens or generic-optics with 'hsmType' instead." #-}

instance Lude.AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Lude.<$> (x Lude..?> "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.CreateCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BackupRetentionPolicy" Lude..=) Lude.<$> backupRetentionPolicy,
            Lude.Just ("SubnetIds" Lude..= subnetIds),
            ("TagList" Lude..=) Lude.<$> tagList,
            ("SourceBackupId" Lude..=) Lude.<$> sourceBackupId,
            Lude.Just ("HsmType" Lude..= hsmType)
          ]
      )

instance Lude.ToPath CreateCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | Information about the cluster that was created.
    cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Information about the cluster that was created.
-- * 'responseStatus' - The response status code.
mkCreateClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClusterResponse
mkCreateClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the cluster that was created.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCluster :: Lens.Lens' CreateClusterResponse (Lude.Maybe Cluster)
ccrsCluster = Lens.lens (cluster :: CreateClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: CreateClusterResponse)
{-# DEPRECATED ccrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateClusterResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClusterResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
