-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxyTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxyTargetGroup
  ( DBProxyTargetGroup (..),

    -- * Smart constructor
    mkDBProxyTargetGroup,

    -- * Lenses
    dptgStatus,
    dptgConnectionPoolConfig,
    dptgTargetGroupARN,
    dptgUpdatedDate,
    dptgCreatedDate,
    dptgDBProxyName,
    dptgTargetGroupName,
    dptgIsDefault,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo

-- | Represents a set of RDS DB instances, Aurora DB clusters, or both that a proxy can connect to. Currently, each target group is associated with exactly one RDS DB instance or Aurora DB cluster.
--
-- This data type is used as a response element in the @DescribeDBProxyTargetGroups@ action.
--
-- /See:/ 'mkDBProxyTargetGroup' smart constructor.
data DBProxyTargetGroup = DBProxyTargetGroup'
  { status ::
      Lude.Maybe Lude.Text,
    connectionPoolConfig ::
      Lude.Maybe ConnectionPoolConfigurationInfo,
    targetGroupARN :: Lude.Maybe Lude.Text,
    updatedDate :: Lude.Maybe Lude.ISO8601,
    createdDate :: Lude.Maybe Lude.ISO8601,
    dbProxyName :: Lude.Maybe Lude.Text,
    targetGroupName :: Lude.Maybe Lude.Text,
    isDefault :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBProxyTargetGroup' with the minimum fields required to make a request.
--
-- * 'connectionPoolConfig' - The settings that determine the size and behavior of the connection pool for the target group.
-- * 'createdDate' - The date and time when the target group was first created.
-- * 'dbProxyName' - The identifier for the RDS proxy associated with this target group.
-- * 'isDefault' - Whether this target group is the first one used for connection requests by the associated proxy. Because each proxy is currently associated with a single target group, currently this setting is always @true@ .
-- * 'status' - The current status of this target group. A status of @available@ means the target group is correctly associated with a database. Other values indicate that you must wait for the target group to be ready, or take some action to resolve an issue.
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) representing the target group.
-- * 'targetGroupName' - The identifier for the target group. This name must be unique for all target groups owned by your AWS account in the specified AWS Region.
-- * 'updatedDate' - The date and time when the target group was last updated.
mkDBProxyTargetGroup ::
  DBProxyTargetGroup
mkDBProxyTargetGroup =
  DBProxyTargetGroup'
    { status = Lude.Nothing,
      connectionPoolConfig = Lude.Nothing,
      targetGroupARN = Lude.Nothing,
      updatedDate = Lude.Nothing,
      createdDate = Lude.Nothing,
      dbProxyName = Lude.Nothing,
      targetGroupName = Lude.Nothing,
      isDefault = Lude.Nothing
    }

-- | The current status of this target group. A status of @available@ means the target group is correctly associated with a database. Other values indicate that you must wait for the target group to be ready, or take some action to resolve an issue.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptgStatus :: Lens.Lens' DBProxyTargetGroup (Lude.Maybe Lude.Text)
dptgStatus = Lens.lens (status :: DBProxyTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBProxyTargetGroup)
{-# DEPRECATED dptgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The settings that determine the size and behavior of the connection pool for the target group.
--
-- /Note:/ Consider using 'connectionPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptgConnectionPoolConfig :: Lens.Lens' DBProxyTargetGroup (Lude.Maybe ConnectionPoolConfigurationInfo)
dptgConnectionPoolConfig = Lens.lens (connectionPoolConfig :: DBProxyTargetGroup -> Lude.Maybe ConnectionPoolConfigurationInfo) (\s a -> s {connectionPoolConfig = a} :: DBProxyTargetGroup)
{-# DEPRECATED dptgConnectionPoolConfig "Use generic-lens or generic-optics with 'connectionPoolConfig' instead." #-}

-- | The Amazon Resource Name (ARN) representing the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptgTargetGroupARN :: Lens.Lens' DBProxyTargetGroup (Lude.Maybe Lude.Text)
dptgTargetGroupARN = Lens.lens (targetGroupARN :: DBProxyTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupARN = a} :: DBProxyTargetGroup)
{-# DEPRECATED dptgTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

-- | The date and time when the target group was last updated.
--
-- /Note:/ Consider using 'updatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptgUpdatedDate :: Lens.Lens' DBProxyTargetGroup (Lude.Maybe Lude.ISO8601)
dptgUpdatedDate = Lens.lens (updatedDate :: DBProxyTargetGroup -> Lude.Maybe Lude.ISO8601) (\s a -> s {updatedDate = a} :: DBProxyTargetGroup)
{-# DEPRECATED dptgUpdatedDate "Use generic-lens or generic-optics with 'updatedDate' instead." #-}

-- | The date and time when the target group was first created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptgCreatedDate :: Lens.Lens' DBProxyTargetGroup (Lude.Maybe Lude.ISO8601)
dptgCreatedDate = Lens.lens (createdDate :: DBProxyTargetGroup -> Lude.Maybe Lude.ISO8601) (\s a -> s {createdDate = a} :: DBProxyTargetGroup)
{-# DEPRECATED dptgCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The identifier for the RDS proxy associated with this target group.
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptgDBProxyName :: Lens.Lens' DBProxyTargetGroup (Lude.Maybe Lude.Text)
dptgDBProxyName = Lens.lens (dbProxyName :: DBProxyTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbProxyName = a} :: DBProxyTargetGroup)
{-# DEPRECATED dptgDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

-- | The identifier for the target group. This name must be unique for all target groups owned by your AWS account in the specified AWS Region.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptgTargetGroupName :: Lens.Lens' DBProxyTargetGroup (Lude.Maybe Lude.Text)
dptgTargetGroupName = Lens.lens (targetGroupName :: DBProxyTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupName = a} :: DBProxyTargetGroup)
{-# DEPRECATED dptgTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

-- | Whether this target group is the first one used for connection requests by the associated proxy. Because each proxy is currently associated with a single target group, currently this setting is always @true@ .
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptgIsDefault :: Lens.Lens' DBProxyTargetGroup (Lude.Maybe Lude.Bool)
dptgIsDefault = Lens.lens (isDefault :: DBProxyTargetGroup -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: DBProxyTargetGroup)
{-# DEPRECATED dptgIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

instance Lude.FromXML DBProxyTargetGroup where
  parseXML x =
    DBProxyTargetGroup'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "ConnectionPoolConfig")
      Lude.<*> (x Lude..@? "TargetGroupArn")
      Lude.<*> (x Lude..@? "UpdatedDate")
      Lude.<*> (x Lude..@? "CreatedDate")
      Lude.<*> (x Lude..@? "DBProxyName")
      Lude.<*> (x Lude..@? "TargetGroupName")
      Lude.<*> (x Lude..@? "IsDefault")
