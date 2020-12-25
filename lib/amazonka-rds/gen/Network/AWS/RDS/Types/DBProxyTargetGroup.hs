{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dbptgConnectionPoolConfig,
    dbptgCreatedDate,
    dbptgDBProxyName,
    dbptgIsDefault,
    dbptgStatus,
    dbptgTargetGroupArn,
    dbptgTargetGroupName,
    dbptgUpdatedDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo as Types
import qualified Network.AWS.RDS.Types.DBProxyName as Types
import qualified Network.AWS.RDS.Types.Status as Types
import qualified Network.AWS.RDS.Types.TargetGroupArn as Types
import qualified Network.AWS.RDS.Types.TargetGroupName as Types

-- | Represents a set of RDS DB instances, Aurora DB clusters, or both that a proxy can connect to. Currently, each target group is associated with exactly one RDS DB instance or Aurora DB cluster.
--
-- This data type is used as a response element in the @DescribeDBProxyTargetGroups@ action.
--
-- /See:/ 'mkDBProxyTargetGroup' smart constructor.
data DBProxyTargetGroup = DBProxyTargetGroup'
  { -- | The settings that determine the size and behavior of the connection pool for the target group.
    connectionPoolConfig :: Core.Maybe Types.ConnectionPoolConfigurationInfo,
    -- | The date and time when the target group was first created.
    createdDate :: Core.Maybe Core.UTCTime,
    -- | The identifier for the RDS proxy associated with this target group.
    dBProxyName :: Core.Maybe Types.DBProxyName,
    -- | Whether this target group is the first one used for connection requests by the associated proxy. Because each proxy is currently associated with a single target group, currently this setting is always @true@ .
    isDefault :: Core.Maybe Core.Bool,
    -- | The current status of this target group. A status of @available@ means the target group is correctly associated with a database. Other values indicate that you must wait for the target group to be ready, or take some action to resolve an issue.
    status :: Core.Maybe Types.Status,
    -- | The Amazon Resource Name (ARN) representing the target group.
    targetGroupArn :: Core.Maybe Types.TargetGroupArn,
    -- | The identifier for the target group. This name must be unique for all target groups owned by your AWS account in the specified AWS Region.
    targetGroupName :: Core.Maybe Types.TargetGroupName,
    -- | The date and time when the target group was last updated.
    updatedDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DBProxyTargetGroup' value with any optional fields omitted.
mkDBProxyTargetGroup ::
  DBProxyTargetGroup
mkDBProxyTargetGroup =
  DBProxyTargetGroup'
    { connectionPoolConfig = Core.Nothing,
      createdDate = Core.Nothing,
      dBProxyName = Core.Nothing,
      isDefault = Core.Nothing,
      status = Core.Nothing,
      targetGroupArn = Core.Nothing,
      targetGroupName = Core.Nothing,
      updatedDate = Core.Nothing
    }

-- | The settings that determine the size and behavior of the connection pool for the target group.
--
-- /Note:/ Consider using 'connectionPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgConnectionPoolConfig :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Types.ConnectionPoolConfigurationInfo)
dbptgConnectionPoolConfig = Lens.field @"connectionPoolConfig"
{-# DEPRECATED dbptgConnectionPoolConfig "Use generic-lens or generic-optics with 'connectionPoolConfig' instead." #-}

-- | The date and time when the target group was first created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgCreatedDate :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.UTCTime)
dbptgCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED dbptgCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The identifier for the RDS proxy associated with this target group.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgDBProxyName :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Types.DBProxyName)
dbptgDBProxyName = Lens.field @"dBProxyName"
{-# DEPRECATED dbptgDBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead." #-}

-- | Whether this target group is the first one used for connection requests by the associated proxy. Because each proxy is currently associated with a single target group, currently this setting is always @true@ .
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgIsDefault :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Bool)
dbptgIsDefault = Lens.field @"isDefault"
{-# DEPRECATED dbptgIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

-- | The current status of this target group. A status of @available@ means the target group is correctly associated with a database. Other values indicate that you must wait for the target group to be ready, or take some action to resolve an issue.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgStatus :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Types.Status)
dbptgStatus = Lens.field @"status"
{-# DEPRECATED dbptgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) representing the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgTargetGroupArn :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Types.TargetGroupArn)
dbptgTargetGroupArn = Lens.field @"targetGroupArn"
{-# DEPRECATED dbptgTargetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead." #-}

-- | The identifier for the target group. This name must be unique for all target groups owned by your AWS account in the specified AWS Region.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgTargetGroupName :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Types.TargetGroupName)
dbptgTargetGroupName = Lens.field @"targetGroupName"
{-# DEPRECATED dbptgTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

-- | The date and time when the target group was last updated.
--
-- /Note:/ Consider using 'updatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgUpdatedDate :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.UTCTime)
dbptgUpdatedDate = Lens.field @"updatedDate"
{-# DEPRECATED dbptgUpdatedDate "Use generic-lens or generic-optics with 'updatedDate' instead." #-}

instance Core.FromXML DBProxyTargetGroup where
  parseXML x =
    DBProxyTargetGroup'
      Core.<$> (x Core..@? "ConnectionPoolConfig")
      Core.<*> (x Core..@? "CreatedDate")
      Core.<*> (x Core..@? "DBProxyName")
      Core.<*> (x Core..@? "IsDefault")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "TargetGroupArn")
      Core.<*> (x Core..@? "TargetGroupName")
      Core.<*> (x Core..@? "UpdatedDate")
