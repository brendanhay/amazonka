{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxyTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBProxyTargetGroup
  ( DBProxyTargetGroup (..)
  -- * Smart constructor
  , mkDBProxyTargetGroup
  -- * Lenses
  , dbptgConnectionPoolConfig
  , dbptgCreatedDate
  , dbptgDBProxyName
  , dbptgIsDefault
  , dbptgStatus
  , dbptgTargetGroupArn
  , dbptgTargetGroupName
  , dbptgUpdatedDate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo as Types

-- | Represents a set of RDS DB instances, Aurora DB clusters, or both that a proxy can connect to. Currently, each target group is associated with exactly one RDS DB instance or Aurora DB cluster.
--
-- This data type is used as a response element in the @DescribeDBProxyTargetGroups@ action.
--
-- /See:/ 'mkDBProxyTargetGroup' smart constructor.
data DBProxyTargetGroup = DBProxyTargetGroup'
  { connectionPoolConfig :: Core.Maybe Types.ConnectionPoolConfigurationInfo
    -- ^ The settings that determine the size and behavior of the connection pool for the target group.
  , createdDate :: Core.Maybe Core.UTCTime
    -- ^ The date and time when the target group was first created.
  , dBProxyName :: Core.Maybe Core.Text
    -- ^ The identifier for the RDS proxy associated with this target group.
  , isDefault :: Core.Maybe Core.Bool
    -- ^ Whether this target group is the first one used for connection requests by the associated proxy. Because each proxy is currently associated with a single target group, currently this setting is always @true@ .
  , status :: Core.Maybe Core.Text
    -- ^ The current status of this target group. A status of @available@ means the target group is correctly associated with a database. Other values indicate that you must wait for the target group to be ready, or take some action to resolve an issue.
  , targetGroupArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) representing the target group.
  , targetGroupName :: Core.Maybe Core.Text
    -- ^ The identifier for the target group. This name must be unique for all target groups owned by your AWS account in the specified AWS Region.
  , updatedDate :: Core.Maybe Core.UTCTime
    -- ^ The date and time when the target group was last updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DBProxyTargetGroup' value with any optional fields omitted.
mkDBProxyTargetGroup
    :: DBProxyTargetGroup
mkDBProxyTargetGroup
  = DBProxyTargetGroup'{connectionPoolConfig = Core.Nothing,
                        createdDate = Core.Nothing, dBProxyName = Core.Nothing,
                        isDefault = Core.Nothing, status = Core.Nothing,
                        targetGroupArn = Core.Nothing, targetGroupName = Core.Nothing,
                        updatedDate = Core.Nothing}

-- | The settings that determine the size and behavior of the connection pool for the target group.
--
-- /Note:/ Consider using 'connectionPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgConnectionPoolConfig :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Types.ConnectionPoolConfigurationInfo)
dbptgConnectionPoolConfig = Lens.field @"connectionPoolConfig"
{-# INLINEABLE dbptgConnectionPoolConfig #-}
{-# DEPRECATED connectionPoolConfig "Use generic-lens or generic-optics with 'connectionPoolConfig' instead"  #-}

-- | The date and time when the target group was first created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgCreatedDate :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.UTCTime)
dbptgCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE dbptgCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The identifier for the RDS proxy associated with this target group.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgDBProxyName :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Text)
dbptgDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE dbptgDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

-- | Whether this target group is the first one used for connection requests by the associated proxy. Because each proxy is currently associated with a single target group, currently this setting is always @true@ .
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgIsDefault :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Bool)
dbptgIsDefault = Lens.field @"isDefault"
{-# INLINEABLE dbptgIsDefault #-}
{-# DEPRECATED isDefault "Use generic-lens or generic-optics with 'isDefault' instead"  #-}

-- | The current status of this target group. A status of @available@ means the target group is correctly associated with a database. Other values indicate that you must wait for the target group to be ready, or take some action to resolve an issue.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgStatus :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Text)
dbptgStatus = Lens.field @"status"
{-# INLINEABLE dbptgStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The Amazon Resource Name (ARN) representing the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgTargetGroupArn :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Text)
dbptgTargetGroupArn = Lens.field @"targetGroupArn"
{-# INLINEABLE dbptgTargetGroupArn #-}
{-# DEPRECATED targetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead"  #-}

-- | The identifier for the target group. This name must be unique for all target groups owned by your AWS account in the specified AWS Region.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgTargetGroupName :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Text)
dbptgTargetGroupName = Lens.field @"targetGroupName"
{-# INLINEABLE dbptgTargetGroupName #-}
{-# DEPRECATED targetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead"  #-}

-- | The date and time when the target group was last updated.
--
-- /Note:/ Consider using 'updatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptgUpdatedDate :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.UTCTime)
dbptgUpdatedDate = Lens.field @"updatedDate"
{-# INLINEABLE dbptgUpdatedDate #-}
{-# DEPRECATED updatedDate "Use generic-lens or generic-optics with 'updatedDate' instead"  #-}

instance Core.FromXML DBProxyTargetGroup where
        parseXML x
          = DBProxyTargetGroup' Core.<$>
              (x Core..@? "ConnectionPoolConfig") Core.<*>
                x Core..@? "CreatedDate"
                Core.<*> x Core..@? "DBProxyName"
                Core.<*> x Core..@? "IsDefault"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "TargetGroupArn"
                Core.<*> x Core..@? "TargetGroupName"
                Core.<*> x Core..@? "UpdatedDate"
