{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxyTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxyTarget
  ( DBProxyTarget (..),

    -- * Smart constructor
    mkDBProxyTarget,

    -- * Lenses
    dbptEndpoint,
    dbptPort,
    dbptRdsResourceId,
    dbptTargetArn,
    dbptTargetHealth,
    dbptTrackedClusterId,
    dbptType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types
import qualified Network.AWS.RDS.Types.TargetHealth as Types
import qualified Network.AWS.RDS.Types.TargetType as Types

-- | Contains the details for an RDS Proxy target. It represents an RDS DB instance or Aurora DB cluster that the proxy can connect to. One or more targets are associated with an RDS Proxy target group.
--
-- This data type is used as a response element in the @DescribeDBProxyTargets@ action.
--
-- /See:/ 'mkDBProxyTarget' smart constructor.
data DBProxyTarget = DBProxyTarget'
  { -- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
    endpoint :: Core.Maybe Types.String,
    -- | The port that the RDS Proxy uses to connect to the target RDS DB instance or Aurora DB cluster.
    port :: Core.Maybe Core.Int,
    -- | The identifier representing the target. It can be the instance identifier for an RDS DB instance, or the cluster identifier for an Aurora DB cluster.
    rdsResourceId :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB cluster.
    targetArn :: Core.Maybe Types.String,
    -- | Information about the connection health of the RDS Proxy target.
    targetHealth :: Core.Maybe Types.TargetHealth,
    -- | The DB cluster identifier when the target represents an Aurora DB cluster. This field is blank when the target represents an RDS DB instance.
    trackedClusterId :: Core.Maybe Types.String,
    -- | Specifies the kind of database, such as an RDS DB instance or an Aurora DB cluster, that the target represents.
    type' :: Core.Maybe Types.TargetType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBProxyTarget' value with any optional fields omitted.
mkDBProxyTarget ::
  DBProxyTarget
mkDBProxyTarget =
  DBProxyTarget'
    { endpoint = Core.Nothing,
      port = Core.Nothing,
      rdsResourceId = Core.Nothing,
      targetArn = Core.Nothing,
      targetHealth = Core.Nothing,
      trackedClusterId = Core.Nothing,
      type' = Core.Nothing
    }

-- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptEndpoint :: Lens.Lens' DBProxyTarget (Core.Maybe Types.String)
dbptEndpoint = Lens.field @"endpoint"
{-# DEPRECATED dbptEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The port that the RDS Proxy uses to connect to the target RDS DB instance or Aurora DB cluster.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptPort :: Lens.Lens' DBProxyTarget (Core.Maybe Core.Int)
dbptPort = Lens.field @"port"
{-# DEPRECATED dbptPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The identifier representing the target. It can be the instance identifier for an RDS DB instance, or the cluster identifier for an Aurora DB cluster.
--
-- /Note:/ Consider using 'rdsResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptRdsResourceId :: Lens.Lens' DBProxyTarget (Core.Maybe Types.String)
dbptRdsResourceId = Lens.field @"rdsResourceId"
{-# DEPRECATED dbptRdsResourceId "Use generic-lens or generic-optics with 'rdsResourceId' instead." #-}

-- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB cluster.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptTargetArn :: Lens.Lens' DBProxyTarget (Core.Maybe Types.String)
dbptTargetArn = Lens.field @"targetArn"
{-# DEPRECATED dbptTargetArn "Use generic-lens or generic-optics with 'targetArn' instead." #-}

-- | Information about the connection health of the RDS Proxy target.
--
-- /Note:/ Consider using 'targetHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptTargetHealth :: Lens.Lens' DBProxyTarget (Core.Maybe Types.TargetHealth)
dbptTargetHealth = Lens.field @"targetHealth"
{-# DEPRECATED dbptTargetHealth "Use generic-lens or generic-optics with 'targetHealth' instead." #-}

-- | The DB cluster identifier when the target represents an Aurora DB cluster. This field is blank when the target represents an RDS DB instance.
--
-- /Note:/ Consider using 'trackedClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptTrackedClusterId :: Lens.Lens' DBProxyTarget (Core.Maybe Types.String)
dbptTrackedClusterId = Lens.field @"trackedClusterId"
{-# DEPRECATED dbptTrackedClusterId "Use generic-lens or generic-optics with 'trackedClusterId' instead." #-}

-- | Specifies the kind of database, such as an RDS DB instance or an Aurora DB cluster, that the target represents.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbptType :: Lens.Lens' DBProxyTarget (Core.Maybe Types.TargetType)
dbptType = Lens.field @"type'"
{-# DEPRECATED dbptType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromXML DBProxyTarget where
  parseXML x =
    DBProxyTarget'
      Core.<$> (x Core..@? "Endpoint")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "RdsResourceId")
      Core.<*> (x Core..@? "TargetArn")
      Core.<*> (x Core..@? "TargetHealth")
      Core.<*> (x Core..@? "TrackedClusterId")
      Core.<*> (x Core..@? "Type")
