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
    dptTargetARN,
    dptTargetHealth,
    dptTrackedClusterId,
    dptRDSResourceId,
    dptType,
    dptEndpoint,
    dptPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.TargetHealth
import Network.AWS.RDS.Types.TargetType

-- | Contains the details for an RDS Proxy target. It represents an RDS DB instance or Aurora DB cluster that the proxy can connect to. One or more targets are associated with an RDS Proxy target group.
--
-- This data type is used as a response element in the @DescribeDBProxyTargets@ action.
--
-- /See:/ 'mkDBProxyTarget' smart constructor.
data DBProxyTarget = DBProxyTarget'
  { -- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB cluster.
    targetARN :: Lude.Maybe Lude.Text,
    -- | Information about the connection health of the RDS Proxy target.
    targetHealth :: Lude.Maybe TargetHealth,
    -- | The DB cluster identifier when the target represents an Aurora DB cluster. This field is blank when the target represents an RDS DB instance.
    trackedClusterId :: Lude.Maybe Lude.Text,
    -- | The identifier representing the target. It can be the instance identifier for an RDS DB instance, or the cluster identifier for an Aurora DB cluster.
    rdsResourceId :: Lude.Maybe Lude.Text,
    -- | Specifies the kind of database, such as an RDS DB instance or an Aurora DB cluster, that the target represents.
    type' :: Lude.Maybe TargetType,
    -- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
    endpoint :: Lude.Maybe Lude.Text,
    -- | The port that the RDS Proxy uses to connect to the target RDS DB instance or Aurora DB cluster.
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBProxyTarget' with the minimum fields required to make a request.
--
-- * 'targetARN' - The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB cluster.
-- * 'targetHealth' - Information about the connection health of the RDS Proxy target.
-- * 'trackedClusterId' - The DB cluster identifier when the target represents an Aurora DB cluster. This field is blank when the target represents an RDS DB instance.
-- * 'rdsResourceId' - The identifier representing the target. It can be the instance identifier for an RDS DB instance, or the cluster identifier for an Aurora DB cluster.
-- * 'type'' - Specifies the kind of database, such as an RDS DB instance or an Aurora DB cluster, that the target represents.
-- * 'endpoint' - The writer endpoint for the RDS DB instance or Aurora DB cluster.
-- * 'port' - The port that the RDS Proxy uses to connect to the target RDS DB instance or Aurora DB cluster.
mkDBProxyTarget ::
  DBProxyTarget
mkDBProxyTarget =
  DBProxyTarget'
    { targetARN = Lude.Nothing,
      targetHealth = Lude.Nothing,
      trackedClusterId = Lude.Nothing,
      rdsResourceId = Lude.Nothing,
      type' = Lude.Nothing,
      endpoint = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB cluster.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptTargetARN :: Lens.Lens' DBProxyTarget (Lude.Maybe Lude.Text)
dptTargetARN = Lens.lens (targetARN :: DBProxyTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: DBProxyTarget)
{-# DEPRECATED dptTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | Information about the connection health of the RDS Proxy target.
--
-- /Note:/ Consider using 'targetHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptTargetHealth :: Lens.Lens' DBProxyTarget (Lude.Maybe TargetHealth)
dptTargetHealth = Lens.lens (targetHealth :: DBProxyTarget -> Lude.Maybe TargetHealth) (\s a -> s {targetHealth = a} :: DBProxyTarget)
{-# DEPRECATED dptTargetHealth "Use generic-lens or generic-optics with 'targetHealth' instead." #-}

-- | The DB cluster identifier when the target represents an Aurora DB cluster. This field is blank when the target represents an RDS DB instance.
--
-- /Note:/ Consider using 'trackedClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptTrackedClusterId :: Lens.Lens' DBProxyTarget (Lude.Maybe Lude.Text)
dptTrackedClusterId = Lens.lens (trackedClusterId :: DBProxyTarget -> Lude.Maybe Lude.Text) (\s a -> s {trackedClusterId = a} :: DBProxyTarget)
{-# DEPRECATED dptTrackedClusterId "Use generic-lens or generic-optics with 'trackedClusterId' instead." #-}

-- | The identifier representing the target. It can be the instance identifier for an RDS DB instance, or the cluster identifier for an Aurora DB cluster.
--
-- /Note:/ Consider using 'rdsResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptRDSResourceId :: Lens.Lens' DBProxyTarget (Lude.Maybe Lude.Text)
dptRDSResourceId = Lens.lens (rdsResourceId :: DBProxyTarget -> Lude.Maybe Lude.Text) (\s a -> s {rdsResourceId = a} :: DBProxyTarget)
{-# DEPRECATED dptRDSResourceId "Use generic-lens or generic-optics with 'rdsResourceId' instead." #-}

-- | Specifies the kind of database, such as an RDS DB instance or an Aurora DB cluster, that the target represents.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptType :: Lens.Lens' DBProxyTarget (Lude.Maybe TargetType)
dptType = Lens.lens (type' :: DBProxyTarget -> Lude.Maybe TargetType) (\s a -> s {type' = a} :: DBProxyTarget)
{-# DEPRECATED dptType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptEndpoint :: Lens.Lens' DBProxyTarget (Lude.Maybe Lude.Text)
dptEndpoint = Lens.lens (endpoint :: DBProxyTarget -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: DBProxyTarget)
{-# DEPRECATED dptEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The port that the RDS Proxy uses to connect to the target RDS DB instance or Aurora DB cluster.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptPort :: Lens.Lens' DBProxyTarget (Lude.Maybe Lude.Int)
dptPort = Lens.lens (port :: DBProxyTarget -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: DBProxyTarget)
{-# DEPRECATED dptPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromXML DBProxyTarget where
  parseXML x =
    DBProxyTarget'
      Lude.<$> (x Lude..@? "TargetArn")
      Lude.<*> (x Lude..@? "TargetHealth")
      Lude.<*> (x Lude..@? "TrackedClusterId")
      Lude.<*> (x Lude..@? "RdsResourceId")
      Lude.<*> (x Lude..@? "Type")
      Lude.<*> (x Lude..@? "Endpoint")
      Lude.<*> (x Lude..@? "Port")
