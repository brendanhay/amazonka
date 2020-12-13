{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterMember
  ( DBClusterMember (..),

    -- * Smart constructor
    mkDBClusterMember,

    -- * Lenses
    dcmPromotionTier,
    dcmDBInstanceIdentifier,
    dcmIsClusterWriter,
    dcmDBClusterParameterGroupStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an instance that is part of a DB cluster.
--
-- /See:/ 'mkDBClusterMember' smart constructor.
data DBClusterMember = DBClusterMember'
  { -- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
    promotionTier :: Lude.Maybe Lude.Int,
    -- | Specifies the instance identifier for this member of the DB cluster.
    dbInstanceIdentifier :: Lude.Maybe Lude.Text,
    -- | Value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
    isClusterWriter :: Lude.Maybe Lude.Bool,
    -- | Specifies the status of the DB cluster parameter group for this member of the DB cluster.
    dbClusterParameterGroupStatus :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBClusterMember' with the minimum fields required to make a request.
--
-- * 'promotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
-- * 'dbInstanceIdentifier' - Specifies the instance identifier for this member of the DB cluster.
-- * 'isClusterWriter' - Value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
-- * 'dbClusterParameterGroupStatus' - Specifies the status of the DB cluster parameter group for this member of the DB cluster.
mkDBClusterMember ::
  DBClusterMember
mkDBClusterMember =
  DBClusterMember'
    { promotionTier = Lude.Nothing,
      dbInstanceIdentifier = Lude.Nothing,
      isClusterWriter = Lude.Nothing,
      dbClusterParameterGroupStatus = Lude.Nothing
    }

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'promotionTier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmPromotionTier :: Lens.Lens' DBClusterMember (Lude.Maybe Lude.Int)
dcmPromotionTier = Lens.lens (promotionTier :: DBClusterMember -> Lude.Maybe Lude.Int) (\s a -> s {promotionTier = a} :: DBClusterMember)
{-# DEPRECATED dcmPromotionTier "Use generic-lens or generic-optics with 'promotionTier' instead." #-}

-- | Specifies the instance identifier for this member of the DB cluster.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmDBInstanceIdentifier :: Lens.Lens' DBClusterMember (Lude.Maybe Lude.Text)
dcmDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DBClusterMember -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DBClusterMember)
{-# DEPRECATED dcmDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | Value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
--
-- /Note:/ Consider using 'isClusterWriter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmIsClusterWriter :: Lens.Lens' DBClusterMember (Lude.Maybe Lude.Bool)
dcmIsClusterWriter = Lens.lens (isClusterWriter :: DBClusterMember -> Lude.Maybe Lude.Bool) (\s a -> s {isClusterWriter = a} :: DBClusterMember)
{-# DEPRECATED dcmIsClusterWriter "Use generic-lens or generic-optics with 'isClusterWriter' instead." #-}

-- | Specifies the status of the DB cluster parameter group for this member of the DB cluster.
--
-- /Note:/ Consider using 'dbClusterParameterGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmDBClusterParameterGroupStatus :: Lens.Lens' DBClusterMember (Lude.Maybe Lude.Text)
dcmDBClusterParameterGroupStatus = Lens.lens (dbClusterParameterGroupStatus :: DBClusterMember -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupStatus = a} :: DBClusterMember)
{-# DEPRECATED dcmDBClusterParameterGroupStatus "Use generic-lens or generic-optics with 'dbClusterParameterGroupStatus' instead." #-}

instance Lude.FromXML DBClusterMember where
  parseXML x =
    DBClusterMember'
      Lude.<$> (x Lude..@? "PromotionTier")
      Lude.<*> (x Lude..@? "DBInstanceIdentifier")
      Lude.<*> (x Lude..@? "IsClusterWriter")
      Lude.<*> (x Lude..@? "DBClusterParameterGroupStatus")
