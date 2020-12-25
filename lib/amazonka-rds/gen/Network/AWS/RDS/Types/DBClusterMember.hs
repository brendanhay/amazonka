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
    dbcmDBClusterParameterGroupStatus,
    dbcmDBInstanceIdentifier,
    dbcmIsClusterWriter,
    dbcmPromotionTier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Contains information about an instance that is part of a DB cluster.
--
-- /See:/ 'mkDBClusterMember' smart constructor.
data DBClusterMember = DBClusterMember'
  { -- | Specifies the status of the DB cluster parameter group for this member of the DB cluster.
    dBClusterParameterGroupStatus :: Core.Maybe Types.String,
    -- | Specifies the instance identifier for this member of the DB cluster.
    dBInstanceIdentifier :: Core.Maybe Types.String,
    -- | Value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
    isClusterWriter :: Core.Maybe Core.Bool,
    -- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
    promotionTier :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBClusterMember' value with any optional fields omitted.
mkDBClusterMember ::
  DBClusterMember
mkDBClusterMember =
  DBClusterMember'
    { dBClusterParameterGroupStatus = Core.Nothing,
      dBInstanceIdentifier = Core.Nothing,
      isClusterWriter = Core.Nothing,
      promotionTier = Core.Nothing
    }

-- | Specifies the status of the DB cluster parameter group for this member of the DB cluster.
--
-- /Note:/ Consider using 'dBClusterParameterGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcmDBClusterParameterGroupStatus :: Lens.Lens' DBClusterMember (Core.Maybe Types.String)
dbcmDBClusterParameterGroupStatus = Lens.field @"dBClusterParameterGroupStatus"
{-# DEPRECATED dbcmDBClusterParameterGroupStatus "Use generic-lens or generic-optics with 'dBClusterParameterGroupStatus' instead." #-}

-- | Specifies the instance identifier for this member of the DB cluster.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcmDBInstanceIdentifier :: Lens.Lens' DBClusterMember (Core.Maybe Types.String)
dbcmDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED dbcmDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | Value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
--
-- /Note:/ Consider using 'isClusterWriter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcmIsClusterWriter :: Lens.Lens' DBClusterMember (Core.Maybe Core.Bool)
dbcmIsClusterWriter = Lens.field @"isClusterWriter"
{-# DEPRECATED dbcmIsClusterWriter "Use generic-lens or generic-optics with 'isClusterWriter' instead." #-}

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'promotionTier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcmPromotionTier :: Lens.Lens' DBClusterMember (Core.Maybe Core.Int)
dbcmPromotionTier = Lens.field @"promotionTier"
{-# DEPRECATED dbcmPromotionTier "Use generic-lens or generic-optics with 'promotionTier' instead." #-}

instance Core.FromXML DBClusterMember where
  parseXML x =
    DBClusterMember'
      Core.<$> (x Core..@? "DBClusterParameterGroupStatus")
      Core.<*> (x Core..@? "DBInstanceIdentifier")
      Core.<*> (x Core..@? "IsClusterWriter")
      Core.<*> (x Core..@? "PromotionTier")
