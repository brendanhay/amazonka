{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterMember where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about an instance that is part of a DB cluster.
--
-- /See:/ 'newDBClusterMember' smart constructor.
data DBClusterMember = DBClusterMember'
  { -- | Value that is @true@ if the cluster member is the primary instance for
    -- the DB cluster and @false@ otherwise.
    isClusterWriter :: Core.Maybe Core.Bool,
    -- | Specifies the instance identifier for this member of the DB cluster.
    dbInstanceIdentifier :: Core.Maybe Core.Text,
    -- | A value that specifies the order in which an Aurora Replica is promoted
    -- to the primary instance after a failure of the existing primary
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
    -- in the /Amazon Aurora User Guide/.
    promotionTier :: Core.Maybe Core.Int,
    -- | Specifies the status of the DB cluster parameter group for this member
    -- of the DB cluster.
    dbClusterParameterGroupStatus :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBClusterMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isClusterWriter', 'dbClusterMember_isClusterWriter' - Value that is @true@ if the cluster member is the primary instance for
-- the DB cluster and @false@ otherwise.
--
-- 'dbInstanceIdentifier', 'dbClusterMember_dbInstanceIdentifier' - Specifies the instance identifier for this member of the DB cluster.
--
-- 'promotionTier', 'dbClusterMember_promotionTier' - A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- 'dbClusterParameterGroupStatus', 'dbClusterMember_dbClusterParameterGroupStatus' - Specifies the status of the DB cluster parameter group for this member
-- of the DB cluster.
newDBClusterMember ::
  DBClusterMember
newDBClusterMember =
  DBClusterMember'
    { isClusterWriter = Core.Nothing,
      dbInstanceIdentifier = Core.Nothing,
      promotionTier = Core.Nothing,
      dbClusterParameterGroupStatus = Core.Nothing
    }

-- | Value that is @true@ if the cluster member is the primary instance for
-- the DB cluster and @false@ otherwise.
dbClusterMember_isClusterWriter :: Lens.Lens' DBClusterMember (Core.Maybe Core.Bool)
dbClusterMember_isClusterWriter = Lens.lens (\DBClusterMember' {isClusterWriter} -> isClusterWriter) (\s@DBClusterMember' {} a -> s {isClusterWriter = a} :: DBClusterMember)

-- | Specifies the instance identifier for this member of the DB cluster.
dbClusterMember_dbInstanceIdentifier :: Lens.Lens' DBClusterMember (Core.Maybe Core.Text)
dbClusterMember_dbInstanceIdentifier = Lens.lens (\DBClusterMember' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBClusterMember' {} a -> s {dbInstanceIdentifier = a} :: DBClusterMember)

-- | A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
dbClusterMember_promotionTier :: Lens.Lens' DBClusterMember (Core.Maybe Core.Int)
dbClusterMember_promotionTier = Lens.lens (\DBClusterMember' {promotionTier} -> promotionTier) (\s@DBClusterMember' {} a -> s {promotionTier = a} :: DBClusterMember)

-- | Specifies the status of the DB cluster parameter group for this member
-- of the DB cluster.
dbClusterMember_dbClusterParameterGroupStatus :: Lens.Lens' DBClusterMember (Core.Maybe Core.Text)
dbClusterMember_dbClusterParameterGroupStatus = Lens.lens (\DBClusterMember' {dbClusterParameterGroupStatus} -> dbClusterParameterGroupStatus) (\s@DBClusterMember' {} a -> s {dbClusterParameterGroupStatus = a} :: DBClusterMember)

instance Core.FromXML DBClusterMember where
  parseXML x =
    DBClusterMember'
      Core.<$> (x Core..@? "IsClusterWriter")
      Core.<*> (x Core..@? "DBInstanceIdentifier")
      Core.<*> (x Core..@? "PromotionTier")
      Core.<*> (x Core..@? "DBClusterParameterGroupStatus")

instance Core.Hashable DBClusterMember

instance Core.NFData DBClusterMember
