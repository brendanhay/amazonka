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
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an instance that is part of a DB cluster.
--
-- /See:/ 'newDBClusterMember' smart constructor.
data DBClusterMember = DBClusterMember'
  { -- | Value that is @true@ if the cluster member is the primary instance for
    -- the DB cluster and @false@ otherwise.
    isClusterWriter :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the instance identifier for this member of the DB cluster.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which an Aurora Replica is promoted
    -- to the primary instance after a failure of the existing primary
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
    -- in the /Amazon Aurora User Guide/.
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | Specifies the status of the DB cluster parameter group for this member
    -- of the DB cluster.
    dbClusterParameterGroupStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { isClusterWriter = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      promotionTier = Prelude.Nothing,
      dbClusterParameterGroupStatus = Prelude.Nothing
    }

-- | Value that is @true@ if the cluster member is the primary instance for
-- the DB cluster and @false@ otherwise.
dbClusterMember_isClusterWriter :: Lens.Lens' DBClusterMember (Prelude.Maybe Prelude.Bool)
dbClusterMember_isClusterWriter = Lens.lens (\DBClusterMember' {isClusterWriter} -> isClusterWriter) (\s@DBClusterMember' {} a -> s {isClusterWriter = a} :: DBClusterMember)

-- | Specifies the instance identifier for this member of the DB cluster.
dbClusterMember_dbInstanceIdentifier :: Lens.Lens' DBClusterMember (Prelude.Maybe Prelude.Text)
dbClusterMember_dbInstanceIdentifier = Lens.lens (\DBClusterMember' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBClusterMember' {} a -> s {dbInstanceIdentifier = a} :: DBClusterMember)

-- | A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
dbClusterMember_promotionTier :: Lens.Lens' DBClusterMember (Prelude.Maybe Prelude.Int)
dbClusterMember_promotionTier = Lens.lens (\DBClusterMember' {promotionTier} -> promotionTier) (\s@DBClusterMember' {} a -> s {promotionTier = a} :: DBClusterMember)

-- | Specifies the status of the DB cluster parameter group for this member
-- of the DB cluster.
dbClusterMember_dbClusterParameterGroupStatus :: Lens.Lens' DBClusterMember (Prelude.Maybe Prelude.Text)
dbClusterMember_dbClusterParameterGroupStatus = Lens.lens (\DBClusterMember' {dbClusterParameterGroupStatus} -> dbClusterParameterGroupStatus) (\s@DBClusterMember' {} a -> s {dbClusterParameterGroupStatus = a} :: DBClusterMember)

instance Core.FromXML DBClusterMember where
  parseXML x =
    DBClusterMember'
      Prelude.<$> (x Core..@? "IsClusterWriter")
      Prelude.<*> (x Core..@? "DBInstanceIdentifier")
      Prelude.<*> (x Core..@? "PromotionTier")
      Prelude.<*> (x Core..@? "DBClusterParameterGroupStatus")

instance Prelude.Hashable DBClusterMember

instance Prelude.NFData DBClusterMember
