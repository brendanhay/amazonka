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
-- Module      : Amazonka.DocumentDB.Types.DBClusterMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.DBClusterMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an instance that is part of a cluster.
--
-- /See:/ 'newDBClusterMember' smart constructor.
data DBClusterMember = DBClusterMember'
  { -- | Specifies the status of the cluster parameter group for this member of
    -- the DB cluster.
    dbClusterParameterGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | Specifies the instance identifier for this member of the cluster.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A value that is @true@ if the cluster member is the primary instance for
    -- the cluster and @false@ otherwise.
    isClusterWriter :: Prelude.Maybe Prelude.Bool,
    -- | A value that specifies the order in which an Amazon DocumentDB replica
    -- is promoted to the primary instance after a failure of the existing
    -- primary instance.
    promotionTier :: Prelude.Maybe Prelude.Int
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
-- 'dbClusterParameterGroupStatus', 'dbClusterMember_dbClusterParameterGroupStatus' - Specifies the status of the cluster parameter group for this member of
-- the DB cluster.
--
-- 'dbInstanceIdentifier', 'dbClusterMember_dbInstanceIdentifier' - Specifies the instance identifier for this member of the cluster.
--
-- 'isClusterWriter', 'dbClusterMember_isClusterWriter' - A value that is @true@ if the cluster member is the primary instance for
-- the cluster and @false@ otherwise.
--
-- 'promotionTier', 'dbClusterMember_promotionTier' - A value that specifies the order in which an Amazon DocumentDB replica
-- is promoted to the primary instance after a failure of the existing
-- primary instance.
newDBClusterMember ::
  DBClusterMember
newDBClusterMember =
  DBClusterMember'
    { dbClusterParameterGroupStatus =
        Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      isClusterWriter = Prelude.Nothing,
      promotionTier = Prelude.Nothing
    }

-- | Specifies the status of the cluster parameter group for this member of
-- the DB cluster.
dbClusterMember_dbClusterParameterGroupStatus :: Lens.Lens' DBClusterMember (Prelude.Maybe Prelude.Text)
dbClusterMember_dbClusterParameterGroupStatus = Lens.lens (\DBClusterMember' {dbClusterParameterGroupStatus} -> dbClusterParameterGroupStatus) (\s@DBClusterMember' {} a -> s {dbClusterParameterGroupStatus = a} :: DBClusterMember)

-- | Specifies the instance identifier for this member of the cluster.
dbClusterMember_dbInstanceIdentifier :: Lens.Lens' DBClusterMember (Prelude.Maybe Prelude.Text)
dbClusterMember_dbInstanceIdentifier = Lens.lens (\DBClusterMember' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBClusterMember' {} a -> s {dbInstanceIdentifier = a} :: DBClusterMember)

-- | A value that is @true@ if the cluster member is the primary instance for
-- the cluster and @false@ otherwise.
dbClusterMember_isClusterWriter :: Lens.Lens' DBClusterMember (Prelude.Maybe Prelude.Bool)
dbClusterMember_isClusterWriter = Lens.lens (\DBClusterMember' {isClusterWriter} -> isClusterWriter) (\s@DBClusterMember' {} a -> s {isClusterWriter = a} :: DBClusterMember)

-- | A value that specifies the order in which an Amazon DocumentDB replica
-- is promoted to the primary instance after a failure of the existing
-- primary instance.
dbClusterMember_promotionTier :: Lens.Lens' DBClusterMember (Prelude.Maybe Prelude.Int)
dbClusterMember_promotionTier = Lens.lens (\DBClusterMember' {promotionTier} -> promotionTier) (\s@DBClusterMember' {} a -> s {promotionTier = a} :: DBClusterMember)

instance Data.FromXML DBClusterMember where
  parseXML x =
    DBClusterMember'
      Prelude.<$> (x Data..@? "DBClusterParameterGroupStatus")
      Prelude.<*> (x Data..@? "DBInstanceIdentifier")
      Prelude.<*> (x Data..@? "IsClusterWriter")
      Prelude.<*> (x Data..@? "PromotionTier")

instance Prelude.Hashable DBClusterMember where
  hashWithSalt _salt DBClusterMember' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterParameterGroupStatus
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` isClusterWriter
      `Prelude.hashWithSalt` promotionTier

instance Prelude.NFData DBClusterMember where
  rnf DBClusterMember' {..} =
    Prelude.rnf dbClusterParameterGroupStatus
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf isClusterWriter
      `Prelude.seq` Prelude.rnf promotionTier
