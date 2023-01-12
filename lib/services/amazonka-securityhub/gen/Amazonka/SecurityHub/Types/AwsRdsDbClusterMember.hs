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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbClusterMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbClusterMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an instance in the DB cluster.
--
-- /See:/ 'newAwsRdsDbClusterMember' smart constructor.
data AwsRdsDbClusterMember = AwsRdsDbClusterMember'
  { -- | The status of the DB cluster parameter group for this member of the DB
    -- cluster.
    dbClusterParameterGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | The instance identifier for this member of the DB cluster.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Whether the cluster member is the primary instance for the DB cluster.
    isClusterWriter :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the order in which an Aurora replica is promoted to the
    -- primary instance when the existing primary instance fails.
    promotionTier :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbClusterMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterParameterGroupStatus', 'awsRdsDbClusterMember_dbClusterParameterGroupStatus' - The status of the DB cluster parameter group for this member of the DB
-- cluster.
--
-- 'dbInstanceIdentifier', 'awsRdsDbClusterMember_dbInstanceIdentifier' - The instance identifier for this member of the DB cluster.
--
-- 'isClusterWriter', 'awsRdsDbClusterMember_isClusterWriter' - Whether the cluster member is the primary instance for the DB cluster.
--
-- 'promotionTier', 'awsRdsDbClusterMember_promotionTier' - Specifies the order in which an Aurora replica is promoted to the
-- primary instance when the existing primary instance fails.
newAwsRdsDbClusterMember ::
  AwsRdsDbClusterMember
newAwsRdsDbClusterMember =
  AwsRdsDbClusterMember'
    { dbClusterParameterGroupStatus =
        Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      isClusterWriter = Prelude.Nothing,
      promotionTier = Prelude.Nothing
    }

-- | The status of the DB cluster parameter group for this member of the DB
-- cluster.
awsRdsDbClusterMember_dbClusterParameterGroupStatus :: Lens.Lens' AwsRdsDbClusterMember (Prelude.Maybe Prelude.Text)
awsRdsDbClusterMember_dbClusterParameterGroupStatus = Lens.lens (\AwsRdsDbClusterMember' {dbClusterParameterGroupStatus} -> dbClusterParameterGroupStatus) (\s@AwsRdsDbClusterMember' {} a -> s {dbClusterParameterGroupStatus = a} :: AwsRdsDbClusterMember)

-- | The instance identifier for this member of the DB cluster.
awsRdsDbClusterMember_dbInstanceIdentifier :: Lens.Lens' AwsRdsDbClusterMember (Prelude.Maybe Prelude.Text)
awsRdsDbClusterMember_dbInstanceIdentifier = Lens.lens (\AwsRdsDbClusterMember' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AwsRdsDbClusterMember' {} a -> s {dbInstanceIdentifier = a} :: AwsRdsDbClusterMember)

-- | Whether the cluster member is the primary instance for the DB cluster.
awsRdsDbClusterMember_isClusterWriter :: Lens.Lens' AwsRdsDbClusterMember (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterMember_isClusterWriter = Lens.lens (\AwsRdsDbClusterMember' {isClusterWriter} -> isClusterWriter) (\s@AwsRdsDbClusterMember' {} a -> s {isClusterWriter = a} :: AwsRdsDbClusterMember)

-- | Specifies the order in which an Aurora replica is promoted to the
-- primary instance when the existing primary instance fails.
awsRdsDbClusterMember_promotionTier :: Lens.Lens' AwsRdsDbClusterMember (Prelude.Maybe Prelude.Int)
awsRdsDbClusterMember_promotionTier = Lens.lens (\AwsRdsDbClusterMember' {promotionTier} -> promotionTier) (\s@AwsRdsDbClusterMember' {} a -> s {promotionTier = a} :: AwsRdsDbClusterMember)

instance Data.FromJSON AwsRdsDbClusterMember where
  parseJSON =
    Data.withObject
      "AwsRdsDbClusterMember"
      ( \x ->
          AwsRdsDbClusterMember'
            Prelude.<$> (x Data..:? "DbClusterParameterGroupStatus")
            Prelude.<*> (x Data..:? "DbInstanceIdentifier")
            Prelude.<*> (x Data..:? "IsClusterWriter")
            Prelude.<*> (x Data..:? "PromotionTier")
      )

instance Prelude.Hashable AwsRdsDbClusterMember where
  hashWithSalt _salt AwsRdsDbClusterMember' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterParameterGroupStatus
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` isClusterWriter
      `Prelude.hashWithSalt` promotionTier

instance Prelude.NFData AwsRdsDbClusterMember where
  rnf AwsRdsDbClusterMember' {..} =
    Prelude.rnf dbClusterParameterGroupStatus
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf isClusterWriter
      `Prelude.seq` Prelude.rnf promotionTier

instance Data.ToJSON AwsRdsDbClusterMember where
  toJSON AwsRdsDbClusterMember' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DbClusterParameterGroupStatus" Data..=)
              Prelude.<$> dbClusterParameterGroupStatus,
            ("DbInstanceIdentifier" Data..=)
              Prelude.<$> dbInstanceIdentifier,
            ("IsClusterWriter" Data..=)
              Prelude.<$> isClusterWriter,
            ("PromotionTier" Data..=) Prelude.<$> promotionTier
          ]
      )
