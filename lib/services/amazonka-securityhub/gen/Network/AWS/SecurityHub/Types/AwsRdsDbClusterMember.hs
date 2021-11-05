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
-- Module      : Network.AWS.SecurityHub.Types.AwsRdsDbClusterMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsRdsDbClusterMember where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an instance in the DB cluster.
--
-- /See:/ 'newAwsRdsDbClusterMember' smart constructor.
data AwsRdsDbClusterMember = AwsRdsDbClusterMember'
  { -- | Specifies the order in which an Aurora replica is promoted to the
    -- primary instance when the existing primary instance fails.
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | The instance identifier for this member of the DB cluster.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Whether the cluster member is the primary instance for the DB cluster.
    isClusterWriter :: Prelude.Maybe Prelude.Bool,
    -- | The status of the DB cluster parameter group for this member of the DB
    -- cluster.
    dbClusterParameterGroupStatus :: Prelude.Maybe Prelude.Text
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
-- 'promotionTier', 'awsRdsDbClusterMember_promotionTier' - Specifies the order in which an Aurora replica is promoted to the
-- primary instance when the existing primary instance fails.
--
-- 'dbInstanceIdentifier', 'awsRdsDbClusterMember_dbInstanceIdentifier' - The instance identifier for this member of the DB cluster.
--
-- 'isClusterWriter', 'awsRdsDbClusterMember_isClusterWriter' - Whether the cluster member is the primary instance for the DB cluster.
--
-- 'dbClusterParameterGroupStatus', 'awsRdsDbClusterMember_dbClusterParameterGroupStatus' - The status of the DB cluster parameter group for this member of the DB
-- cluster.
newAwsRdsDbClusterMember ::
  AwsRdsDbClusterMember
newAwsRdsDbClusterMember =
  AwsRdsDbClusterMember'
    { promotionTier =
        Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      isClusterWriter = Prelude.Nothing,
      dbClusterParameterGroupStatus = Prelude.Nothing
    }

-- | Specifies the order in which an Aurora replica is promoted to the
-- primary instance when the existing primary instance fails.
awsRdsDbClusterMember_promotionTier :: Lens.Lens' AwsRdsDbClusterMember (Prelude.Maybe Prelude.Int)
awsRdsDbClusterMember_promotionTier = Lens.lens (\AwsRdsDbClusterMember' {promotionTier} -> promotionTier) (\s@AwsRdsDbClusterMember' {} a -> s {promotionTier = a} :: AwsRdsDbClusterMember)

-- | The instance identifier for this member of the DB cluster.
awsRdsDbClusterMember_dbInstanceIdentifier :: Lens.Lens' AwsRdsDbClusterMember (Prelude.Maybe Prelude.Text)
awsRdsDbClusterMember_dbInstanceIdentifier = Lens.lens (\AwsRdsDbClusterMember' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AwsRdsDbClusterMember' {} a -> s {dbInstanceIdentifier = a} :: AwsRdsDbClusterMember)

-- | Whether the cluster member is the primary instance for the DB cluster.
awsRdsDbClusterMember_isClusterWriter :: Lens.Lens' AwsRdsDbClusterMember (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterMember_isClusterWriter = Lens.lens (\AwsRdsDbClusterMember' {isClusterWriter} -> isClusterWriter) (\s@AwsRdsDbClusterMember' {} a -> s {isClusterWriter = a} :: AwsRdsDbClusterMember)

-- | The status of the DB cluster parameter group for this member of the DB
-- cluster.
awsRdsDbClusterMember_dbClusterParameterGroupStatus :: Lens.Lens' AwsRdsDbClusterMember (Prelude.Maybe Prelude.Text)
awsRdsDbClusterMember_dbClusterParameterGroupStatus = Lens.lens (\AwsRdsDbClusterMember' {dbClusterParameterGroupStatus} -> dbClusterParameterGroupStatus) (\s@AwsRdsDbClusterMember' {} a -> s {dbClusterParameterGroupStatus = a} :: AwsRdsDbClusterMember)

instance Core.FromJSON AwsRdsDbClusterMember where
  parseJSON =
    Core.withObject
      "AwsRdsDbClusterMember"
      ( \x ->
          AwsRdsDbClusterMember'
            Prelude.<$> (x Core..:? "PromotionTier")
            Prelude.<*> (x Core..:? "DbInstanceIdentifier")
            Prelude.<*> (x Core..:? "IsClusterWriter")
            Prelude.<*> (x Core..:? "DbClusterParameterGroupStatus")
      )

instance Prelude.Hashable AwsRdsDbClusterMember

instance Prelude.NFData AwsRdsDbClusterMember

instance Core.ToJSON AwsRdsDbClusterMember where
  toJSON AwsRdsDbClusterMember' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PromotionTier" Core..=) Prelude.<$> promotionTier,
            ("DbInstanceIdentifier" Core..=)
              Prelude.<$> dbInstanceIdentifier,
            ("IsClusterWriter" Core..=)
              Prelude.<$> isClusterWriter,
            ("DbClusterParameterGroupStatus" Core..=)
              Prelude.<$> dbClusterParameterGroupStatus
          ]
      )
