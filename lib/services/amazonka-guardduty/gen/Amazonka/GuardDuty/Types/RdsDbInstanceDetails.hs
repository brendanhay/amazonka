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
-- Module      : Amazonka.GuardDuty.Types.RdsDbInstanceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.RdsDbInstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the resource type @RDSDBInstance@ involved in
-- a GuardDuty finding.
--
-- /See:/ 'newRdsDbInstanceDetails' smart constructor.
data RdsDbInstanceDetails = RdsDbInstanceDetails'
  { -- | The identifier of the database cluster that contains the database
    -- instance ID involved in the finding.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the database instance
    -- involved in the finding.
    dbInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier associated to the database instance that was involved in
    -- the finding.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The database engine of the database instance involved in the finding.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The version of the database engine that was involved in the finding.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Instance tag key-value pairs associated with the database instance ID.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsDbInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifier', 'rdsDbInstanceDetails_dbClusterIdentifier' - The identifier of the database cluster that contains the database
-- instance ID involved in the finding.
--
-- 'dbInstanceArn', 'rdsDbInstanceDetails_dbInstanceArn' - The Amazon Resource Name (ARN) that identifies the database instance
-- involved in the finding.
--
-- 'dbInstanceIdentifier', 'rdsDbInstanceDetails_dbInstanceIdentifier' - The identifier associated to the database instance that was involved in
-- the finding.
--
-- 'engine', 'rdsDbInstanceDetails_engine' - The database engine of the database instance involved in the finding.
--
-- 'engineVersion', 'rdsDbInstanceDetails_engineVersion' - The version of the database engine that was involved in the finding.
--
-- 'tags', 'rdsDbInstanceDetails_tags' - Instance tag key-value pairs associated with the database instance ID.
newRdsDbInstanceDetails ::
  RdsDbInstanceDetails
newRdsDbInstanceDetails =
  RdsDbInstanceDetails'
    { dbClusterIdentifier =
        Prelude.Nothing,
      dbInstanceArn = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The identifier of the database cluster that contains the database
-- instance ID involved in the finding.
rdsDbInstanceDetails_dbClusterIdentifier :: Lens.Lens' RdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
rdsDbInstanceDetails_dbClusterIdentifier = Lens.lens (\RdsDbInstanceDetails' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RdsDbInstanceDetails' {} a -> s {dbClusterIdentifier = a} :: RdsDbInstanceDetails)

-- | The Amazon Resource Name (ARN) that identifies the database instance
-- involved in the finding.
rdsDbInstanceDetails_dbInstanceArn :: Lens.Lens' RdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
rdsDbInstanceDetails_dbInstanceArn = Lens.lens (\RdsDbInstanceDetails' {dbInstanceArn} -> dbInstanceArn) (\s@RdsDbInstanceDetails' {} a -> s {dbInstanceArn = a} :: RdsDbInstanceDetails)

-- | The identifier associated to the database instance that was involved in
-- the finding.
rdsDbInstanceDetails_dbInstanceIdentifier :: Lens.Lens' RdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
rdsDbInstanceDetails_dbInstanceIdentifier = Lens.lens (\RdsDbInstanceDetails' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RdsDbInstanceDetails' {} a -> s {dbInstanceIdentifier = a} :: RdsDbInstanceDetails)

-- | The database engine of the database instance involved in the finding.
rdsDbInstanceDetails_engine :: Lens.Lens' RdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
rdsDbInstanceDetails_engine = Lens.lens (\RdsDbInstanceDetails' {engine} -> engine) (\s@RdsDbInstanceDetails' {} a -> s {engine = a} :: RdsDbInstanceDetails)

-- | The version of the database engine that was involved in the finding.
rdsDbInstanceDetails_engineVersion :: Lens.Lens' RdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
rdsDbInstanceDetails_engineVersion = Lens.lens (\RdsDbInstanceDetails' {engineVersion} -> engineVersion) (\s@RdsDbInstanceDetails' {} a -> s {engineVersion = a} :: RdsDbInstanceDetails)

-- | Instance tag key-value pairs associated with the database instance ID.
rdsDbInstanceDetails_tags :: Lens.Lens' RdsDbInstanceDetails (Prelude.Maybe [Tag])
rdsDbInstanceDetails_tags = Lens.lens (\RdsDbInstanceDetails' {tags} -> tags) (\s@RdsDbInstanceDetails' {} a -> s {tags = a} :: RdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RdsDbInstanceDetails where
  parseJSON =
    Data.withObject
      "RdsDbInstanceDetails"
      ( \x ->
          RdsDbInstanceDetails'
            Prelude.<$> (x Data..:? "dbClusterIdentifier")
            Prelude.<*> (x Data..:? "dbInstanceArn")
            Prelude.<*> (x Data..:? "dbInstanceIdentifier")
            Prelude.<*> (x Data..:? "engine")
            Prelude.<*> (x Data..:? "engineVersion")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RdsDbInstanceDetails where
  hashWithSalt _salt RdsDbInstanceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` dbInstanceArn
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` tags

instance Prelude.NFData RdsDbInstanceDetails where
  rnf RdsDbInstanceDetails' {..} =
    Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceArn
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf tags
