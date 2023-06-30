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
-- Module      : Amazonka.OpsWorks.Types.RdsDbInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.RdsDbInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon RDS instance.
--
-- /See:/ 'newRdsDbInstance' smart constructor.
data RdsDbInstance = RdsDbInstance'
  { -- | The instance\'s address.
    address :: Prelude.Maybe Prelude.Text,
    -- | The DB instance identifier.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual
    -- value.
    dbPassword :: Prelude.Maybe Prelude.Text,
    -- | The master user name.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon
    -- RDS instance. AWS OpsWorks Stacks attempts to discover the instance only
    -- once. If this value is set to @true@, you must deregister the instance,
    -- and then register it again.
    missingOnRds :: Prelude.Maybe Prelude.Bool,
    -- | The instance\'s ARN.
    rdsDbInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s AWS region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The ID of the stack with which the instance is registered.
    stackId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsDbInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'rdsDbInstance_address' - The instance\'s address.
--
-- 'dbInstanceIdentifier', 'rdsDbInstance_dbInstanceIdentifier' - The DB instance identifier.
--
-- 'dbPassword', 'rdsDbInstance_dbPassword' - AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual
-- value.
--
-- 'dbUser', 'rdsDbInstance_dbUser' - The master user name.
--
-- 'engine', 'rdsDbInstance_engine' - The instance\'s database engine.
--
-- 'missingOnRds', 'rdsDbInstance_missingOnRds' - Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon
-- RDS instance. AWS OpsWorks Stacks attempts to discover the instance only
-- once. If this value is set to @true@, you must deregister the instance,
-- and then register it again.
--
-- 'rdsDbInstanceArn', 'rdsDbInstance_rdsDbInstanceArn' - The instance\'s ARN.
--
-- 'region', 'rdsDbInstance_region' - The instance\'s AWS region.
--
-- 'stackId', 'rdsDbInstance_stackId' - The ID of the stack with which the instance is registered.
newRdsDbInstance ::
  RdsDbInstance
newRdsDbInstance =
  RdsDbInstance'
    { address = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbPassword = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      engine = Prelude.Nothing,
      missingOnRds = Prelude.Nothing,
      rdsDbInstanceArn = Prelude.Nothing,
      region = Prelude.Nothing,
      stackId = Prelude.Nothing
    }

-- | The instance\'s address.
rdsDbInstance_address :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_address = Lens.lens (\RdsDbInstance' {address} -> address) (\s@RdsDbInstance' {} a -> s {address = a} :: RdsDbInstance)

-- | The DB instance identifier.
rdsDbInstance_dbInstanceIdentifier :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_dbInstanceIdentifier = Lens.lens (\RdsDbInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RdsDbInstance' {} a -> s {dbInstanceIdentifier = a} :: RdsDbInstance)

-- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual
-- value.
rdsDbInstance_dbPassword :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_dbPassword = Lens.lens (\RdsDbInstance' {dbPassword} -> dbPassword) (\s@RdsDbInstance' {} a -> s {dbPassword = a} :: RdsDbInstance)

-- | The master user name.
rdsDbInstance_dbUser :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_dbUser = Lens.lens (\RdsDbInstance' {dbUser} -> dbUser) (\s@RdsDbInstance' {} a -> s {dbUser = a} :: RdsDbInstance)

-- | The instance\'s database engine.
rdsDbInstance_engine :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_engine = Lens.lens (\RdsDbInstance' {engine} -> engine) (\s@RdsDbInstance' {} a -> s {engine = a} :: RdsDbInstance)

-- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon
-- RDS instance. AWS OpsWorks Stacks attempts to discover the instance only
-- once. If this value is set to @true@, you must deregister the instance,
-- and then register it again.
rdsDbInstance_missingOnRds :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Bool)
rdsDbInstance_missingOnRds = Lens.lens (\RdsDbInstance' {missingOnRds} -> missingOnRds) (\s@RdsDbInstance' {} a -> s {missingOnRds = a} :: RdsDbInstance)

-- | The instance\'s ARN.
rdsDbInstance_rdsDbInstanceArn :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_rdsDbInstanceArn = Lens.lens (\RdsDbInstance' {rdsDbInstanceArn} -> rdsDbInstanceArn) (\s@RdsDbInstance' {} a -> s {rdsDbInstanceArn = a} :: RdsDbInstance)

-- | The instance\'s AWS region.
rdsDbInstance_region :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_region = Lens.lens (\RdsDbInstance' {region} -> region) (\s@RdsDbInstance' {} a -> s {region = a} :: RdsDbInstance)

-- | The ID of the stack with which the instance is registered.
rdsDbInstance_stackId :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_stackId = Lens.lens (\RdsDbInstance' {stackId} -> stackId) (\s@RdsDbInstance' {} a -> s {stackId = a} :: RdsDbInstance)

instance Data.FromJSON RdsDbInstance where
  parseJSON =
    Data.withObject
      "RdsDbInstance"
      ( \x ->
          RdsDbInstance'
            Prelude.<$> (x Data..:? "Address")
            Prelude.<*> (x Data..:? "DbInstanceIdentifier")
            Prelude.<*> (x Data..:? "DbPassword")
            Prelude.<*> (x Data..:? "DbUser")
            Prelude.<*> (x Data..:? "Engine")
            Prelude.<*> (x Data..:? "MissingOnRds")
            Prelude.<*> (x Data..:? "RdsDbInstanceArn")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "StackId")
      )

instance Prelude.Hashable RdsDbInstance where
  hashWithSalt _salt RdsDbInstance' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbPassword
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` missingOnRds
      `Prelude.hashWithSalt` rdsDbInstanceArn
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData RdsDbInstance where
  rnf RdsDbInstance' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbPassword
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf missingOnRds
      `Prelude.seq` Prelude.rnf rdsDbInstanceArn
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf stackId
