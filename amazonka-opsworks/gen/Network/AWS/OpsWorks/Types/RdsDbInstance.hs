{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.OpsWorks.Types.RdsDbInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.RdsDbInstance where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon RDS instance.
--
-- /See:/ 'newRdsDbInstance' smart constructor.
data RdsDbInstance = RdsDbInstance'
  { -- | The instance\'s ARN.
    rdsDbInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The master user name.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s address.
    address :: Prelude.Maybe Prelude.Text,
    -- | The ID of the stack with which the instance is registered.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon
    -- RDS instance. AWS OpsWorks Stacks attempts to discover the instance only
    -- once. If this value is set to @true@, you must deregister the instance,
    -- and then register it again.
    missingOnRds :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance identifier.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual
    -- value.
    dbPassword :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s AWS region.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RdsDbInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rdsDbInstanceArn', 'rdsDbInstance_rdsDbInstanceArn' - The instance\'s ARN.
--
-- 'dbUser', 'rdsDbInstance_dbUser' - The master user name.
--
-- 'address', 'rdsDbInstance_address' - The instance\'s address.
--
-- 'stackId', 'rdsDbInstance_stackId' - The ID of the stack with which the instance is registered.
--
-- 'missingOnRds', 'rdsDbInstance_missingOnRds' - Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon
-- RDS instance. AWS OpsWorks Stacks attempts to discover the instance only
-- once. If this value is set to @true@, you must deregister the instance,
-- and then register it again.
--
-- 'dbInstanceIdentifier', 'rdsDbInstance_dbInstanceIdentifier' - The DB instance identifier.
--
-- 'dbPassword', 'rdsDbInstance_dbPassword' - AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual
-- value.
--
-- 'engine', 'rdsDbInstance_engine' - The instance\'s database engine.
--
-- 'region', 'rdsDbInstance_region' - The instance\'s AWS region.
newRdsDbInstance ::
  RdsDbInstance
newRdsDbInstance =
  RdsDbInstance'
    { rdsDbInstanceArn = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      address = Prelude.Nothing,
      stackId = Prelude.Nothing,
      missingOnRds = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbPassword = Prelude.Nothing,
      engine = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The instance\'s ARN.
rdsDbInstance_rdsDbInstanceArn :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_rdsDbInstanceArn = Lens.lens (\RdsDbInstance' {rdsDbInstanceArn} -> rdsDbInstanceArn) (\s@RdsDbInstance' {} a -> s {rdsDbInstanceArn = a} :: RdsDbInstance)

-- | The master user name.
rdsDbInstance_dbUser :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_dbUser = Lens.lens (\RdsDbInstance' {dbUser} -> dbUser) (\s@RdsDbInstance' {} a -> s {dbUser = a} :: RdsDbInstance)

-- | The instance\'s address.
rdsDbInstance_address :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_address = Lens.lens (\RdsDbInstance' {address} -> address) (\s@RdsDbInstance' {} a -> s {address = a} :: RdsDbInstance)

-- | The ID of the stack with which the instance is registered.
rdsDbInstance_stackId :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_stackId = Lens.lens (\RdsDbInstance' {stackId} -> stackId) (\s@RdsDbInstance' {} a -> s {stackId = a} :: RdsDbInstance)

-- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon
-- RDS instance. AWS OpsWorks Stacks attempts to discover the instance only
-- once. If this value is set to @true@, you must deregister the instance,
-- and then register it again.
rdsDbInstance_missingOnRds :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Bool)
rdsDbInstance_missingOnRds = Lens.lens (\RdsDbInstance' {missingOnRds} -> missingOnRds) (\s@RdsDbInstance' {} a -> s {missingOnRds = a} :: RdsDbInstance)

-- | The DB instance identifier.
rdsDbInstance_dbInstanceIdentifier :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_dbInstanceIdentifier = Lens.lens (\RdsDbInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RdsDbInstance' {} a -> s {dbInstanceIdentifier = a} :: RdsDbInstance)

-- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual
-- value.
rdsDbInstance_dbPassword :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_dbPassword = Lens.lens (\RdsDbInstance' {dbPassword} -> dbPassword) (\s@RdsDbInstance' {} a -> s {dbPassword = a} :: RdsDbInstance)

-- | The instance\'s database engine.
rdsDbInstance_engine :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_engine = Lens.lens (\RdsDbInstance' {engine} -> engine) (\s@RdsDbInstance' {} a -> s {engine = a} :: RdsDbInstance)

-- | The instance\'s AWS region.
rdsDbInstance_region :: Lens.Lens' RdsDbInstance (Prelude.Maybe Prelude.Text)
rdsDbInstance_region = Lens.lens (\RdsDbInstance' {region} -> region) (\s@RdsDbInstance' {} a -> s {region = a} :: RdsDbInstance)

instance Prelude.FromJSON RdsDbInstance where
  parseJSON =
    Prelude.withObject
      "RdsDbInstance"
      ( \x ->
          RdsDbInstance'
            Prelude.<$> (x Prelude..:? "RdsDbInstanceArn")
            Prelude.<*> (x Prelude..:? "DbUser")
            Prelude.<*> (x Prelude..:? "Address")
            Prelude.<*> (x Prelude..:? "StackId")
            Prelude.<*> (x Prelude..:? "MissingOnRds")
            Prelude.<*> (x Prelude..:? "DbInstanceIdentifier")
            Prelude.<*> (x Prelude..:? "DbPassword")
            Prelude.<*> (x Prelude..:? "Engine")
            Prelude.<*> (x Prelude..:? "Region")
      )

instance Prelude.Hashable RdsDbInstance

instance Prelude.NFData RdsDbInstance
