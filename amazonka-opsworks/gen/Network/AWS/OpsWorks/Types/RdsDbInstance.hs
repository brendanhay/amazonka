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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an Amazon RDS instance.
--
-- /See:/ 'newRdsDbInstance' smart constructor.
data RdsDbInstance = RdsDbInstance'
  { -- | The instance\'s ARN.
    rdsDbInstanceArn :: Core.Maybe Core.Text,
    -- | The master user name.
    dbUser :: Core.Maybe Core.Text,
    -- | The instance\'s address.
    address :: Core.Maybe Core.Text,
    -- | The ID of the stack with which the instance is registered.
    stackId :: Core.Maybe Core.Text,
    -- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon
    -- RDS instance. AWS OpsWorks Stacks attempts to discover the instance only
    -- once. If this value is set to @true@, you must deregister the instance,
    -- and then register it again.
    missingOnRds :: Core.Maybe Core.Bool,
    -- | The DB instance identifier.
    dbInstanceIdentifier :: Core.Maybe Core.Text,
    -- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual
    -- value.
    dbPassword :: Core.Maybe Core.Text,
    -- | The instance\'s database engine.
    engine :: Core.Maybe Core.Text,
    -- | The instance\'s AWS region.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { rdsDbInstanceArn = Core.Nothing,
      dbUser = Core.Nothing,
      address = Core.Nothing,
      stackId = Core.Nothing,
      missingOnRds = Core.Nothing,
      dbInstanceIdentifier = Core.Nothing,
      dbPassword = Core.Nothing,
      engine = Core.Nothing,
      region = Core.Nothing
    }

-- | The instance\'s ARN.
rdsDbInstance_rdsDbInstanceArn :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdsDbInstance_rdsDbInstanceArn = Lens.lens (\RdsDbInstance' {rdsDbInstanceArn} -> rdsDbInstanceArn) (\s@RdsDbInstance' {} a -> s {rdsDbInstanceArn = a} :: RdsDbInstance)

-- | The master user name.
rdsDbInstance_dbUser :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdsDbInstance_dbUser = Lens.lens (\RdsDbInstance' {dbUser} -> dbUser) (\s@RdsDbInstance' {} a -> s {dbUser = a} :: RdsDbInstance)

-- | The instance\'s address.
rdsDbInstance_address :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdsDbInstance_address = Lens.lens (\RdsDbInstance' {address} -> address) (\s@RdsDbInstance' {} a -> s {address = a} :: RdsDbInstance)

-- | The ID of the stack with which the instance is registered.
rdsDbInstance_stackId :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdsDbInstance_stackId = Lens.lens (\RdsDbInstance' {stackId} -> stackId) (\s@RdsDbInstance' {} a -> s {stackId = a} :: RdsDbInstance)

-- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon
-- RDS instance. AWS OpsWorks Stacks attempts to discover the instance only
-- once. If this value is set to @true@, you must deregister the instance,
-- and then register it again.
rdsDbInstance_missingOnRds :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Bool)
rdsDbInstance_missingOnRds = Lens.lens (\RdsDbInstance' {missingOnRds} -> missingOnRds) (\s@RdsDbInstance' {} a -> s {missingOnRds = a} :: RdsDbInstance)

-- | The DB instance identifier.
rdsDbInstance_dbInstanceIdentifier :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdsDbInstance_dbInstanceIdentifier = Lens.lens (\RdsDbInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RdsDbInstance' {} a -> s {dbInstanceIdentifier = a} :: RdsDbInstance)

-- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual
-- value.
rdsDbInstance_dbPassword :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdsDbInstance_dbPassword = Lens.lens (\RdsDbInstance' {dbPassword} -> dbPassword) (\s@RdsDbInstance' {} a -> s {dbPassword = a} :: RdsDbInstance)

-- | The instance\'s database engine.
rdsDbInstance_engine :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdsDbInstance_engine = Lens.lens (\RdsDbInstance' {engine} -> engine) (\s@RdsDbInstance' {} a -> s {engine = a} :: RdsDbInstance)

-- | The instance\'s AWS region.
rdsDbInstance_region :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdsDbInstance_region = Lens.lens (\RdsDbInstance' {region} -> region) (\s@RdsDbInstance' {} a -> s {region = a} :: RdsDbInstance)

instance Core.FromJSON RdsDbInstance where
  parseJSON =
    Core.withObject
      "RdsDbInstance"
      ( \x ->
          RdsDbInstance'
            Core.<$> (x Core..:? "RdsDbInstanceArn")
            Core.<*> (x Core..:? "DbUser")
            Core.<*> (x Core..:? "Address")
            Core.<*> (x Core..:? "StackId")
            Core.<*> (x Core..:? "MissingOnRds")
            Core.<*> (x Core..:? "DbInstanceIdentifier")
            Core.<*> (x Core..:? "DbPassword")
            Core.<*> (x Core..:? "Engine")
            Core.<*> (x Core..:? "Region")
      )

instance Core.Hashable RdsDbInstance

instance Core.NFData RdsDbInstance
