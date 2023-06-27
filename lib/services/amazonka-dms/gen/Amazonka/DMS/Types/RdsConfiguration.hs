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
-- Module      : Amazonka.DMS.Types.RdsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RdsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes the configuration of the recommended
-- target engine on Amazon RDS.
--
-- /See:/ 'newRdsConfiguration' smart constructor.
data RdsConfiguration = RdsConfiguration'
  { -- | Describes the deployment option for the recommended Amazon RDS DB
    -- instance. The deployment options include Multi-AZ and Single-AZ
    -- deployments. Valid values include @\"MULTI_AZ\"@ and @\"SINGLE_AZ\"@.
    deploymentOption :: Prelude.Maybe Prelude.Text,
    -- | Describes the recommended target Amazon RDS engine edition.
    engineEdition :: Prelude.Maybe Prelude.Text,
    -- | Describes the memory on the recommended Amazon RDS DB instance that
    -- meets your requirements.
    instanceMemory :: Prelude.Maybe Prelude.Double,
    -- | Describes the recommended target Amazon RDS instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Describes the number of virtual CPUs (vCPU) on the recommended Amazon
    -- RDS DB instance that meets your requirements.
    instanceVcpu :: Prelude.Maybe Prelude.Double,
    -- | Describes the number of I\/O operations completed each second (IOPS) on
    -- the recommended Amazon RDS DB instance that meets your requirements.
    storageIops :: Prelude.Maybe Prelude.Int,
    -- | Describes the storage size of the recommended Amazon RDS DB instance
    -- that meets your requirements.
    storageSize :: Prelude.Maybe Prelude.Int,
    -- | Describes the storage type of the recommended Amazon RDS DB instance
    -- that meets your requirements.
    --
    -- Amazon RDS provides three storage types: General Purpose SSD (also known
    -- as gp2 and gp3), Provisioned IOPS SSD (also known as io1), and magnetic
    -- (also known as standard).
    storageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentOption', 'rdsConfiguration_deploymentOption' - Describes the deployment option for the recommended Amazon RDS DB
-- instance. The deployment options include Multi-AZ and Single-AZ
-- deployments. Valid values include @\"MULTI_AZ\"@ and @\"SINGLE_AZ\"@.
--
-- 'engineEdition', 'rdsConfiguration_engineEdition' - Describes the recommended target Amazon RDS engine edition.
--
-- 'instanceMemory', 'rdsConfiguration_instanceMemory' - Describes the memory on the recommended Amazon RDS DB instance that
-- meets your requirements.
--
-- 'instanceType', 'rdsConfiguration_instanceType' - Describes the recommended target Amazon RDS instance type.
--
-- 'instanceVcpu', 'rdsConfiguration_instanceVcpu' - Describes the number of virtual CPUs (vCPU) on the recommended Amazon
-- RDS DB instance that meets your requirements.
--
-- 'storageIops', 'rdsConfiguration_storageIops' - Describes the number of I\/O operations completed each second (IOPS) on
-- the recommended Amazon RDS DB instance that meets your requirements.
--
-- 'storageSize', 'rdsConfiguration_storageSize' - Describes the storage size of the recommended Amazon RDS DB instance
-- that meets your requirements.
--
-- 'storageType', 'rdsConfiguration_storageType' - Describes the storage type of the recommended Amazon RDS DB instance
-- that meets your requirements.
--
-- Amazon RDS provides three storage types: General Purpose SSD (also known
-- as gp2 and gp3), Provisioned IOPS SSD (also known as io1), and magnetic
-- (also known as standard).
newRdsConfiguration ::
  RdsConfiguration
newRdsConfiguration =
  RdsConfiguration'
    { deploymentOption =
        Prelude.Nothing,
      engineEdition = Prelude.Nothing,
      instanceMemory = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      instanceVcpu = Prelude.Nothing,
      storageIops = Prelude.Nothing,
      storageSize = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | Describes the deployment option for the recommended Amazon RDS DB
-- instance. The deployment options include Multi-AZ and Single-AZ
-- deployments. Valid values include @\"MULTI_AZ\"@ and @\"SINGLE_AZ\"@.
rdsConfiguration_deploymentOption :: Lens.Lens' RdsConfiguration (Prelude.Maybe Prelude.Text)
rdsConfiguration_deploymentOption = Lens.lens (\RdsConfiguration' {deploymentOption} -> deploymentOption) (\s@RdsConfiguration' {} a -> s {deploymentOption = a} :: RdsConfiguration)

-- | Describes the recommended target Amazon RDS engine edition.
rdsConfiguration_engineEdition :: Lens.Lens' RdsConfiguration (Prelude.Maybe Prelude.Text)
rdsConfiguration_engineEdition = Lens.lens (\RdsConfiguration' {engineEdition} -> engineEdition) (\s@RdsConfiguration' {} a -> s {engineEdition = a} :: RdsConfiguration)

-- | Describes the memory on the recommended Amazon RDS DB instance that
-- meets your requirements.
rdsConfiguration_instanceMemory :: Lens.Lens' RdsConfiguration (Prelude.Maybe Prelude.Double)
rdsConfiguration_instanceMemory = Lens.lens (\RdsConfiguration' {instanceMemory} -> instanceMemory) (\s@RdsConfiguration' {} a -> s {instanceMemory = a} :: RdsConfiguration)

-- | Describes the recommended target Amazon RDS instance type.
rdsConfiguration_instanceType :: Lens.Lens' RdsConfiguration (Prelude.Maybe Prelude.Text)
rdsConfiguration_instanceType = Lens.lens (\RdsConfiguration' {instanceType} -> instanceType) (\s@RdsConfiguration' {} a -> s {instanceType = a} :: RdsConfiguration)

-- | Describes the number of virtual CPUs (vCPU) on the recommended Amazon
-- RDS DB instance that meets your requirements.
rdsConfiguration_instanceVcpu :: Lens.Lens' RdsConfiguration (Prelude.Maybe Prelude.Double)
rdsConfiguration_instanceVcpu = Lens.lens (\RdsConfiguration' {instanceVcpu} -> instanceVcpu) (\s@RdsConfiguration' {} a -> s {instanceVcpu = a} :: RdsConfiguration)

-- | Describes the number of I\/O operations completed each second (IOPS) on
-- the recommended Amazon RDS DB instance that meets your requirements.
rdsConfiguration_storageIops :: Lens.Lens' RdsConfiguration (Prelude.Maybe Prelude.Int)
rdsConfiguration_storageIops = Lens.lens (\RdsConfiguration' {storageIops} -> storageIops) (\s@RdsConfiguration' {} a -> s {storageIops = a} :: RdsConfiguration)

-- | Describes the storage size of the recommended Amazon RDS DB instance
-- that meets your requirements.
rdsConfiguration_storageSize :: Lens.Lens' RdsConfiguration (Prelude.Maybe Prelude.Int)
rdsConfiguration_storageSize = Lens.lens (\RdsConfiguration' {storageSize} -> storageSize) (\s@RdsConfiguration' {} a -> s {storageSize = a} :: RdsConfiguration)

-- | Describes the storage type of the recommended Amazon RDS DB instance
-- that meets your requirements.
--
-- Amazon RDS provides three storage types: General Purpose SSD (also known
-- as gp2 and gp3), Provisioned IOPS SSD (also known as io1), and magnetic
-- (also known as standard).
rdsConfiguration_storageType :: Lens.Lens' RdsConfiguration (Prelude.Maybe Prelude.Text)
rdsConfiguration_storageType = Lens.lens (\RdsConfiguration' {storageType} -> storageType) (\s@RdsConfiguration' {} a -> s {storageType = a} :: RdsConfiguration)

instance Data.FromJSON RdsConfiguration where
  parseJSON =
    Data.withObject
      "RdsConfiguration"
      ( \x ->
          RdsConfiguration'
            Prelude.<$> (x Data..:? "DeploymentOption")
            Prelude.<*> (x Data..:? "EngineEdition")
            Prelude.<*> (x Data..:? "InstanceMemory")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "InstanceVcpu")
            Prelude.<*> (x Data..:? "StorageIops")
            Prelude.<*> (x Data..:? "StorageSize")
            Prelude.<*> (x Data..:? "StorageType")
      )

instance Prelude.Hashable RdsConfiguration where
  hashWithSalt _salt RdsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentOption
      `Prelude.hashWithSalt` engineEdition
      `Prelude.hashWithSalt` instanceMemory
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instanceVcpu
      `Prelude.hashWithSalt` storageIops
      `Prelude.hashWithSalt` storageSize
      `Prelude.hashWithSalt` storageType

instance Prelude.NFData RdsConfiguration where
  rnf RdsConfiguration' {..} =
    Prelude.rnf deploymentOption
      `Prelude.seq` Prelude.rnf engineEdition
      `Prelude.seq` Prelude.rnf instanceMemory
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceVcpu
      `Prelude.seq` Prelude.rnf storageIops
      `Prelude.seq` Prelude.rnf storageSize
      `Prelude.seq` Prelude.rnf storageType
