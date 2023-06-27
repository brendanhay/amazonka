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
-- Module      : Amazonka.DMS.Types.RdsRequirements
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RdsRequirements where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes the requirements to the target
-- engine on Amazon RDS.
--
-- /See:/ 'newRdsRequirements' smart constructor.
data RdsRequirements = RdsRequirements'
  { -- | The required deployment option for the Amazon RDS DB instance. Valid
    -- values include @\"MULTI_AZ\"@ for Multi-AZ deployments and
    -- @\"SINGLE_AZ\"@ for Single-AZ deployments.
    deploymentOption :: Prelude.Maybe Prelude.Text,
    -- | The required target Amazon RDS engine edition.
    engineEdition :: Prelude.Maybe Prelude.Text,
    -- | The required memory on the Amazon RDS DB instance.
    instanceMemory :: Prelude.Maybe Prelude.Double,
    -- | The required number of virtual CPUs (vCPU) on the Amazon RDS DB
    -- instance.
    instanceVcpu :: Prelude.Maybe Prelude.Double,
    -- | The required number of I\/O operations completed each second (IOPS) on
    -- your Amazon RDS DB instance.
    storageIops :: Prelude.Maybe Prelude.Int,
    -- | The required Amazon RDS DB instance storage size.
    storageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsRequirements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentOption', 'rdsRequirements_deploymentOption' - The required deployment option for the Amazon RDS DB instance. Valid
-- values include @\"MULTI_AZ\"@ for Multi-AZ deployments and
-- @\"SINGLE_AZ\"@ for Single-AZ deployments.
--
-- 'engineEdition', 'rdsRequirements_engineEdition' - The required target Amazon RDS engine edition.
--
-- 'instanceMemory', 'rdsRequirements_instanceMemory' - The required memory on the Amazon RDS DB instance.
--
-- 'instanceVcpu', 'rdsRequirements_instanceVcpu' - The required number of virtual CPUs (vCPU) on the Amazon RDS DB
-- instance.
--
-- 'storageIops', 'rdsRequirements_storageIops' - The required number of I\/O operations completed each second (IOPS) on
-- your Amazon RDS DB instance.
--
-- 'storageSize', 'rdsRequirements_storageSize' - The required Amazon RDS DB instance storage size.
newRdsRequirements ::
  RdsRequirements
newRdsRequirements =
  RdsRequirements'
    { deploymentOption =
        Prelude.Nothing,
      engineEdition = Prelude.Nothing,
      instanceMemory = Prelude.Nothing,
      instanceVcpu = Prelude.Nothing,
      storageIops = Prelude.Nothing,
      storageSize = Prelude.Nothing
    }

-- | The required deployment option for the Amazon RDS DB instance. Valid
-- values include @\"MULTI_AZ\"@ for Multi-AZ deployments and
-- @\"SINGLE_AZ\"@ for Single-AZ deployments.
rdsRequirements_deploymentOption :: Lens.Lens' RdsRequirements (Prelude.Maybe Prelude.Text)
rdsRequirements_deploymentOption = Lens.lens (\RdsRequirements' {deploymentOption} -> deploymentOption) (\s@RdsRequirements' {} a -> s {deploymentOption = a} :: RdsRequirements)

-- | The required target Amazon RDS engine edition.
rdsRequirements_engineEdition :: Lens.Lens' RdsRequirements (Prelude.Maybe Prelude.Text)
rdsRequirements_engineEdition = Lens.lens (\RdsRequirements' {engineEdition} -> engineEdition) (\s@RdsRequirements' {} a -> s {engineEdition = a} :: RdsRequirements)

-- | The required memory on the Amazon RDS DB instance.
rdsRequirements_instanceMemory :: Lens.Lens' RdsRequirements (Prelude.Maybe Prelude.Double)
rdsRequirements_instanceMemory = Lens.lens (\RdsRequirements' {instanceMemory} -> instanceMemory) (\s@RdsRequirements' {} a -> s {instanceMemory = a} :: RdsRequirements)

-- | The required number of virtual CPUs (vCPU) on the Amazon RDS DB
-- instance.
rdsRequirements_instanceVcpu :: Lens.Lens' RdsRequirements (Prelude.Maybe Prelude.Double)
rdsRequirements_instanceVcpu = Lens.lens (\RdsRequirements' {instanceVcpu} -> instanceVcpu) (\s@RdsRequirements' {} a -> s {instanceVcpu = a} :: RdsRequirements)

-- | The required number of I\/O operations completed each second (IOPS) on
-- your Amazon RDS DB instance.
rdsRequirements_storageIops :: Lens.Lens' RdsRequirements (Prelude.Maybe Prelude.Int)
rdsRequirements_storageIops = Lens.lens (\RdsRequirements' {storageIops} -> storageIops) (\s@RdsRequirements' {} a -> s {storageIops = a} :: RdsRequirements)

-- | The required Amazon RDS DB instance storage size.
rdsRequirements_storageSize :: Lens.Lens' RdsRequirements (Prelude.Maybe Prelude.Int)
rdsRequirements_storageSize = Lens.lens (\RdsRequirements' {storageSize} -> storageSize) (\s@RdsRequirements' {} a -> s {storageSize = a} :: RdsRequirements)

instance Data.FromJSON RdsRequirements where
  parseJSON =
    Data.withObject
      "RdsRequirements"
      ( \x ->
          RdsRequirements'
            Prelude.<$> (x Data..:? "DeploymentOption")
            Prelude.<*> (x Data..:? "EngineEdition")
            Prelude.<*> (x Data..:? "InstanceMemory")
            Prelude.<*> (x Data..:? "InstanceVcpu")
            Prelude.<*> (x Data..:? "StorageIops")
            Prelude.<*> (x Data..:? "StorageSize")
      )

instance Prelude.Hashable RdsRequirements where
  hashWithSalt _salt RdsRequirements' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentOption
      `Prelude.hashWithSalt` engineEdition
      `Prelude.hashWithSalt` instanceMemory
      `Prelude.hashWithSalt` instanceVcpu
      `Prelude.hashWithSalt` storageIops
      `Prelude.hashWithSalt` storageSize

instance Prelude.NFData RdsRequirements where
  rnf RdsRequirements' {..} =
    Prelude.rnf deploymentOption
      `Prelude.seq` Prelude.rnf engineEdition
      `Prelude.seq` Prelude.rnf instanceMemory
      `Prelude.seq` Prelude.rnf instanceVcpu
      `Prelude.seq` Prelude.rnf storageIops
      `Prelude.seq` Prelude.rnf storageSize
