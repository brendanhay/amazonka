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
-- Module      : Amazonka.Braket.Types.InstanceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.InstanceConfig where

import Amazonka.Braket.Types.InstanceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures the resource instances to use while running the Amazon Braket
-- hybrid job on Amazon Braket.
--
-- /See:/ 'newInstanceConfig' smart constructor.
data InstanceConfig = InstanceConfig'
  { -- | Configures the number of resource instances to use while running an
    -- Amazon Braket job on Amazon Braket. The default value is 1.
    instanceCount :: Prelude.Maybe Prelude.Natural,
    -- | Configures the type resource instances to use while running an Amazon
    -- Braket hybrid job.
    instanceType :: InstanceType,
    -- | The size of the storage volume, in GB, that user wants to provision.
    volumeSizeInGb :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCount', 'instanceConfig_instanceCount' - Configures the number of resource instances to use while running an
-- Amazon Braket job on Amazon Braket. The default value is 1.
--
-- 'instanceType', 'instanceConfig_instanceType' - Configures the type resource instances to use while running an Amazon
-- Braket hybrid job.
--
-- 'volumeSizeInGb', 'instanceConfig_volumeSizeInGb' - The size of the storage volume, in GB, that user wants to provision.
newInstanceConfig ::
  -- | 'instanceType'
  InstanceType ->
  -- | 'volumeSizeInGb'
  Prelude.Natural ->
  InstanceConfig
newInstanceConfig pInstanceType_ pVolumeSizeInGb_ =
  InstanceConfig'
    { instanceCount = Prelude.Nothing,
      instanceType = pInstanceType_,
      volumeSizeInGb = pVolumeSizeInGb_
    }

-- | Configures the number of resource instances to use while running an
-- Amazon Braket job on Amazon Braket. The default value is 1.
instanceConfig_instanceCount :: Lens.Lens' InstanceConfig (Prelude.Maybe Prelude.Natural)
instanceConfig_instanceCount = Lens.lens (\InstanceConfig' {instanceCount} -> instanceCount) (\s@InstanceConfig' {} a -> s {instanceCount = a} :: InstanceConfig)

-- | Configures the type resource instances to use while running an Amazon
-- Braket hybrid job.
instanceConfig_instanceType :: Lens.Lens' InstanceConfig InstanceType
instanceConfig_instanceType = Lens.lens (\InstanceConfig' {instanceType} -> instanceType) (\s@InstanceConfig' {} a -> s {instanceType = a} :: InstanceConfig)

-- | The size of the storage volume, in GB, that user wants to provision.
instanceConfig_volumeSizeInGb :: Lens.Lens' InstanceConfig Prelude.Natural
instanceConfig_volumeSizeInGb = Lens.lens (\InstanceConfig' {volumeSizeInGb} -> volumeSizeInGb) (\s@InstanceConfig' {} a -> s {volumeSizeInGb = a} :: InstanceConfig)

instance Data.FromJSON InstanceConfig where
  parseJSON =
    Data.withObject
      "InstanceConfig"
      ( \x ->
          InstanceConfig'
            Prelude.<$> (x Data..:? "instanceCount")
            Prelude.<*> (x Data..: "instanceType")
            Prelude.<*> (x Data..: "volumeSizeInGb")
      )

instance Prelude.Hashable InstanceConfig where
  hashWithSalt _salt InstanceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` volumeSizeInGb

instance Prelude.NFData InstanceConfig where
  rnf InstanceConfig' {..} =
    Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf volumeSizeInGb

instance Data.ToJSON InstanceConfig where
  toJSON InstanceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("instanceCount" Data..=) Prelude.<$> instanceCount,
            Prelude.Just ("instanceType" Data..= instanceType),
            Prelude.Just
              ("volumeSizeInGb" Data..= volumeSizeInGb)
          ]
      )
