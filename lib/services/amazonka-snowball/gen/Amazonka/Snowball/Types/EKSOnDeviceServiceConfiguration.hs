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
-- Module      : Amazonka.Snowball.Types.EKSOnDeviceServiceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.EKSOnDeviceServiceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the metadata and configuration settings of EKS
-- Anywhere on the Snow Family device.
--
-- /See:/ 'newEKSOnDeviceServiceConfiguration' smart constructor.
data EKSOnDeviceServiceConfiguration = EKSOnDeviceServiceConfiguration'
  { -- | The version of EKS Anywhere on the Snow Family device.
    eKSAnywhereVersion :: Prelude.Maybe Prelude.Text,
    -- | The Kubernetes version for EKS Anywhere on the Snow Family device.
    kubernetesVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EKSOnDeviceServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eKSAnywhereVersion', 'eKSOnDeviceServiceConfiguration_eKSAnywhereVersion' - The version of EKS Anywhere on the Snow Family device.
--
-- 'kubernetesVersion', 'eKSOnDeviceServiceConfiguration_kubernetesVersion' - The Kubernetes version for EKS Anywhere on the Snow Family device.
newEKSOnDeviceServiceConfiguration ::
  EKSOnDeviceServiceConfiguration
newEKSOnDeviceServiceConfiguration =
  EKSOnDeviceServiceConfiguration'
    { eKSAnywhereVersion =
        Prelude.Nothing,
      kubernetesVersion = Prelude.Nothing
    }

-- | The version of EKS Anywhere on the Snow Family device.
eKSOnDeviceServiceConfiguration_eKSAnywhereVersion :: Lens.Lens' EKSOnDeviceServiceConfiguration (Prelude.Maybe Prelude.Text)
eKSOnDeviceServiceConfiguration_eKSAnywhereVersion = Lens.lens (\EKSOnDeviceServiceConfiguration' {eKSAnywhereVersion} -> eKSAnywhereVersion) (\s@EKSOnDeviceServiceConfiguration' {} a -> s {eKSAnywhereVersion = a} :: EKSOnDeviceServiceConfiguration)

-- | The Kubernetes version for EKS Anywhere on the Snow Family device.
eKSOnDeviceServiceConfiguration_kubernetesVersion :: Lens.Lens' EKSOnDeviceServiceConfiguration (Prelude.Maybe Prelude.Text)
eKSOnDeviceServiceConfiguration_kubernetesVersion = Lens.lens (\EKSOnDeviceServiceConfiguration' {kubernetesVersion} -> kubernetesVersion) (\s@EKSOnDeviceServiceConfiguration' {} a -> s {kubernetesVersion = a} :: EKSOnDeviceServiceConfiguration)

instance
  Data.FromJSON
    EKSOnDeviceServiceConfiguration
  where
  parseJSON =
    Data.withObject
      "EKSOnDeviceServiceConfiguration"
      ( \x ->
          EKSOnDeviceServiceConfiguration'
            Prelude.<$> (x Data..:? "EKSAnywhereVersion")
            Prelude.<*> (x Data..:? "KubernetesVersion")
      )

instance
  Prelude.Hashable
    EKSOnDeviceServiceConfiguration
  where
  hashWithSalt
    _salt
    EKSOnDeviceServiceConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` eKSAnywhereVersion
        `Prelude.hashWithSalt` kubernetesVersion

instance
  Prelude.NFData
    EKSOnDeviceServiceConfiguration
  where
  rnf EKSOnDeviceServiceConfiguration' {..} =
    Prelude.rnf eKSAnywhereVersion
      `Prelude.seq` Prelude.rnf kubernetesVersion

instance Data.ToJSON EKSOnDeviceServiceConfiguration where
  toJSON EKSOnDeviceServiceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EKSAnywhereVersion" Data..=)
              Prelude.<$> eKSAnywhereVersion,
            ("KubernetesVersion" Data..=)
              Prelude.<$> kubernetesVersion
          ]
      )
