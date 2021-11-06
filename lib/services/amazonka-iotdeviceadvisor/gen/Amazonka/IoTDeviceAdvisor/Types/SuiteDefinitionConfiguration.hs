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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Gets Suite Definition Configuration.
--
-- /See:/ 'newSuiteDefinitionConfiguration' smart constructor.
data SuiteDefinitionConfiguration = SuiteDefinitionConfiguration'
  { -- | Gets Suite Definition Configuration name.
    suiteDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | Gets the tests intended for qualification in a suite.
    intendedForQualification :: Prelude.Maybe Prelude.Bool,
    -- | Gets device permission arn.
    devicePermissionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Gets the devices configured.
    devices :: Prelude.Maybe [DeviceUnderTest],
    -- | Gets test suite root group.
    rootGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuiteDefinitionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suiteDefinitionName', 'suiteDefinitionConfiguration_suiteDefinitionName' - Gets Suite Definition Configuration name.
--
-- 'intendedForQualification', 'suiteDefinitionConfiguration_intendedForQualification' - Gets the tests intended for qualification in a suite.
--
-- 'devicePermissionRoleArn', 'suiteDefinitionConfiguration_devicePermissionRoleArn' - Gets device permission arn.
--
-- 'devices', 'suiteDefinitionConfiguration_devices' - Gets the devices configured.
--
-- 'rootGroup', 'suiteDefinitionConfiguration_rootGroup' - Gets test suite root group.
newSuiteDefinitionConfiguration ::
  SuiteDefinitionConfiguration
newSuiteDefinitionConfiguration =
  SuiteDefinitionConfiguration'
    { suiteDefinitionName =
        Prelude.Nothing,
      intendedForQualification = Prelude.Nothing,
      devicePermissionRoleArn = Prelude.Nothing,
      devices = Prelude.Nothing,
      rootGroup = Prelude.Nothing
    }

-- | Gets Suite Definition Configuration name.
suiteDefinitionConfiguration_suiteDefinitionName :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Text)
suiteDefinitionConfiguration_suiteDefinitionName = Lens.lens (\SuiteDefinitionConfiguration' {suiteDefinitionName} -> suiteDefinitionName) (\s@SuiteDefinitionConfiguration' {} a -> s {suiteDefinitionName = a} :: SuiteDefinitionConfiguration)

-- | Gets the tests intended for qualification in a suite.
suiteDefinitionConfiguration_intendedForQualification :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Bool)
suiteDefinitionConfiguration_intendedForQualification = Lens.lens (\SuiteDefinitionConfiguration' {intendedForQualification} -> intendedForQualification) (\s@SuiteDefinitionConfiguration' {} a -> s {intendedForQualification = a} :: SuiteDefinitionConfiguration)

-- | Gets device permission arn.
suiteDefinitionConfiguration_devicePermissionRoleArn :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Text)
suiteDefinitionConfiguration_devicePermissionRoleArn = Lens.lens (\SuiteDefinitionConfiguration' {devicePermissionRoleArn} -> devicePermissionRoleArn) (\s@SuiteDefinitionConfiguration' {} a -> s {devicePermissionRoleArn = a} :: SuiteDefinitionConfiguration)

-- | Gets the devices configured.
suiteDefinitionConfiguration_devices :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe [DeviceUnderTest])
suiteDefinitionConfiguration_devices = Lens.lens (\SuiteDefinitionConfiguration' {devices} -> devices) (\s@SuiteDefinitionConfiguration' {} a -> s {devices = a} :: SuiteDefinitionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Gets test suite root group.
suiteDefinitionConfiguration_rootGroup :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Text)
suiteDefinitionConfiguration_rootGroup = Lens.lens (\SuiteDefinitionConfiguration' {rootGroup} -> rootGroup) (\s@SuiteDefinitionConfiguration' {} a -> s {rootGroup = a} :: SuiteDefinitionConfiguration)

instance Core.FromJSON SuiteDefinitionConfiguration where
  parseJSON =
    Core.withObject
      "SuiteDefinitionConfiguration"
      ( \x ->
          SuiteDefinitionConfiguration'
            Prelude.<$> (x Core..:? "suiteDefinitionName")
            Prelude.<*> (x Core..:? "intendedForQualification")
            Prelude.<*> (x Core..:? "devicePermissionRoleArn")
            Prelude.<*> (x Core..:? "devices" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "rootGroup")
      )

instance
  Prelude.Hashable
    SuiteDefinitionConfiguration

instance Prelude.NFData SuiteDefinitionConfiguration

instance Core.ToJSON SuiteDefinitionConfiguration where
  toJSON SuiteDefinitionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("suiteDefinitionName" Core..=)
              Prelude.<$> suiteDefinitionName,
            ("intendedForQualification" Core..=)
              Prelude.<$> intendedForQualification,
            ("devicePermissionRoleArn" Core..=)
              Prelude.<$> devicePermissionRoleArn,
            ("devices" Core..=) Prelude.<$> devices,
            ("rootGroup" Core..=) Prelude.<$> rootGroup
          ]
      )
