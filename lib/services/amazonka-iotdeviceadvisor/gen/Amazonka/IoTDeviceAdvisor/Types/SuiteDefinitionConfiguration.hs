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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest
import Amazonka.IoTDeviceAdvisor.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | Gets Suite Definition Configuration.
--
-- /See:/ 'newSuiteDefinitionConfiguration' smart constructor.
data SuiteDefinitionConfiguration = SuiteDefinitionConfiguration'
  { -- | Gets test suite root group.
    rootGroup :: Prelude.Maybe Prelude.Text,
    -- | Gets the devices configured.
    devices :: Prelude.Maybe [DeviceUnderTest],
    -- | Gets Suite Definition Configuration name.
    suiteDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | Gets the tests intended for qualification in a suite.
    intendedForQualification :: Prelude.Maybe Prelude.Bool,
    -- | Gets the MQTT protocol that is configured in the suite definition.
    protocol :: Prelude.Maybe Protocol,
    -- | Verifies if the test suite is a long duration test.
    isLongDurationTest :: Prelude.Maybe Prelude.Bool,
    -- | Gets the device permission ARN.
    devicePermissionRoleArn :: Prelude.Maybe Prelude.Text
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
-- 'rootGroup', 'suiteDefinitionConfiguration_rootGroup' - Gets test suite root group.
--
-- 'devices', 'suiteDefinitionConfiguration_devices' - Gets the devices configured.
--
-- 'suiteDefinitionName', 'suiteDefinitionConfiguration_suiteDefinitionName' - Gets Suite Definition Configuration name.
--
-- 'intendedForQualification', 'suiteDefinitionConfiguration_intendedForQualification' - Gets the tests intended for qualification in a suite.
--
-- 'protocol', 'suiteDefinitionConfiguration_protocol' - Gets the MQTT protocol that is configured in the suite definition.
--
-- 'isLongDurationTest', 'suiteDefinitionConfiguration_isLongDurationTest' - Verifies if the test suite is a long duration test.
--
-- 'devicePermissionRoleArn', 'suiteDefinitionConfiguration_devicePermissionRoleArn' - Gets the device permission ARN.
newSuiteDefinitionConfiguration ::
  SuiteDefinitionConfiguration
newSuiteDefinitionConfiguration =
  SuiteDefinitionConfiguration'
    { rootGroup =
        Prelude.Nothing,
      devices = Prelude.Nothing,
      suiteDefinitionName = Prelude.Nothing,
      intendedForQualification = Prelude.Nothing,
      protocol = Prelude.Nothing,
      isLongDurationTest = Prelude.Nothing,
      devicePermissionRoleArn = Prelude.Nothing
    }

-- | Gets test suite root group.
suiteDefinitionConfiguration_rootGroup :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Text)
suiteDefinitionConfiguration_rootGroup = Lens.lens (\SuiteDefinitionConfiguration' {rootGroup} -> rootGroup) (\s@SuiteDefinitionConfiguration' {} a -> s {rootGroup = a} :: SuiteDefinitionConfiguration)

-- | Gets the devices configured.
suiteDefinitionConfiguration_devices :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe [DeviceUnderTest])
suiteDefinitionConfiguration_devices = Lens.lens (\SuiteDefinitionConfiguration' {devices} -> devices) (\s@SuiteDefinitionConfiguration' {} a -> s {devices = a} :: SuiteDefinitionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Gets Suite Definition Configuration name.
suiteDefinitionConfiguration_suiteDefinitionName :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Text)
suiteDefinitionConfiguration_suiteDefinitionName = Lens.lens (\SuiteDefinitionConfiguration' {suiteDefinitionName} -> suiteDefinitionName) (\s@SuiteDefinitionConfiguration' {} a -> s {suiteDefinitionName = a} :: SuiteDefinitionConfiguration)

-- | Gets the tests intended for qualification in a suite.
suiteDefinitionConfiguration_intendedForQualification :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Bool)
suiteDefinitionConfiguration_intendedForQualification = Lens.lens (\SuiteDefinitionConfiguration' {intendedForQualification} -> intendedForQualification) (\s@SuiteDefinitionConfiguration' {} a -> s {intendedForQualification = a} :: SuiteDefinitionConfiguration)

-- | Gets the MQTT protocol that is configured in the suite definition.
suiteDefinitionConfiguration_protocol :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Protocol)
suiteDefinitionConfiguration_protocol = Lens.lens (\SuiteDefinitionConfiguration' {protocol} -> protocol) (\s@SuiteDefinitionConfiguration' {} a -> s {protocol = a} :: SuiteDefinitionConfiguration)

-- | Verifies if the test suite is a long duration test.
suiteDefinitionConfiguration_isLongDurationTest :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Bool)
suiteDefinitionConfiguration_isLongDurationTest = Lens.lens (\SuiteDefinitionConfiguration' {isLongDurationTest} -> isLongDurationTest) (\s@SuiteDefinitionConfiguration' {} a -> s {isLongDurationTest = a} :: SuiteDefinitionConfiguration)

-- | Gets the device permission ARN.
suiteDefinitionConfiguration_devicePermissionRoleArn :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Text)
suiteDefinitionConfiguration_devicePermissionRoleArn = Lens.lens (\SuiteDefinitionConfiguration' {devicePermissionRoleArn} -> devicePermissionRoleArn) (\s@SuiteDefinitionConfiguration' {} a -> s {devicePermissionRoleArn = a} :: SuiteDefinitionConfiguration)

instance Data.FromJSON SuiteDefinitionConfiguration where
  parseJSON =
    Data.withObject
      "SuiteDefinitionConfiguration"
      ( \x ->
          SuiteDefinitionConfiguration'
            Prelude.<$> (x Data..:? "rootGroup")
            Prelude.<*> (x Data..:? "devices" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "suiteDefinitionName")
            Prelude.<*> (x Data..:? "intendedForQualification")
            Prelude.<*> (x Data..:? "protocol")
            Prelude.<*> (x Data..:? "isLongDurationTest")
            Prelude.<*> (x Data..:? "devicePermissionRoleArn")
      )

instance
  Prelude.Hashable
    SuiteDefinitionConfiguration
  where
  hashWithSalt _salt SuiteDefinitionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` rootGroup
      `Prelude.hashWithSalt` devices
      `Prelude.hashWithSalt` suiteDefinitionName
      `Prelude.hashWithSalt` intendedForQualification
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` isLongDurationTest
      `Prelude.hashWithSalt` devicePermissionRoleArn

instance Prelude.NFData SuiteDefinitionConfiguration where
  rnf SuiteDefinitionConfiguration' {..} =
    Prelude.rnf rootGroup
      `Prelude.seq` Prelude.rnf devices
      `Prelude.seq` Prelude.rnf suiteDefinitionName
      `Prelude.seq` Prelude.rnf intendedForQualification
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf isLongDurationTest
      `Prelude.seq` Prelude.rnf devicePermissionRoleArn

instance Data.ToJSON SuiteDefinitionConfiguration where
  toJSON SuiteDefinitionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("rootGroup" Data..=) Prelude.<$> rootGroup,
            ("devices" Data..=) Prelude.<$> devices,
            ("suiteDefinitionName" Data..=)
              Prelude.<$> suiteDefinitionName,
            ("intendedForQualification" Data..=)
              Prelude.<$> intendedForQualification,
            ("protocol" Data..=) Prelude.<$> protocol,
            ("isLongDurationTest" Data..=)
              Prelude.<$> isLongDurationTest,
            ("devicePermissionRoleArn" Data..=)
              Prelude.<$> devicePermissionRoleArn
          ]
      )
