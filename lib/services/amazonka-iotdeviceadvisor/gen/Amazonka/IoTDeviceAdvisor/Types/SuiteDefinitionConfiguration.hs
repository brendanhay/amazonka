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
-- Copyright   : (c) 2013-2023 Brendan Hay
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

-- | Gets the suite definition configuration.
--
-- /See:/ 'newSuiteDefinitionConfiguration' smart constructor.
data SuiteDefinitionConfiguration = SuiteDefinitionConfiguration'
  { -- | Gets the devices configured.
    devices :: Prelude.Maybe [DeviceUnderTest],
    -- | Gets the tests intended for qualification in a suite.
    intendedForQualification :: Prelude.Maybe Prelude.Bool,
    -- | Verifies if the test suite is a long duration test.
    isLongDurationTest :: Prelude.Maybe Prelude.Bool,
    -- | Sets the MQTT protocol that is configured in the suite definition.
    protocol :: Prelude.Maybe Protocol,
    -- | Gets the suite definition name. This is a required parameter.
    suiteDefinitionName :: Prelude.Text,
    -- | Gets the test suite root group. This is a required parameter. For
    -- updating or creating the latest qualification suite, if
    -- @intendedForQualification@ is set to true, @rootGroup@ can be an empty
    -- string. If @intendedForQualification@ is false, @rootGroup@ cannot be an
    -- empty string. If @rootGroup@ is empty, and @intendedForQualification@ is
    -- set to true, all the qualification tests are included, and the
    -- configuration is default.
    --
    -- For a qualification suite, the minimum length is 0, and the maximum is
    -- 2048. For a non-qualification suite, the minimum length is 1, and the
    -- maximum is 2048.
    rootGroup :: Prelude.Text,
    -- | Gets the device permission ARN. This is a required parameter.
    devicePermissionRoleArn :: Prelude.Text
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
-- 'devices', 'suiteDefinitionConfiguration_devices' - Gets the devices configured.
--
-- 'intendedForQualification', 'suiteDefinitionConfiguration_intendedForQualification' - Gets the tests intended for qualification in a suite.
--
-- 'isLongDurationTest', 'suiteDefinitionConfiguration_isLongDurationTest' - Verifies if the test suite is a long duration test.
--
-- 'protocol', 'suiteDefinitionConfiguration_protocol' - Sets the MQTT protocol that is configured in the suite definition.
--
-- 'suiteDefinitionName', 'suiteDefinitionConfiguration_suiteDefinitionName' - Gets the suite definition name. This is a required parameter.
--
-- 'rootGroup', 'suiteDefinitionConfiguration_rootGroup' - Gets the test suite root group. This is a required parameter. For
-- updating or creating the latest qualification suite, if
-- @intendedForQualification@ is set to true, @rootGroup@ can be an empty
-- string. If @intendedForQualification@ is false, @rootGroup@ cannot be an
-- empty string. If @rootGroup@ is empty, and @intendedForQualification@ is
-- set to true, all the qualification tests are included, and the
-- configuration is default.
--
-- For a qualification suite, the minimum length is 0, and the maximum is
-- 2048. For a non-qualification suite, the minimum length is 1, and the
-- maximum is 2048.
--
-- 'devicePermissionRoleArn', 'suiteDefinitionConfiguration_devicePermissionRoleArn' - Gets the device permission ARN. This is a required parameter.
newSuiteDefinitionConfiguration ::
  -- | 'suiteDefinitionName'
  Prelude.Text ->
  -- | 'rootGroup'
  Prelude.Text ->
  -- | 'devicePermissionRoleArn'
  Prelude.Text ->
  SuiteDefinitionConfiguration
newSuiteDefinitionConfiguration
  pSuiteDefinitionName_
  pRootGroup_
  pDevicePermissionRoleArn_ =
    SuiteDefinitionConfiguration'
      { devices =
          Prelude.Nothing,
        intendedForQualification = Prelude.Nothing,
        isLongDurationTest = Prelude.Nothing,
        protocol = Prelude.Nothing,
        suiteDefinitionName = pSuiteDefinitionName_,
        rootGroup = pRootGroup_,
        devicePermissionRoleArn =
          pDevicePermissionRoleArn_
      }

-- | Gets the devices configured.
suiteDefinitionConfiguration_devices :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe [DeviceUnderTest])
suiteDefinitionConfiguration_devices = Lens.lens (\SuiteDefinitionConfiguration' {devices} -> devices) (\s@SuiteDefinitionConfiguration' {} a -> s {devices = a} :: SuiteDefinitionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Gets the tests intended for qualification in a suite.
suiteDefinitionConfiguration_intendedForQualification :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Bool)
suiteDefinitionConfiguration_intendedForQualification = Lens.lens (\SuiteDefinitionConfiguration' {intendedForQualification} -> intendedForQualification) (\s@SuiteDefinitionConfiguration' {} a -> s {intendedForQualification = a} :: SuiteDefinitionConfiguration)

-- | Verifies if the test suite is a long duration test.
suiteDefinitionConfiguration_isLongDurationTest :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Prelude.Bool)
suiteDefinitionConfiguration_isLongDurationTest = Lens.lens (\SuiteDefinitionConfiguration' {isLongDurationTest} -> isLongDurationTest) (\s@SuiteDefinitionConfiguration' {} a -> s {isLongDurationTest = a} :: SuiteDefinitionConfiguration)

-- | Sets the MQTT protocol that is configured in the suite definition.
suiteDefinitionConfiguration_protocol :: Lens.Lens' SuiteDefinitionConfiguration (Prelude.Maybe Protocol)
suiteDefinitionConfiguration_protocol = Lens.lens (\SuiteDefinitionConfiguration' {protocol} -> protocol) (\s@SuiteDefinitionConfiguration' {} a -> s {protocol = a} :: SuiteDefinitionConfiguration)

-- | Gets the suite definition name. This is a required parameter.
suiteDefinitionConfiguration_suiteDefinitionName :: Lens.Lens' SuiteDefinitionConfiguration Prelude.Text
suiteDefinitionConfiguration_suiteDefinitionName = Lens.lens (\SuiteDefinitionConfiguration' {suiteDefinitionName} -> suiteDefinitionName) (\s@SuiteDefinitionConfiguration' {} a -> s {suiteDefinitionName = a} :: SuiteDefinitionConfiguration)

-- | Gets the test suite root group. This is a required parameter. For
-- updating or creating the latest qualification suite, if
-- @intendedForQualification@ is set to true, @rootGroup@ can be an empty
-- string. If @intendedForQualification@ is false, @rootGroup@ cannot be an
-- empty string. If @rootGroup@ is empty, and @intendedForQualification@ is
-- set to true, all the qualification tests are included, and the
-- configuration is default.
--
-- For a qualification suite, the minimum length is 0, and the maximum is
-- 2048. For a non-qualification suite, the minimum length is 1, and the
-- maximum is 2048.
suiteDefinitionConfiguration_rootGroup :: Lens.Lens' SuiteDefinitionConfiguration Prelude.Text
suiteDefinitionConfiguration_rootGroup = Lens.lens (\SuiteDefinitionConfiguration' {rootGroup} -> rootGroup) (\s@SuiteDefinitionConfiguration' {} a -> s {rootGroup = a} :: SuiteDefinitionConfiguration)

-- | Gets the device permission ARN. This is a required parameter.
suiteDefinitionConfiguration_devicePermissionRoleArn :: Lens.Lens' SuiteDefinitionConfiguration Prelude.Text
suiteDefinitionConfiguration_devicePermissionRoleArn = Lens.lens (\SuiteDefinitionConfiguration' {devicePermissionRoleArn} -> devicePermissionRoleArn) (\s@SuiteDefinitionConfiguration' {} a -> s {devicePermissionRoleArn = a} :: SuiteDefinitionConfiguration)

instance Data.FromJSON SuiteDefinitionConfiguration where
  parseJSON =
    Data.withObject
      "SuiteDefinitionConfiguration"
      ( \x ->
          SuiteDefinitionConfiguration'
            Prelude.<$> (x Data..:? "devices" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "intendedForQualification")
            Prelude.<*> (x Data..:? "isLongDurationTest")
            Prelude.<*> (x Data..:? "protocol")
            Prelude.<*> (x Data..: "suiteDefinitionName")
            Prelude.<*> (x Data..: "rootGroup")
            Prelude.<*> (x Data..: "devicePermissionRoleArn")
      )

instance
  Prelude.Hashable
    SuiteDefinitionConfiguration
  where
  hashWithSalt _salt SuiteDefinitionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` devices
      `Prelude.hashWithSalt` intendedForQualification
      `Prelude.hashWithSalt` isLongDurationTest
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` suiteDefinitionName
      `Prelude.hashWithSalt` rootGroup
      `Prelude.hashWithSalt` devicePermissionRoleArn

instance Prelude.NFData SuiteDefinitionConfiguration where
  rnf SuiteDefinitionConfiguration' {..} =
    Prelude.rnf devices
      `Prelude.seq` Prelude.rnf intendedForQualification
      `Prelude.seq` Prelude.rnf isLongDurationTest
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf suiteDefinitionName
      `Prelude.seq` Prelude.rnf rootGroup
      `Prelude.seq` Prelude.rnf devicePermissionRoleArn

instance Data.ToJSON SuiteDefinitionConfiguration where
  toJSON SuiteDefinitionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("devices" Data..=) Prelude.<$> devices,
            ("intendedForQualification" Data..=)
              Prelude.<$> intendedForQualification,
            ("isLongDurationTest" Data..=)
              Prelude.<$> isLongDurationTest,
            ("protocol" Data..=) Prelude.<$> protocol,
            Prelude.Just
              ("suiteDefinitionName" Data..= suiteDefinitionName),
            Prelude.Just ("rootGroup" Data..= rootGroup),
            Prelude.Just
              ( "devicePermissionRoleArn"
                  Data..= devicePermissionRoleArn
              )
          ]
      )
