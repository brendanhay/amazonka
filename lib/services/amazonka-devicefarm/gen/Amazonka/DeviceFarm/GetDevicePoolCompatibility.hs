{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.GetDevicePoolCompatibility
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about compatibility with a device pool.
module Amazonka.DeviceFarm.GetDevicePoolCompatibility
  ( -- * Creating a Request
    GetDevicePoolCompatibility (..),
    newGetDevicePoolCompatibility,

    -- * Request Lenses
    getDevicePoolCompatibility_appArn,
    getDevicePoolCompatibility_configuration,
    getDevicePoolCompatibility_test,
    getDevicePoolCompatibility_testType,
    getDevicePoolCompatibility_devicePoolArn,

    -- * Destructuring the Response
    GetDevicePoolCompatibilityResponse (..),
    newGetDevicePoolCompatibilityResponse,

    -- * Response Lenses
    getDevicePoolCompatibilityResponse_compatibleDevices,
    getDevicePoolCompatibilityResponse_incompatibleDevices,
    getDevicePoolCompatibilityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the get device pool compatibility operation.
--
-- /See:/ 'newGetDevicePoolCompatibility' smart constructor.
data GetDevicePoolCompatibility = GetDevicePoolCompatibility'
  { -- | The ARN of the app that is associated with the specified device pool.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | An object that contains information about the settings for a run.
    configuration :: Prelude.Maybe ScheduleRunConfiguration,
    -- | Information about the uploaded test to be run against the device pool.
    test :: Prelude.Maybe ScheduleRunTest,
    -- | The test type for the specified device pool.
    --
    -- Allowed values include the following:
    --
    -- -   BUILTIN_FUZZ.
    --
    -- -   BUILTIN_EXPLORER. For Android, an app explorer that traverses an
    --     Android app, interacting with it and capturing screenshots at the
    --     same time.
    --
    -- -   APPIUM_JAVA_JUNIT.
    --
    -- -   APPIUM_JAVA_TESTNG.
    --
    -- -   APPIUM_PYTHON.
    --
    -- -   APPIUM_NODE.
    --
    -- -   APPIUM_RUBY.
    --
    -- -   APPIUM_WEB_JAVA_JUNIT.
    --
    -- -   APPIUM_WEB_JAVA_TESTNG.
    --
    -- -   APPIUM_WEB_PYTHON.
    --
    -- -   APPIUM_WEB_NODE.
    --
    -- -   APPIUM_WEB_RUBY.
    --
    -- -   CALABASH.
    --
    -- -   INSTRUMENTATION.
    --
    -- -   UIAUTOMATION.
    --
    -- -   UIAUTOMATOR.
    --
    -- -   XCTEST.
    --
    -- -   XCTEST_UI.
    testType :: Prelude.Maybe TestType,
    -- | The device pool\'s ARN.
    devicePoolArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicePoolCompatibility' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'getDevicePoolCompatibility_appArn' - The ARN of the app that is associated with the specified device pool.
--
-- 'configuration', 'getDevicePoolCompatibility_configuration' - An object that contains information about the settings for a run.
--
-- 'test', 'getDevicePoolCompatibility_test' - Information about the uploaded test to be run against the device pool.
--
-- 'testType', 'getDevicePoolCompatibility_testType' - The test type for the specified device pool.
--
-- Allowed values include the following:
--
-- -   BUILTIN_FUZZ.
--
-- -   BUILTIN_EXPLORER. For Android, an app explorer that traverses an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT.
--
-- -   APPIUM_JAVA_TESTNG.
--
-- -   APPIUM_PYTHON.
--
-- -   APPIUM_NODE.
--
-- -   APPIUM_RUBY.
--
-- -   APPIUM_WEB_JAVA_JUNIT.
--
-- -   APPIUM_WEB_JAVA_TESTNG.
--
-- -   APPIUM_WEB_PYTHON.
--
-- -   APPIUM_WEB_NODE.
--
-- -   APPIUM_WEB_RUBY.
--
-- -   CALABASH.
--
-- -   INSTRUMENTATION.
--
-- -   UIAUTOMATION.
--
-- -   UIAUTOMATOR.
--
-- -   XCTEST.
--
-- -   XCTEST_UI.
--
-- 'devicePoolArn', 'getDevicePoolCompatibility_devicePoolArn' - The device pool\'s ARN.
newGetDevicePoolCompatibility ::
  -- | 'devicePoolArn'
  Prelude.Text ->
  GetDevicePoolCompatibility
newGetDevicePoolCompatibility pDevicePoolArn_ =
  GetDevicePoolCompatibility'
    { appArn =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      test = Prelude.Nothing,
      testType = Prelude.Nothing,
      devicePoolArn = pDevicePoolArn_
    }

-- | The ARN of the app that is associated with the specified device pool.
getDevicePoolCompatibility_appArn :: Lens.Lens' GetDevicePoolCompatibility (Prelude.Maybe Prelude.Text)
getDevicePoolCompatibility_appArn = Lens.lens (\GetDevicePoolCompatibility' {appArn} -> appArn) (\s@GetDevicePoolCompatibility' {} a -> s {appArn = a} :: GetDevicePoolCompatibility)

-- | An object that contains information about the settings for a run.
getDevicePoolCompatibility_configuration :: Lens.Lens' GetDevicePoolCompatibility (Prelude.Maybe ScheduleRunConfiguration)
getDevicePoolCompatibility_configuration = Lens.lens (\GetDevicePoolCompatibility' {configuration} -> configuration) (\s@GetDevicePoolCompatibility' {} a -> s {configuration = a} :: GetDevicePoolCompatibility)

-- | Information about the uploaded test to be run against the device pool.
getDevicePoolCompatibility_test :: Lens.Lens' GetDevicePoolCompatibility (Prelude.Maybe ScheduleRunTest)
getDevicePoolCompatibility_test = Lens.lens (\GetDevicePoolCompatibility' {test} -> test) (\s@GetDevicePoolCompatibility' {} a -> s {test = a} :: GetDevicePoolCompatibility)

-- | The test type for the specified device pool.
--
-- Allowed values include the following:
--
-- -   BUILTIN_FUZZ.
--
-- -   BUILTIN_EXPLORER. For Android, an app explorer that traverses an
--     Android app, interacting with it and capturing screenshots at the
--     same time.
--
-- -   APPIUM_JAVA_JUNIT.
--
-- -   APPIUM_JAVA_TESTNG.
--
-- -   APPIUM_PYTHON.
--
-- -   APPIUM_NODE.
--
-- -   APPIUM_RUBY.
--
-- -   APPIUM_WEB_JAVA_JUNIT.
--
-- -   APPIUM_WEB_JAVA_TESTNG.
--
-- -   APPIUM_WEB_PYTHON.
--
-- -   APPIUM_WEB_NODE.
--
-- -   APPIUM_WEB_RUBY.
--
-- -   CALABASH.
--
-- -   INSTRUMENTATION.
--
-- -   UIAUTOMATION.
--
-- -   UIAUTOMATOR.
--
-- -   XCTEST.
--
-- -   XCTEST_UI.
getDevicePoolCompatibility_testType :: Lens.Lens' GetDevicePoolCompatibility (Prelude.Maybe TestType)
getDevicePoolCompatibility_testType = Lens.lens (\GetDevicePoolCompatibility' {testType} -> testType) (\s@GetDevicePoolCompatibility' {} a -> s {testType = a} :: GetDevicePoolCompatibility)

-- | The device pool\'s ARN.
getDevicePoolCompatibility_devicePoolArn :: Lens.Lens' GetDevicePoolCompatibility Prelude.Text
getDevicePoolCompatibility_devicePoolArn = Lens.lens (\GetDevicePoolCompatibility' {devicePoolArn} -> devicePoolArn) (\s@GetDevicePoolCompatibility' {} a -> s {devicePoolArn = a} :: GetDevicePoolCompatibility)

instance Core.AWSRequest GetDevicePoolCompatibility where
  type
    AWSResponse GetDevicePoolCompatibility =
      GetDevicePoolCompatibilityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevicePoolCompatibilityResponse'
            Prelude.<$> ( x Data..?> "compatibleDevices"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "incompatibleDevices"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDevicePoolCompatibility where
  hashWithSalt _salt GetDevicePoolCompatibility' {..} =
    _salt `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` test
      `Prelude.hashWithSalt` testType
      `Prelude.hashWithSalt` devicePoolArn

instance Prelude.NFData GetDevicePoolCompatibility where
  rnf GetDevicePoolCompatibility' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf test
      `Prelude.seq` Prelude.rnf testType
      `Prelude.seq` Prelude.rnf devicePoolArn

instance Data.ToHeaders GetDevicePoolCompatibility where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.GetDevicePoolCompatibility" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDevicePoolCompatibility where
  toJSON GetDevicePoolCompatibility' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("appArn" Data..=) Prelude.<$> appArn,
            ("configuration" Data..=) Prelude.<$> configuration,
            ("test" Data..=) Prelude.<$> test,
            ("testType" Data..=) Prelude.<$> testType,
            Prelude.Just
              ("devicePoolArn" Data..= devicePoolArn)
          ]
      )

instance Data.ToPath GetDevicePoolCompatibility where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDevicePoolCompatibility where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of describe device pool compatibility request.
--
-- /See:/ 'newGetDevicePoolCompatibilityResponse' smart constructor.
data GetDevicePoolCompatibilityResponse = GetDevicePoolCompatibilityResponse'
  { -- | Information about compatible devices.
    compatibleDevices :: Prelude.Maybe [DevicePoolCompatibilityResult],
    -- | Information about incompatible devices.
    incompatibleDevices :: Prelude.Maybe [DevicePoolCompatibilityResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicePoolCompatibilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleDevices', 'getDevicePoolCompatibilityResponse_compatibleDevices' - Information about compatible devices.
--
-- 'incompatibleDevices', 'getDevicePoolCompatibilityResponse_incompatibleDevices' - Information about incompatible devices.
--
-- 'httpStatus', 'getDevicePoolCompatibilityResponse_httpStatus' - The response's http status code.
newGetDevicePoolCompatibilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDevicePoolCompatibilityResponse
newGetDevicePoolCompatibilityResponse pHttpStatus_ =
  GetDevicePoolCompatibilityResponse'
    { compatibleDevices =
        Prelude.Nothing,
      incompatibleDevices = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about compatible devices.
getDevicePoolCompatibilityResponse_compatibleDevices :: Lens.Lens' GetDevicePoolCompatibilityResponse (Prelude.Maybe [DevicePoolCompatibilityResult])
getDevicePoolCompatibilityResponse_compatibleDevices = Lens.lens (\GetDevicePoolCompatibilityResponse' {compatibleDevices} -> compatibleDevices) (\s@GetDevicePoolCompatibilityResponse' {} a -> s {compatibleDevices = a} :: GetDevicePoolCompatibilityResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about incompatible devices.
getDevicePoolCompatibilityResponse_incompatibleDevices :: Lens.Lens' GetDevicePoolCompatibilityResponse (Prelude.Maybe [DevicePoolCompatibilityResult])
getDevicePoolCompatibilityResponse_incompatibleDevices = Lens.lens (\GetDevicePoolCompatibilityResponse' {incompatibleDevices} -> incompatibleDevices) (\s@GetDevicePoolCompatibilityResponse' {} a -> s {incompatibleDevices = a} :: GetDevicePoolCompatibilityResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDevicePoolCompatibilityResponse_httpStatus :: Lens.Lens' GetDevicePoolCompatibilityResponse Prelude.Int
getDevicePoolCompatibilityResponse_httpStatus = Lens.lens (\GetDevicePoolCompatibilityResponse' {httpStatus} -> httpStatus) (\s@GetDevicePoolCompatibilityResponse' {} a -> s {httpStatus = a} :: GetDevicePoolCompatibilityResponse)

instance
  Prelude.NFData
    GetDevicePoolCompatibilityResponse
  where
  rnf GetDevicePoolCompatibilityResponse' {..} =
    Prelude.rnf compatibleDevices
      `Prelude.seq` Prelude.rnf incompatibleDevices
      `Prelude.seq` Prelude.rnf httpStatus
