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
-- Module      : Amazonka.Braket.GetDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the devices available in Amazon Braket.
--
-- For backwards compatibility with older versions of BraketSchemas,
-- OpenQASM information is omitted from GetDevice API calls. To get this
-- information the user-agent needs to present a recent version of the
-- BraketSchemas (1.8.0 or later). The Braket SDK automatically reports
-- this for you. If you do not see OpenQASM results in the GetDevice
-- response when using a Braket SDK, you may need to set AWS_EXECUTION_ENV
-- environment variable to configure user-agent. See the code examples
-- provided below for how to do this for the AWS CLI, Boto3, and the Go,
-- Java, and JavaScript\/TypeScript SDKs.
module Amazonka.Braket.GetDevice
  ( -- * Creating a Request
    GetDevice (..),
    newGetDevice,

    -- * Request Lenses
    getDevice_deviceArn,

    -- * Destructuring the Response
    GetDeviceResponse (..),
    newGetDeviceResponse,

    -- * Response Lenses
    getDeviceResponse_httpStatus,
    getDeviceResponse_deviceArn,
    getDeviceResponse_deviceCapabilities,
    getDeviceResponse_deviceName,
    getDeviceResponse_deviceStatus,
    getDeviceResponse_deviceType,
    getDeviceResponse_providerName,
  )
where

import Amazonka.Braket.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDevice' smart constructor.
data GetDevice = GetDevice'
  { -- | The ARN of the device to retrieve.
    deviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'getDevice_deviceArn' - The ARN of the device to retrieve.
newGetDevice ::
  -- | 'deviceArn'
  Prelude.Text ->
  GetDevice
newGetDevice pDeviceArn_ =
  GetDevice' {deviceArn = pDeviceArn_}

-- | The ARN of the device to retrieve.
getDevice_deviceArn :: Lens.Lens' GetDevice Prelude.Text
getDevice_deviceArn = Lens.lens (\GetDevice' {deviceArn} -> deviceArn) (\s@GetDevice' {} a -> s {deviceArn = a} :: GetDevice)

instance Core.AWSRequest GetDevice where
  type AWSResponse GetDevice = GetDeviceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "deviceArn")
            Prelude.<*> (x Data..:> "deviceCapabilities")
            Prelude.<*> (x Data..:> "deviceName")
            Prelude.<*> (x Data..:> "deviceStatus")
            Prelude.<*> (x Data..:> "deviceType")
            Prelude.<*> (x Data..:> "providerName")
      )

instance Prelude.Hashable GetDevice where
  hashWithSalt _salt GetDevice' {..} =
    _salt `Prelude.hashWithSalt` deviceArn

instance Prelude.NFData GetDevice where
  rnf GetDevice' {..} = Prelude.rnf deviceArn

instance Data.ToHeaders GetDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDevice where
  toPath GetDevice' {..} =
    Prelude.mconcat ["/device/", Data.toBS deviceArn]

instance Data.ToQuery GetDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the device.
    deviceArn :: Prelude.Text,
    -- | Details about the capabilities of the device.
    deviceCapabilities :: Prelude.Text,
    -- | The name of the device.
    deviceName :: Prelude.Text,
    -- | The status of the device.
    deviceStatus :: DeviceStatus,
    -- | The type of the device.
    deviceType :: DeviceType,
    -- | The name of the partner company for the device.
    providerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDeviceResponse_httpStatus' - The response's http status code.
--
-- 'deviceArn', 'getDeviceResponse_deviceArn' - The ARN of the device.
--
-- 'deviceCapabilities', 'getDeviceResponse_deviceCapabilities' - Details about the capabilities of the device.
--
-- 'deviceName', 'getDeviceResponse_deviceName' - The name of the device.
--
-- 'deviceStatus', 'getDeviceResponse_deviceStatus' - The status of the device.
--
-- 'deviceType', 'getDeviceResponse_deviceType' - The type of the device.
--
-- 'providerName', 'getDeviceResponse_providerName' - The name of the partner company for the device.
newGetDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deviceArn'
  Prelude.Text ->
  -- | 'deviceCapabilities'
  Prelude.Text ->
  -- | 'deviceName'
  Prelude.Text ->
  -- | 'deviceStatus'
  DeviceStatus ->
  -- | 'deviceType'
  DeviceType ->
  -- | 'providerName'
  Prelude.Text ->
  GetDeviceResponse
newGetDeviceResponse
  pHttpStatus_
  pDeviceArn_
  pDeviceCapabilities_
  pDeviceName_
  pDeviceStatus_
  pDeviceType_
  pProviderName_ =
    GetDeviceResponse'
      { httpStatus = pHttpStatus_,
        deviceArn = pDeviceArn_,
        deviceCapabilities = pDeviceCapabilities_,
        deviceName = pDeviceName_,
        deviceStatus = pDeviceStatus_,
        deviceType = pDeviceType_,
        providerName = pProviderName_
      }

-- | The response's http status code.
getDeviceResponse_httpStatus :: Lens.Lens' GetDeviceResponse Prelude.Int
getDeviceResponse_httpStatus = Lens.lens (\GetDeviceResponse' {httpStatus} -> httpStatus) (\s@GetDeviceResponse' {} a -> s {httpStatus = a} :: GetDeviceResponse)

-- | The ARN of the device.
getDeviceResponse_deviceArn :: Lens.Lens' GetDeviceResponse Prelude.Text
getDeviceResponse_deviceArn = Lens.lens (\GetDeviceResponse' {deviceArn} -> deviceArn) (\s@GetDeviceResponse' {} a -> s {deviceArn = a} :: GetDeviceResponse)

-- | Details about the capabilities of the device.
getDeviceResponse_deviceCapabilities :: Lens.Lens' GetDeviceResponse Prelude.Text
getDeviceResponse_deviceCapabilities = Lens.lens (\GetDeviceResponse' {deviceCapabilities} -> deviceCapabilities) (\s@GetDeviceResponse' {} a -> s {deviceCapabilities = a} :: GetDeviceResponse)

-- | The name of the device.
getDeviceResponse_deviceName :: Lens.Lens' GetDeviceResponse Prelude.Text
getDeviceResponse_deviceName = Lens.lens (\GetDeviceResponse' {deviceName} -> deviceName) (\s@GetDeviceResponse' {} a -> s {deviceName = a} :: GetDeviceResponse)

-- | The status of the device.
getDeviceResponse_deviceStatus :: Lens.Lens' GetDeviceResponse DeviceStatus
getDeviceResponse_deviceStatus = Lens.lens (\GetDeviceResponse' {deviceStatus} -> deviceStatus) (\s@GetDeviceResponse' {} a -> s {deviceStatus = a} :: GetDeviceResponse)

-- | The type of the device.
getDeviceResponse_deviceType :: Lens.Lens' GetDeviceResponse DeviceType
getDeviceResponse_deviceType = Lens.lens (\GetDeviceResponse' {deviceType} -> deviceType) (\s@GetDeviceResponse' {} a -> s {deviceType = a} :: GetDeviceResponse)

-- | The name of the partner company for the device.
getDeviceResponse_providerName :: Lens.Lens' GetDeviceResponse Prelude.Text
getDeviceResponse_providerName = Lens.lens (\GetDeviceResponse' {providerName} -> providerName) (\s@GetDeviceResponse' {} a -> s {providerName = a} :: GetDeviceResponse)

instance Prelude.NFData GetDeviceResponse where
  rnf GetDeviceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf deviceArn `Prelude.seq`
        Prelude.rnf deviceCapabilities `Prelude.seq`
          Prelude.rnf deviceName `Prelude.seq`
            Prelude.rnf deviceStatus `Prelude.seq`
              Prelude.rnf deviceType `Prelude.seq`
                Prelude.rnf providerName
