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
-- Module      : Amazonka.Panorama.DescribeDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a device.
module Amazonka.Panorama.DescribeDevice
  ( -- * Creating a Request
    DescribeDevice (..),
    newDescribeDevice,

    -- * Request Lenses
    describeDevice_deviceId,

    -- * Destructuring the Response
    DescribeDeviceResponse (..),
    newDescribeDeviceResponse,

    -- * Response Lenses
    describeDeviceResponse_tags,
    describeDeviceResponse_currentNetworkingStatus,
    describeDeviceResponse_name,
    describeDeviceResponse_type,
    describeDeviceResponse_createdTime,
    describeDeviceResponse_leaseExpirationTime,
    describeDeviceResponse_latestSoftware,
    describeDeviceResponse_provisioningStatus,
    describeDeviceResponse_deviceId,
    describeDeviceResponse_networkingConfiguration,
    describeDeviceResponse_arn,
    describeDeviceResponse_description,
    describeDeviceResponse_deviceConnectionStatus,
    describeDeviceResponse_serialNumber,
    describeDeviceResponse_currentSoftware,
    describeDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDevice' smart constructor.
data DescribeDevice = DescribeDevice'
  { -- | The device\'s ID.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'describeDevice_deviceId' - The device\'s ID.
newDescribeDevice ::
  -- | 'deviceId'
  Prelude.Text ->
  DescribeDevice
newDescribeDevice pDeviceId_ =
  DescribeDevice' {deviceId = pDeviceId_}

-- | The device\'s ID.
describeDevice_deviceId :: Lens.Lens' DescribeDevice Prelude.Text
describeDevice_deviceId = Lens.lens (\DescribeDevice' {deviceId} -> deviceId) (\s@DescribeDevice' {} a -> s {deviceId = a} :: DescribeDevice)

instance Core.AWSRequest DescribeDevice where
  type
    AWSResponse DescribeDevice =
      DescribeDeviceResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "CurrentNetworkingStatus")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Type")
            Prelude.<*> (x Core..?> "CreatedTime")
            Prelude.<*> (x Core..?> "LeaseExpirationTime")
            Prelude.<*> (x Core..?> "LatestSoftware")
            Prelude.<*> (x Core..?> "ProvisioningStatus")
            Prelude.<*> (x Core..?> "DeviceId")
            Prelude.<*> (x Core..?> "NetworkingConfiguration")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "DeviceConnectionStatus")
            Prelude.<*> (x Core..?> "SerialNumber")
            Prelude.<*> (x Core..?> "CurrentSoftware")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDevice where
  hashWithSalt _salt DescribeDevice' {..} =
    _salt `Prelude.hashWithSalt` deviceId

instance Prelude.NFData DescribeDevice where
  rnf DescribeDevice' {..} = Prelude.rnf deviceId

instance Core.ToHeaders DescribeDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeDevice where
  toPath DescribeDevice' {..} =
    Prelude.mconcat ["/devices/", Core.toBS deviceId]

instance Core.ToQuery DescribeDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeviceResponse' smart constructor.
data DescribeDeviceResponse = DescribeDeviceResponse'
  { -- | The device\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The device\'s networking status.
    currentNetworkingStatus :: Prelude.Maybe NetworkStatus,
    -- | The device\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The device\'s type.
    type' :: Prelude.Maybe DeviceType,
    -- | When the device was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s lease expiration time.
    leaseExpirationTime :: Prelude.Maybe Core.POSIX,
    -- | The latest software version available for the device.
    latestSoftware :: Prelude.Maybe Prelude.Text,
    -- | The device\'s provisioning status.
    provisioningStatus :: Prelude.Maybe DeviceStatus,
    -- | The device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The device\'s networking configuration.
    networkingConfiguration :: Prelude.Maybe NetworkPayload,
    -- | The device\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The device\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The device\'s connection status.
    deviceConnectionStatus :: Prelude.Maybe DeviceConnectionStatus,
    -- | The device\'s serial number.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The device\'s current software version.
    currentSoftware :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeDeviceResponse_tags' - The device\'s tags.
--
-- 'currentNetworkingStatus', 'describeDeviceResponse_currentNetworkingStatus' - The device\'s networking status.
--
-- 'name', 'describeDeviceResponse_name' - The device\'s name.
--
-- 'type'', 'describeDeviceResponse_type' - The device\'s type.
--
-- 'createdTime', 'describeDeviceResponse_createdTime' - When the device was created.
--
-- 'leaseExpirationTime', 'describeDeviceResponse_leaseExpirationTime' - The device\'s lease expiration time.
--
-- 'latestSoftware', 'describeDeviceResponse_latestSoftware' - The latest software version available for the device.
--
-- 'provisioningStatus', 'describeDeviceResponse_provisioningStatus' - The device\'s provisioning status.
--
-- 'deviceId', 'describeDeviceResponse_deviceId' - The device\'s ID.
--
-- 'networkingConfiguration', 'describeDeviceResponse_networkingConfiguration' - The device\'s networking configuration.
--
-- 'arn', 'describeDeviceResponse_arn' - The device\'s ARN.
--
-- 'description', 'describeDeviceResponse_description' - The device\'s description.
--
-- 'deviceConnectionStatus', 'describeDeviceResponse_deviceConnectionStatus' - The device\'s connection status.
--
-- 'serialNumber', 'describeDeviceResponse_serialNumber' - The device\'s serial number.
--
-- 'currentSoftware', 'describeDeviceResponse_currentSoftware' - The device\'s current software version.
--
-- 'httpStatus', 'describeDeviceResponse_httpStatus' - The response's http status code.
newDescribeDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDeviceResponse
newDescribeDeviceResponse pHttpStatus_ =
  DescribeDeviceResponse'
    { tags = Prelude.Nothing,
      currentNetworkingStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      leaseExpirationTime = Prelude.Nothing,
      latestSoftware = Prelude.Nothing,
      provisioningStatus = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      networkingConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceConnectionStatus = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      currentSoftware = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The device\'s tags.
describeDeviceResponse_tags :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeDeviceResponse_tags = Lens.lens (\DescribeDeviceResponse' {tags} -> tags) (\s@DescribeDeviceResponse' {} a -> s {tags = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The device\'s networking status.
describeDeviceResponse_currentNetworkingStatus :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe NetworkStatus)
describeDeviceResponse_currentNetworkingStatus = Lens.lens (\DescribeDeviceResponse' {currentNetworkingStatus} -> currentNetworkingStatus) (\s@DescribeDeviceResponse' {} a -> s {currentNetworkingStatus = a} :: DescribeDeviceResponse)

-- | The device\'s name.
describeDeviceResponse_name :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_name = Lens.lens (\DescribeDeviceResponse' {name} -> name) (\s@DescribeDeviceResponse' {} a -> s {name = a} :: DescribeDeviceResponse)

-- | The device\'s type.
describeDeviceResponse_type :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe DeviceType)
describeDeviceResponse_type = Lens.lens (\DescribeDeviceResponse' {type'} -> type') (\s@DescribeDeviceResponse' {} a -> s {type' = a} :: DescribeDeviceResponse)

-- | When the device was created.
describeDeviceResponse_createdTime :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_createdTime = Lens.lens (\DescribeDeviceResponse' {createdTime} -> createdTime) (\s@DescribeDeviceResponse' {} a -> s {createdTime = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Core._Time

-- | The device\'s lease expiration time.
describeDeviceResponse_leaseExpirationTime :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_leaseExpirationTime = Lens.lens (\DescribeDeviceResponse' {leaseExpirationTime} -> leaseExpirationTime) (\s@DescribeDeviceResponse' {} a -> s {leaseExpirationTime = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Core._Time

-- | The latest software version available for the device.
describeDeviceResponse_latestSoftware :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_latestSoftware = Lens.lens (\DescribeDeviceResponse' {latestSoftware} -> latestSoftware) (\s@DescribeDeviceResponse' {} a -> s {latestSoftware = a} :: DescribeDeviceResponse)

-- | The device\'s provisioning status.
describeDeviceResponse_provisioningStatus :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe DeviceStatus)
describeDeviceResponse_provisioningStatus = Lens.lens (\DescribeDeviceResponse' {provisioningStatus} -> provisioningStatus) (\s@DescribeDeviceResponse' {} a -> s {provisioningStatus = a} :: DescribeDeviceResponse)

-- | The device\'s ID.
describeDeviceResponse_deviceId :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_deviceId = Lens.lens (\DescribeDeviceResponse' {deviceId} -> deviceId) (\s@DescribeDeviceResponse' {} a -> s {deviceId = a} :: DescribeDeviceResponse)

-- | The device\'s networking configuration.
describeDeviceResponse_networkingConfiguration :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe NetworkPayload)
describeDeviceResponse_networkingConfiguration = Lens.lens (\DescribeDeviceResponse' {networkingConfiguration} -> networkingConfiguration) (\s@DescribeDeviceResponse' {} a -> s {networkingConfiguration = a} :: DescribeDeviceResponse)

-- | The device\'s ARN.
describeDeviceResponse_arn :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_arn = Lens.lens (\DescribeDeviceResponse' {arn} -> arn) (\s@DescribeDeviceResponse' {} a -> s {arn = a} :: DescribeDeviceResponse)

-- | The device\'s description.
describeDeviceResponse_description :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_description = Lens.lens (\DescribeDeviceResponse' {description} -> description) (\s@DescribeDeviceResponse' {} a -> s {description = a} :: DescribeDeviceResponse)

-- | The device\'s connection status.
describeDeviceResponse_deviceConnectionStatus :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe DeviceConnectionStatus)
describeDeviceResponse_deviceConnectionStatus = Lens.lens (\DescribeDeviceResponse' {deviceConnectionStatus} -> deviceConnectionStatus) (\s@DescribeDeviceResponse' {} a -> s {deviceConnectionStatus = a} :: DescribeDeviceResponse)

-- | The device\'s serial number.
describeDeviceResponse_serialNumber :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_serialNumber = Lens.lens (\DescribeDeviceResponse' {serialNumber} -> serialNumber) (\s@DescribeDeviceResponse' {} a -> s {serialNumber = a} :: DescribeDeviceResponse)

-- | The device\'s current software version.
describeDeviceResponse_currentSoftware :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_currentSoftware = Lens.lens (\DescribeDeviceResponse' {currentSoftware} -> currentSoftware) (\s@DescribeDeviceResponse' {} a -> s {currentSoftware = a} :: DescribeDeviceResponse)

-- | The response's http status code.
describeDeviceResponse_httpStatus :: Lens.Lens' DescribeDeviceResponse Prelude.Int
describeDeviceResponse_httpStatus = Lens.lens (\DescribeDeviceResponse' {httpStatus} -> httpStatus) (\s@DescribeDeviceResponse' {} a -> s {httpStatus = a} :: DescribeDeviceResponse)

instance Prelude.NFData DescribeDeviceResponse where
  rnf DescribeDeviceResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf currentNetworkingStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf leaseExpirationTime
      `Prelude.seq` Prelude.rnf latestSoftware
      `Prelude.seq` Prelude.rnf provisioningStatus
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf networkingConfiguration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceConnectionStatus
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf currentSoftware
      `Prelude.seq` Prelude.rnf httpStatus
