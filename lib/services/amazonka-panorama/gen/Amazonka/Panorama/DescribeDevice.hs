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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeDeviceResponse_alternateSoftwares,
    describeDeviceResponse_arn,
    describeDeviceResponse_brand,
    describeDeviceResponse_createdTime,
    describeDeviceResponse_currentNetworkingStatus,
    describeDeviceResponse_currentSoftware,
    describeDeviceResponse_description,
    describeDeviceResponse_deviceAggregatedStatus,
    describeDeviceResponse_deviceConnectionStatus,
    describeDeviceResponse_deviceId,
    describeDeviceResponse_latestAlternateSoftware,
    describeDeviceResponse_latestDeviceJob,
    describeDeviceResponse_latestSoftware,
    describeDeviceResponse_leaseExpirationTime,
    describeDeviceResponse_name,
    describeDeviceResponse_networkingConfiguration,
    describeDeviceResponse_provisioningStatus,
    describeDeviceResponse_serialNumber,
    describeDeviceResponse_tags,
    describeDeviceResponse_type,
    describeDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceResponse'
            Prelude.<$> ( x Data..?> "AlternateSoftwares"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Brand")
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "CurrentNetworkingStatus")
            Prelude.<*> (x Data..?> "CurrentSoftware")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "DeviceAggregatedStatus")
            Prelude.<*> (x Data..?> "DeviceConnectionStatus")
            Prelude.<*> (x Data..?> "DeviceId")
            Prelude.<*> (x Data..?> "LatestAlternateSoftware")
            Prelude.<*> (x Data..?> "LatestDeviceJob")
            Prelude.<*> (x Data..?> "LatestSoftware")
            Prelude.<*> (x Data..?> "LeaseExpirationTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "NetworkingConfiguration")
            Prelude.<*> (x Data..?> "ProvisioningStatus")
            Prelude.<*> (x Data..?> "SerialNumber")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDevice where
  hashWithSalt _salt DescribeDevice' {..} =
    _salt `Prelude.hashWithSalt` deviceId

instance Prelude.NFData DescribeDevice where
  rnf DescribeDevice' {..} = Prelude.rnf deviceId

instance Data.ToHeaders DescribeDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDevice where
  toPath DescribeDevice' {..} =
    Prelude.mconcat ["/devices/", Data.toBS deviceId]

instance Data.ToQuery DescribeDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeviceResponse' smart constructor.
data DescribeDeviceResponse = DescribeDeviceResponse'
  { -- | Beta software releases available for the device.
    alternateSoftwares :: Prelude.Maybe [AlternateSoftwareMetadata],
    -- | The device\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The device\'s maker.
    brand :: Prelude.Maybe DeviceBrand,
    -- | When the device was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The device\'s networking status.
    currentNetworkingStatus :: Prelude.Maybe NetworkStatus,
    -- | The device\'s current software version.
    currentSoftware :: Prelude.Maybe Prelude.Text,
    -- | The device\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A device\'s aggregated status. Including the device\'s connection
    -- status, provisioning status, and lease status.
    deviceAggregatedStatus :: Prelude.Maybe DeviceAggregatedStatus,
    -- | The device\'s connection status.
    deviceConnectionStatus :: Prelude.Maybe DeviceConnectionStatus,
    -- | The device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The most recent beta software release.
    latestAlternateSoftware :: Prelude.Maybe Prelude.Text,
    -- | A device\'s latest job. Includes the target image version, and the job
    -- status.
    latestDeviceJob :: Prelude.Maybe LatestDeviceJob,
    -- | The latest software version available for the device.
    latestSoftware :: Prelude.Maybe Prelude.Text,
    -- | The device\'s lease expiration time.
    leaseExpirationTime :: Prelude.Maybe Data.POSIX,
    -- | The device\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The device\'s networking configuration.
    networkingConfiguration :: Prelude.Maybe NetworkPayload,
    -- | The device\'s provisioning status.
    provisioningStatus :: Prelude.Maybe DeviceStatus,
    -- | The device\'s serial number.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The device\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The device\'s type.
    type' :: Prelude.Maybe DeviceType,
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
-- 'alternateSoftwares', 'describeDeviceResponse_alternateSoftwares' - Beta software releases available for the device.
--
-- 'arn', 'describeDeviceResponse_arn' - The device\'s ARN.
--
-- 'brand', 'describeDeviceResponse_brand' - The device\'s maker.
--
-- 'createdTime', 'describeDeviceResponse_createdTime' - When the device was created.
--
-- 'currentNetworkingStatus', 'describeDeviceResponse_currentNetworkingStatus' - The device\'s networking status.
--
-- 'currentSoftware', 'describeDeviceResponse_currentSoftware' - The device\'s current software version.
--
-- 'description', 'describeDeviceResponse_description' - The device\'s description.
--
-- 'deviceAggregatedStatus', 'describeDeviceResponse_deviceAggregatedStatus' - A device\'s aggregated status. Including the device\'s connection
-- status, provisioning status, and lease status.
--
-- 'deviceConnectionStatus', 'describeDeviceResponse_deviceConnectionStatus' - The device\'s connection status.
--
-- 'deviceId', 'describeDeviceResponse_deviceId' - The device\'s ID.
--
-- 'latestAlternateSoftware', 'describeDeviceResponse_latestAlternateSoftware' - The most recent beta software release.
--
-- 'latestDeviceJob', 'describeDeviceResponse_latestDeviceJob' - A device\'s latest job. Includes the target image version, and the job
-- status.
--
-- 'latestSoftware', 'describeDeviceResponse_latestSoftware' - The latest software version available for the device.
--
-- 'leaseExpirationTime', 'describeDeviceResponse_leaseExpirationTime' - The device\'s lease expiration time.
--
-- 'name', 'describeDeviceResponse_name' - The device\'s name.
--
-- 'networkingConfiguration', 'describeDeviceResponse_networkingConfiguration' - The device\'s networking configuration.
--
-- 'provisioningStatus', 'describeDeviceResponse_provisioningStatus' - The device\'s provisioning status.
--
-- 'serialNumber', 'describeDeviceResponse_serialNumber' - The device\'s serial number.
--
-- 'tags', 'describeDeviceResponse_tags' - The device\'s tags.
--
-- 'type'', 'describeDeviceResponse_type' - The device\'s type.
--
-- 'httpStatus', 'describeDeviceResponse_httpStatus' - The response's http status code.
newDescribeDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDeviceResponse
newDescribeDeviceResponse pHttpStatus_ =
  DescribeDeviceResponse'
    { alternateSoftwares =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      brand = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      currentNetworkingStatus = Prelude.Nothing,
      currentSoftware = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceAggregatedStatus = Prelude.Nothing,
      deviceConnectionStatus = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      latestAlternateSoftware = Prelude.Nothing,
      latestDeviceJob = Prelude.Nothing,
      latestSoftware = Prelude.Nothing,
      leaseExpirationTime = Prelude.Nothing,
      name = Prelude.Nothing,
      networkingConfiguration = Prelude.Nothing,
      provisioningStatus = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Beta software releases available for the device.
describeDeviceResponse_alternateSoftwares :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe [AlternateSoftwareMetadata])
describeDeviceResponse_alternateSoftwares = Lens.lens (\DescribeDeviceResponse' {alternateSoftwares} -> alternateSoftwares) (\s@DescribeDeviceResponse' {} a -> s {alternateSoftwares = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The device\'s ARN.
describeDeviceResponse_arn :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_arn = Lens.lens (\DescribeDeviceResponse' {arn} -> arn) (\s@DescribeDeviceResponse' {} a -> s {arn = a} :: DescribeDeviceResponse)

-- | The device\'s maker.
describeDeviceResponse_brand :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe DeviceBrand)
describeDeviceResponse_brand = Lens.lens (\DescribeDeviceResponse' {brand} -> brand) (\s@DescribeDeviceResponse' {} a -> s {brand = a} :: DescribeDeviceResponse)

-- | When the device was created.
describeDeviceResponse_createdTime :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_createdTime = Lens.lens (\DescribeDeviceResponse' {createdTime} -> createdTime) (\s@DescribeDeviceResponse' {} a -> s {createdTime = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Data._Time

-- | The device\'s networking status.
describeDeviceResponse_currentNetworkingStatus :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe NetworkStatus)
describeDeviceResponse_currentNetworkingStatus = Lens.lens (\DescribeDeviceResponse' {currentNetworkingStatus} -> currentNetworkingStatus) (\s@DescribeDeviceResponse' {} a -> s {currentNetworkingStatus = a} :: DescribeDeviceResponse)

-- | The device\'s current software version.
describeDeviceResponse_currentSoftware :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_currentSoftware = Lens.lens (\DescribeDeviceResponse' {currentSoftware} -> currentSoftware) (\s@DescribeDeviceResponse' {} a -> s {currentSoftware = a} :: DescribeDeviceResponse)

-- | The device\'s description.
describeDeviceResponse_description :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_description = Lens.lens (\DescribeDeviceResponse' {description} -> description) (\s@DescribeDeviceResponse' {} a -> s {description = a} :: DescribeDeviceResponse)

-- | A device\'s aggregated status. Including the device\'s connection
-- status, provisioning status, and lease status.
describeDeviceResponse_deviceAggregatedStatus :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe DeviceAggregatedStatus)
describeDeviceResponse_deviceAggregatedStatus = Lens.lens (\DescribeDeviceResponse' {deviceAggregatedStatus} -> deviceAggregatedStatus) (\s@DescribeDeviceResponse' {} a -> s {deviceAggregatedStatus = a} :: DescribeDeviceResponse)

-- | The device\'s connection status.
describeDeviceResponse_deviceConnectionStatus :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe DeviceConnectionStatus)
describeDeviceResponse_deviceConnectionStatus = Lens.lens (\DescribeDeviceResponse' {deviceConnectionStatus} -> deviceConnectionStatus) (\s@DescribeDeviceResponse' {} a -> s {deviceConnectionStatus = a} :: DescribeDeviceResponse)

-- | The device\'s ID.
describeDeviceResponse_deviceId :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_deviceId = Lens.lens (\DescribeDeviceResponse' {deviceId} -> deviceId) (\s@DescribeDeviceResponse' {} a -> s {deviceId = a} :: DescribeDeviceResponse)

-- | The most recent beta software release.
describeDeviceResponse_latestAlternateSoftware :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_latestAlternateSoftware = Lens.lens (\DescribeDeviceResponse' {latestAlternateSoftware} -> latestAlternateSoftware) (\s@DescribeDeviceResponse' {} a -> s {latestAlternateSoftware = a} :: DescribeDeviceResponse)

-- | A device\'s latest job. Includes the target image version, and the job
-- status.
describeDeviceResponse_latestDeviceJob :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe LatestDeviceJob)
describeDeviceResponse_latestDeviceJob = Lens.lens (\DescribeDeviceResponse' {latestDeviceJob} -> latestDeviceJob) (\s@DescribeDeviceResponse' {} a -> s {latestDeviceJob = a} :: DescribeDeviceResponse)

-- | The latest software version available for the device.
describeDeviceResponse_latestSoftware :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_latestSoftware = Lens.lens (\DescribeDeviceResponse' {latestSoftware} -> latestSoftware) (\s@DescribeDeviceResponse' {} a -> s {latestSoftware = a} :: DescribeDeviceResponse)

-- | The device\'s lease expiration time.
describeDeviceResponse_leaseExpirationTime :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_leaseExpirationTime = Lens.lens (\DescribeDeviceResponse' {leaseExpirationTime} -> leaseExpirationTime) (\s@DescribeDeviceResponse' {} a -> s {leaseExpirationTime = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Data._Time

-- | The device\'s name.
describeDeviceResponse_name :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_name = Lens.lens (\DescribeDeviceResponse' {name} -> name) (\s@DescribeDeviceResponse' {} a -> s {name = a} :: DescribeDeviceResponse)

-- | The device\'s networking configuration.
describeDeviceResponse_networkingConfiguration :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe NetworkPayload)
describeDeviceResponse_networkingConfiguration = Lens.lens (\DescribeDeviceResponse' {networkingConfiguration} -> networkingConfiguration) (\s@DescribeDeviceResponse' {} a -> s {networkingConfiguration = a} :: DescribeDeviceResponse)

-- | The device\'s provisioning status.
describeDeviceResponse_provisioningStatus :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe DeviceStatus)
describeDeviceResponse_provisioningStatus = Lens.lens (\DescribeDeviceResponse' {provisioningStatus} -> provisioningStatus) (\s@DescribeDeviceResponse' {} a -> s {provisioningStatus = a} :: DescribeDeviceResponse)

-- | The device\'s serial number.
describeDeviceResponse_serialNumber :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_serialNumber = Lens.lens (\DescribeDeviceResponse' {serialNumber} -> serialNumber) (\s@DescribeDeviceResponse' {} a -> s {serialNumber = a} :: DescribeDeviceResponse)

-- | The device\'s tags.
describeDeviceResponse_tags :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeDeviceResponse_tags = Lens.lens (\DescribeDeviceResponse' {tags} -> tags) (\s@DescribeDeviceResponse' {} a -> s {tags = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The device\'s type.
describeDeviceResponse_type :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe DeviceType)
describeDeviceResponse_type = Lens.lens (\DescribeDeviceResponse' {type'} -> type') (\s@DescribeDeviceResponse' {} a -> s {type' = a} :: DescribeDeviceResponse)

-- | The response's http status code.
describeDeviceResponse_httpStatus :: Lens.Lens' DescribeDeviceResponse Prelude.Int
describeDeviceResponse_httpStatus = Lens.lens (\DescribeDeviceResponse' {httpStatus} -> httpStatus) (\s@DescribeDeviceResponse' {} a -> s {httpStatus = a} :: DescribeDeviceResponse)

instance Prelude.NFData DescribeDeviceResponse where
  rnf DescribeDeviceResponse' {..} =
    Prelude.rnf alternateSoftwares
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf brand
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf currentNetworkingStatus
      `Prelude.seq` Prelude.rnf currentSoftware
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceAggregatedStatus
      `Prelude.seq` Prelude.rnf deviceConnectionStatus
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf latestAlternateSoftware
      `Prelude.seq` Prelude.rnf latestDeviceJob
      `Prelude.seq` Prelude.rnf latestSoftware
      `Prelude.seq` Prelude.rnf leaseExpirationTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkingConfiguration
      `Prelude.seq` Prelude.rnf provisioningStatus
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
