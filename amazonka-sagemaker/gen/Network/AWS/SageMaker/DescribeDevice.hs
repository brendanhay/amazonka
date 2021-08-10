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
-- Module      : Network.AWS.SageMaker.DescribeDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the device.
module Network.AWS.SageMaker.DescribeDevice
  ( -- * Creating a Request
    DescribeDevice (..),
    newDescribeDevice,

    -- * Request Lenses
    describeDevice_nextToken,
    describeDevice_deviceName,
    describeDevice_deviceFleetName,

    -- * Destructuring the Response
    DescribeDeviceResponse (..),
    newDescribeDeviceResponse,

    -- * Response Lenses
    describeDeviceResponse_nextToken,
    describeDeviceResponse_latestHeartbeat,
    describeDeviceResponse_maxModels,
    describeDeviceResponse_deviceArn,
    describeDeviceResponse_models,
    describeDeviceResponse_iotThingName,
    describeDeviceResponse_description,
    describeDeviceResponse_httpStatus,
    describeDeviceResponse_deviceName,
    describeDeviceResponse_deviceFleetName,
    describeDeviceResponse_registrationTime,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeDevice' smart constructor.
data DescribeDevice = DescribeDevice'
  { -- | Next token of device description.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the device.
    deviceName :: Prelude.Text,
    -- | The name of the fleet the devices belong to.
    deviceFleetName :: Prelude.Text
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
-- 'nextToken', 'describeDevice_nextToken' - Next token of device description.
--
-- 'deviceName', 'describeDevice_deviceName' - The unique ID of the device.
--
-- 'deviceFleetName', 'describeDevice_deviceFleetName' - The name of the fleet the devices belong to.
newDescribeDevice ::
  -- | 'deviceName'
  Prelude.Text ->
  -- | 'deviceFleetName'
  Prelude.Text ->
  DescribeDevice
newDescribeDevice pDeviceName_ pDeviceFleetName_ =
  DescribeDevice'
    { nextToken = Prelude.Nothing,
      deviceName = pDeviceName_,
      deviceFleetName = pDeviceFleetName_
    }

-- | Next token of device description.
describeDevice_nextToken :: Lens.Lens' DescribeDevice (Prelude.Maybe Prelude.Text)
describeDevice_nextToken = Lens.lens (\DescribeDevice' {nextToken} -> nextToken) (\s@DescribeDevice' {} a -> s {nextToken = a} :: DescribeDevice)

-- | The unique ID of the device.
describeDevice_deviceName :: Lens.Lens' DescribeDevice Prelude.Text
describeDevice_deviceName = Lens.lens (\DescribeDevice' {deviceName} -> deviceName) (\s@DescribeDevice' {} a -> s {deviceName = a} :: DescribeDevice)

-- | The name of the fleet the devices belong to.
describeDevice_deviceFleetName :: Lens.Lens' DescribeDevice Prelude.Text
describeDevice_deviceFleetName = Lens.lens (\DescribeDevice' {deviceFleetName} -> deviceFleetName) (\s@DescribeDevice' {} a -> s {deviceFleetName = a} :: DescribeDevice)

instance Core.AWSRequest DescribeDevice where
  type
    AWSResponse DescribeDevice =
      DescribeDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "LatestHeartbeat")
            Prelude.<*> (x Core..?> "MaxModels")
            Prelude.<*> (x Core..?> "DeviceArn")
            Prelude.<*> (x Core..?> "Models" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "IotThingName")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DeviceName")
            Prelude.<*> (x Core..:> "DeviceFleetName")
            Prelude.<*> (x Core..:> "RegistrationTime")
      )

instance Prelude.Hashable DescribeDevice

instance Prelude.NFData DescribeDevice

instance Core.ToHeaders DescribeDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeDevice" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDevice where
  toJSON DescribeDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            Prelude.Just ("DeviceName" Core..= deviceName),
            Prelude.Just
              ("DeviceFleetName" Core..= deviceFleetName)
          ]
      )

instance Core.ToPath DescribeDevice where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeviceResponse' smart constructor.
data DescribeDeviceResponse = DescribeDeviceResponse'
  { -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The last heartbeat received from the device.
    latestHeartbeat :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of models.
    maxModels :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the device.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | Models on the device.
    models :: Prelude.Maybe [EdgeModel],
    -- | The AWS Internet of Things (IoT) object thing name associated with the
    -- device.
    iotThingName :: Prelude.Maybe Prelude.Text,
    -- | A description of the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of the device.
    deviceName :: Prelude.Text,
    -- | The name of the fleet the device belongs to.
    deviceFleetName :: Prelude.Text,
    -- | The timestamp of the last registration or de-reregistration.
    registrationTime :: Core.POSIX
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
-- 'nextToken', 'describeDeviceResponse_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'latestHeartbeat', 'describeDeviceResponse_latestHeartbeat' - The last heartbeat received from the device.
--
-- 'maxModels', 'describeDeviceResponse_maxModels' - The maximum number of models.
--
-- 'deviceArn', 'describeDeviceResponse_deviceArn' - The Amazon Resource Name (ARN) of the device.
--
-- 'models', 'describeDeviceResponse_models' - Models on the device.
--
-- 'iotThingName', 'describeDeviceResponse_iotThingName' - The AWS Internet of Things (IoT) object thing name associated with the
-- device.
--
-- 'description', 'describeDeviceResponse_description' - A description of the device.
--
-- 'httpStatus', 'describeDeviceResponse_httpStatus' - The response's http status code.
--
-- 'deviceName', 'describeDeviceResponse_deviceName' - The unique identifier of the device.
--
-- 'deviceFleetName', 'describeDeviceResponse_deviceFleetName' - The name of the fleet the device belongs to.
--
-- 'registrationTime', 'describeDeviceResponse_registrationTime' - The timestamp of the last registration or de-reregistration.
newDescribeDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deviceName'
  Prelude.Text ->
  -- | 'deviceFleetName'
  Prelude.Text ->
  -- | 'registrationTime'
  Prelude.UTCTime ->
  DescribeDeviceResponse
newDescribeDeviceResponse
  pHttpStatus_
  pDeviceName_
  pDeviceFleetName_
  pRegistrationTime_ =
    DescribeDeviceResponse'
      { nextToken =
          Prelude.Nothing,
        latestHeartbeat = Prelude.Nothing,
        maxModels = Prelude.Nothing,
        deviceArn = Prelude.Nothing,
        models = Prelude.Nothing,
        iotThingName = Prelude.Nothing,
        description = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        deviceName = pDeviceName_,
        deviceFleetName = pDeviceFleetName_,
        registrationTime =
          Core._Time Lens.# pRegistrationTime_
      }

-- | The response from the last list when returning a list large enough to
-- need tokening.
describeDeviceResponse_nextToken :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_nextToken = Lens.lens (\DescribeDeviceResponse' {nextToken} -> nextToken) (\s@DescribeDeviceResponse' {} a -> s {nextToken = a} :: DescribeDeviceResponse)

-- | The last heartbeat received from the device.
describeDeviceResponse_latestHeartbeat :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_latestHeartbeat = Lens.lens (\DescribeDeviceResponse' {latestHeartbeat} -> latestHeartbeat) (\s@DescribeDeviceResponse' {} a -> s {latestHeartbeat = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Core._Time

-- | The maximum number of models.
describeDeviceResponse_maxModels :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Int)
describeDeviceResponse_maxModels = Lens.lens (\DescribeDeviceResponse' {maxModels} -> maxModels) (\s@DescribeDeviceResponse' {} a -> s {maxModels = a} :: DescribeDeviceResponse)

-- | The Amazon Resource Name (ARN) of the device.
describeDeviceResponse_deviceArn :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_deviceArn = Lens.lens (\DescribeDeviceResponse' {deviceArn} -> deviceArn) (\s@DescribeDeviceResponse' {} a -> s {deviceArn = a} :: DescribeDeviceResponse)

-- | Models on the device.
describeDeviceResponse_models :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe [EdgeModel])
describeDeviceResponse_models = Lens.lens (\DescribeDeviceResponse' {models} -> models) (\s@DescribeDeviceResponse' {} a -> s {models = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The AWS Internet of Things (IoT) object thing name associated with the
-- device.
describeDeviceResponse_iotThingName :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_iotThingName = Lens.lens (\DescribeDeviceResponse' {iotThingName} -> iotThingName) (\s@DescribeDeviceResponse' {} a -> s {iotThingName = a} :: DescribeDeviceResponse)

-- | A description of the device.
describeDeviceResponse_description :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_description = Lens.lens (\DescribeDeviceResponse' {description} -> description) (\s@DescribeDeviceResponse' {} a -> s {description = a} :: DescribeDeviceResponse)

-- | The response's http status code.
describeDeviceResponse_httpStatus :: Lens.Lens' DescribeDeviceResponse Prelude.Int
describeDeviceResponse_httpStatus = Lens.lens (\DescribeDeviceResponse' {httpStatus} -> httpStatus) (\s@DescribeDeviceResponse' {} a -> s {httpStatus = a} :: DescribeDeviceResponse)

-- | The unique identifier of the device.
describeDeviceResponse_deviceName :: Lens.Lens' DescribeDeviceResponse Prelude.Text
describeDeviceResponse_deviceName = Lens.lens (\DescribeDeviceResponse' {deviceName} -> deviceName) (\s@DescribeDeviceResponse' {} a -> s {deviceName = a} :: DescribeDeviceResponse)

-- | The name of the fleet the device belongs to.
describeDeviceResponse_deviceFleetName :: Lens.Lens' DescribeDeviceResponse Prelude.Text
describeDeviceResponse_deviceFleetName = Lens.lens (\DescribeDeviceResponse' {deviceFleetName} -> deviceFleetName) (\s@DescribeDeviceResponse' {} a -> s {deviceFleetName = a} :: DescribeDeviceResponse)

-- | The timestamp of the last registration or de-reregistration.
describeDeviceResponse_registrationTime :: Lens.Lens' DescribeDeviceResponse Prelude.UTCTime
describeDeviceResponse_registrationTime = Lens.lens (\DescribeDeviceResponse' {registrationTime} -> registrationTime) (\s@DescribeDeviceResponse' {} a -> s {registrationTime = a} :: DescribeDeviceResponse) Prelude.. Core._Time

instance Prelude.NFData DescribeDeviceResponse
