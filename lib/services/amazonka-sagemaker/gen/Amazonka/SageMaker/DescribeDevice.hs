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
-- Module      : Amazonka.SageMaker.DescribeDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the device.
module Amazonka.SageMaker.DescribeDevice
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
    describeDeviceResponse_agentVersion,
    describeDeviceResponse_description,
    describeDeviceResponse_deviceArn,
    describeDeviceResponse_iotThingName,
    describeDeviceResponse_latestHeartbeat,
    describeDeviceResponse_maxModels,
    describeDeviceResponse_models,
    describeDeviceResponse_nextToken,
    describeDeviceResponse_httpStatus,
    describeDeviceResponse_deviceName,
    describeDeviceResponse_deviceFleetName,
    describeDeviceResponse_registrationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceResponse'
            Prelude.<$> (x Data..?> "AgentVersion")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "DeviceArn")
            Prelude.<*> (x Data..?> "IotThingName")
            Prelude.<*> (x Data..?> "LatestHeartbeat")
            Prelude.<*> (x Data..?> "MaxModels")
            Prelude.<*> (x Data..?> "Models" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DeviceName")
            Prelude.<*> (x Data..:> "DeviceFleetName")
            Prelude.<*> (x Data..:> "RegistrationTime")
      )

instance Prelude.Hashable DescribeDevice where
  hashWithSalt _salt DescribeDevice' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` deviceFleetName

instance Prelude.NFData DescribeDevice where
  rnf DescribeDevice' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf deviceFleetName

instance Data.ToHeaders DescribeDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeDevice" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDevice where
  toJSON DescribeDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("DeviceName" Data..= deviceName),
            Prelude.Just
              ("DeviceFleetName" Data..= deviceFleetName)
          ]
      )

instance Data.ToPath DescribeDevice where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeviceResponse' smart constructor.
data DescribeDeviceResponse = DescribeDeviceResponse'
  { -- | Edge Manager agent version.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the device.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Internet of Things (IoT) object thing name
    -- associated with the device.
    iotThingName :: Prelude.Maybe Prelude.Text,
    -- | The last heartbeat received from the device.
    latestHeartbeat :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of models.
    maxModels :: Prelude.Maybe Prelude.Int,
    -- | Models on the device.
    models :: Prelude.Maybe [EdgeModel],
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of the device.
    deviceName :: Prelude.Text,
    -- | The name of the fleet the device belongs to.
    deviceFleetName :: Prelude.Text,
    -- | The timestamp of the last registration or de-reregistration.
    registrationTime :: Data.POSIX
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
-- 'agentVersion', 'describeDeviceResponse_agentVersion' - Edge Manager agent version.
--
-- 'description', 'describeDeviceResponse_description' - A description of the device.
--
-- 'deviceArn', 'describeDeviceResponse_deviceArn' - The Amazon Resource Name (ARN) of the device.
--
-- 'iotThingName', 'describeDeviceResponse_iotThingName' - The Amazon Web Services Internet of Things (IoT) object thing name
-- associated with the device.
--
-- 'latestHeartbeat', 'describeDeviceResponse_latestHeartbeat' - The last heartbeat received from the device.
--
-- 'maxModels', 'describeDeviceResponse_maxModels' - The maximum number of models.
--
-- 'models', 'describeDeviceResponse_models' - Models on the device.
--
-- 'nextToken', 'describeDeviceResponse_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
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
      { agentVersion =
          Prelude.Nothing,
        description = Prelude.Nothing,
        deviceArn = Prelude.Nothing,
        iotThingName = Prelude.Nothing,
        latestHeartbeat = Prelude.Nothing,
        maxModels = Prelude.Nothing,
        models = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        deviceName = pDeviceName_,
        deviceFleetName = pDeviceFleetName_,
        registrationTime =
          Data._Time Lens.# pRegistrationTime_
      }

-- | Edge Manager agent version.
describeDeviceResponse_agentVersion :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_agentVersion = Lens.lens (\DescribeDeviceResponse' {agentVersion} -> agentVersion) (\s@DescribeDeviceResponse' {} a -> s {agentVersion = a} :: DescribeDeviceResponse)

-- | A description of the device.
describeDeviceResponse_description :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_description = Lens.lens (\DescribeDeviceResponse' {description} -> description) (\s@DescribeDeviceResponse' {} a -> s {description = a} :: DescribeDeviceResponse)

-- | The Amazon Resource Name (ARN) of the device.
describeDeviceResponse_deviceArn :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_deviceArn = Lens.lens (\DescribeDeviceResponse' {deviceArn} -> deviceArn) (\s@DescribeDeviceResponse' {} a -> s {deviceArn = a} :: DescribeDeviceResponse)

-- | The Amazon Web Services Internet of Things (IoT) object thing name
-- associated with the device.
describeDeviceResponse_iotThingName :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_iotThingName = Lens.lens (\DescribeDeviceResponse' {iotThingName} -> iotThingName) (\s@DescribeDeviceResponse' {} a -> s {iotThingName = a} :: DescribeDeviceResponse)

-- | The last heartbeat received from the device.
describeDeviceResponse_latestHeartbeat :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_latestHeartbeat = Lens.lens (\DescribeDeviceResponse' {latestHeartbeat} -> latestHeartbeat) (\s@DescribeDeviceResponse' {} a -> s {latestHeartbeat = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Data._Time

-- | The maximum number of models.
describeDeviceResponse_maxModels :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Int)
describeDeviceResponse_maxModels = Lens.lens (\DescribeDeviceResponse' {maxModels} -> maxModels) (\s@DescribeDeviceResponse' {} a -> s {maxModels = a} :: DescribeDeviceResponse)

-- | Models on the device.
describeDeviceResponse_models :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe [EdgeModel])
describeDeviceResponse_models = Lens.lens (\DescribeDeviceResponse' {models} -> models) (\s@DescribeDeviceResponse' {} a -> s {models = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response from the last list when returning a list large enough to
-- need tokening.
describeDeviceResponse_nextToken :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_nextToken = Lens.lens (\DescribeDeviceResponse' {nextToken} -> nextToken) (\s@DescribeDeviceResponse' {} a -> s {nextToken = a} :: DescribeDeviceResponse)

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
describeDeviceResponse_registrationTime = Lens.lens (\DescribeDeviceResponse' {registrationTime} -> registrationTime) (\s@DescribeDeviceResponse' {} a -> s {registrationTime = a} :: DescribeDeviceResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeDeviceResponse where
  rnf DescribeDeviceResponse' {..} =
    Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceArn
      `Prelude.seq` Prelude.rnf iotThingName
      `Prelude.seq` Prelude.rnf latestHeartbeat
      `Prelude.seq` Prelude.rnf maxModels
      `Prelude.seq` Prelude.rnf models
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf registrationTime
