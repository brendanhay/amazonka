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
-- Module      : Network.AWS.SageMaker.DescribeDeviceFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A description of the fleet the device belongs to.
module Network.AWS.SageMaker.DescribeDeviceFleet
  ( -- * Creating a Request
    DescribeDeviceFleet (..),
    newDescribeDeviceFleet,

    -- * Request Lenses
    describeDeviceFleet_deviceFleetName,

    -- * Destructuring the Response
    DescribeDeviceFleetResponse (..),
    newDescribeDeviceFleetResponse,

    -- * Response Lenses
    describeDeviceFleetResponse_roleArn,
    describeDeviceFleetResponse_iotRoleAlias,
    describeDeviceFleetResponse_description,
    describeDeviceFleetResponse_httpStatus,
    describeDeviceFleetResponse_deviceFleetName,
    describeDeviceFleetResponse_deviceFleetArn,
    describeDeviceFleetResponse_outputConfig,
    describeDeviceFleetResponse_creationTime,
    describeDeviceFleetResponse_lastModifiedTime,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeDeviceFleet' smart constructor.
data DescribeDeviceFleet = DescribeDeviceFleet'
  { -- | The name of the fleet.
    deviceFleetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDeviceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceFleetName', 'describeDeviceFleet_deviceFleetName' - The name of the fleet.
newDescribeDeviceFleet ::
  -- | 'deviceFleetName'
  Core.Text ->
  DescribeDeviceFleet
newDescribeDeviceFleet pDeviceFleetName_ =
  DescribeDeviceFleet'
    { deviceFleetName =
        pDeviceFleetName_
    }

-- | The name of the fleet.
describeDeviceFleet_deviceFleetName :: Lens.Lens' DescribeDeviceFleet Core.Text
describeDeviceFleet_deviceFleetName = Lens.lens (\DescribeDeviceFleet' {deviceFleetName} -> deviceFleetName) (\s@DescribeDeviceFleet' {} a -> s {deviceFleetName = a} :: DescribeDeviceFleet)

instance Core.AWSRequest DescribeDeviceFleet where
  type
    AWSResponse DescribeDeviceFleet =
      DescribeDeviceFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceFleetResponse'
            Core.<$> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "IotRoleAlias")
            Core.<*> (x Core..?> "Description")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "DeviceFleetName")
            Core.<*> (x Core..:> "DeviceFleetArn")
            Core.<*> (x Core..:> "OutputConfig")
            Core.<*> (x Core..:> "CreationTime")
            Core.<*> (x Core..:> "LastModifiedTime")
      )

instance Core.Hashable DescribeDeviceFleet

instance Core.NFData DescribeDeviceFleet

instance Core.ToHeaders DescribeDeviceFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeDeviceFleet" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDeviceFleet where
  toJSON DescribeDeviceFleet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DeviceFleetName" Core..= deviceFleetName)
          ]
      )

instance Core.ToPath DescribeDeviceFleet where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDeviceFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDeviceFleetResponse' smart constructor.
data DescribeDeviceFleetResponse = DescribeDeviceFleetResponse'
  { -- | The Amazon Resource Name (ARN) that has access to AWS Internet of Things
    -- (IoT).
    roleArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) alias created in AWS Internet of Things
    -- (IoT).
    iotRoleAlias :: Core.Maybe Core.Text,
    -- | A description of the fleet.
    description :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The name of the fleet.
    deviceFleetName :: Core.Text,
    -- | The The Amazon Resource Name (ARN) of the fleet.
    deviceFleetArn :: Core.Text,
    -- | The output configuration for storing sampled data.
    outputConfig :: EdgeOutputConfig,
    -- | Timestamp of when the device fleet was created.
    creationTime :: Core.POSIX,
    -- | Timestamp of when the device fleet was last updated.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDeviceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'describeDeviceFleetResponse_roleArn' - The Amazon Resource Name (ARN) that has access to AWS Internet of Things
-- (IoT).
--
-- 'iotRoleAlias', 'describeDeviceFleetResponse_iotRoleAlias' - The Amazon Resource Name (ARN) alias created in AWS Internet of Things
-- (IoT).
--
-- 'description', 'describeDeviceFleetResponse_description' - A description of the fleet.
--
-- 'httpStatus', 'describeDeviceFleetResponse_httpStatus' - The response's http status code.
--
-- 'deviceFleetName', 'describeDeviceFleetResponse_deviceFleetName' - The name of the fleet.
--
-- 'deviceFleetArn', 'describeDeviceFleetResponse_deviceFleetArn' - The The Amazon Resource Name (ARN) of the fleet.
--
-- 'outputConfig', 'describeDeviceFleetResponse_outputConfig' - The output configuration for storing sampled data.
--
-- 'creationTime', 'describeDeviceFleetResponse_creationTime' - Timestamp of when the device fleet was created.
--
-- 'lastModifiedTime', 'describeDeviceFleetResponse_lastModifiedTime' - Timestamp of when the device fleet was last updated.
newDescribeDeviceFleetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'deviceFleetName'
  Core.Text ->
  -- | 'deviceFleetArn'
  Core.Text ->
  -- | 'outputConfig'
  EdgeOutputConfig ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  DescribeDeviceFleetResponse
newDescribeDeviceFleetResponse
  pHttpStatus_
  pDeviceFleetName_
  pDeviceFleetArn_
  pOutputConfig_
  pCreationTime_
  pLastModifiedTime_ =
    DescribeDeviceFleetResponse'
      { roleArn =
          Core.Nothing,
        iotRoleAlias = Core.Nothing,
        description = Core.Nothing,
        httpStatus = pHttpStatus_,
        deviceFleetName = pDeviceFleetName_,
        deviceFleetArn = pDeviceFleetArn_,
        outputConfig = pOutputConfig_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | The Amazon Resource Name (ARN) that has access to AWS Internet of Things
-- (IoT).
describeDeviceFleetResponse_roleArn :: Lens.Lens' DescribeDeviceFleetResponse (Core.Maybe Core.Text)
describeDeviceFleetResponse_roleArn = Lens.lens (\DescribeDeviceFleetResponse' {roleArn} -> roleArn) (\s@DescribeDeviceFleetResponse' {} a -> s {roleArn = a} :: DescribeDeviceFleetResponse)

-- | The Amazon Resource Name (ARN) alias created in AWS Internet of Things
-- (IoT).
describeDeviceFleetResponse_iotRoleAlias :: Lens.Lens' DescribeDeviceFleetResponse (Core.Maybe Core.Text)
describeDeviceFleetResponse_iotRoleAlias = Lens.lens (\DescribeDeviceFleetResponse' {iotRoleAlias} -> iotRoleAlias) (\s@DescribeDeviceFleetResponse' {} a -> s {iotRoleAlias = a} :: DescribeDeviceFleetResponse)

-- | A description of the fleet.
describeDeviceFleetResponse_description :: Lens.Lens' DescribeDeviceFleetResponse (Core.Maybe Core.Text)
describeDeviceFleetResponse_description = Lens.lens (\DescribeDeviceFleetResponse' {description} -> description) (\s@DescribeDeviceFleetResponse' {} a -> s {description = a} :: DescribeDeviceFleetResponse)

-- | The response's http status code.
describeDeviceFleetResponse_httpStatus :: Lens.Lens' DescribeDeviceFleetResponse Core.Int
describeDeviceFleetResponse_httpStatus = Lens.lens (\DescribeDeviceFleetResponse' {httpStatus} -> httpStatus) (\s@DescribeDeviceFleetResponse' {} a -> s {httpStatus = a} :: DescribeDeviceFleetResponse)

-- | The name of the fleet.
describeDeviceFleetResponse_deviceFleetName :: Lens.Lens' DescribeDeviceFleetResponse Core.Text
describeDeviceFleetResponse_deviceFleetName = Lens.lens (\DescribeDeviceFleetResponse' {deviceFleetName} -> deviceFleetName) (\s@DescribeDeviceFleetResponse' {} a -> s {deviceFleetName = a} :: DescribeDeviceFleetResponse)

-- | The The Amazon Resource Name (ARN) of the fleet.
describeDeviceFleetResponse_deviceFleetArn :: Lens.Lens' DescribeDeviceFleetResponse Core.Text
describeDeviceFleetResponse_deviceFleetArn = Lens.lens (\DescribeDeviceFleetResponse' {deviceFleetArn} -> deviceFleetArn) (\s@DescribeDeviceFleetResponse' {} a -> s {deviceFleetArn = a} :: DescribeDeviceFleetResponse)

-- | The output configuration for storing sampled data.
describeDeviceFleetResponse_outputConfig :: Lens.Lens' DescribeDeviceFleetResponse EdgeOutputConfig
describeDeviceFleetResponse_outputConfig = Lens.lens (\DescribeDeviceFleetResponse' {outputConfig} -> outputConfig) (\s@DescribeDeviceFleetResponse' {} a -> s {outputConfig = a} :: DescribeDeviceFleetResponse)

-- | Timestamp of when the device fleet was created.
describeDeviceFleetResponse_creationTime :: Lens.Lens' DescribeDeviceFleetResponse Core.UTCTime
describeDeviceFleetResponse_creationTime = Lens.lens (\DescribeDeviceFleetResponse' {creationTime} -> creationTime) (\s@DescribeDeviceFleetResponse' {} a -> s {creationTime = a} :: DescribeDeviceFleetResponse) Core.. Core._Time

-- | Timestamp of when the device fleet was last updated.
describeDeviceFleetResponse_lastModifiedTime :: Lens.Lens' DescribeDeviceFleetResponse Core.UTCTime
describeDeviceFleetResponse_lastModifiedTime = Lens.lens (\DescribeDeviceFleetResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeDeviceFleetResponse' {} a -> s {lastModifiedTime = a} :: DescribeDeviceFleetResponse) Core.. Core._Time

instance Core.NFData DescribeDeviceFleetResponse
