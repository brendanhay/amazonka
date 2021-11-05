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
-- Module      : Network.AWS.WorkLink.DescribeDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a user\'s device.
module Network.AWS.WorkLink.DescribeDevice
  ( -- * Creating a Request
    DescribeDevice (..),
    newDescribeDevice,

    -- * Request Lenses
    describeDevice_fleetArn,
    describeDevice_deviceId,

    -- * Destructuring the Response
    DescribeDeviceResponse (..),
    newDescribeDeviceResponse,

    -- * Response Lenses
    describeDeviceResponse_status,
    describeDeviceResponse_manufacturer,
    describeDeviceResponse_lastAccessedTime,
    describeDeviceResponse_operatingSystem,
    describeDeviceResponse_username,
    describeDeviceResponse_model,
    describeDeviceResponse_operatingSystemVersion,
    describeDeviceResponse_firstAccessedTime,
    describeDeviceResponse_patchLevel,
    describeDeviceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkLink.Types

-- | /See:/ 'newDescribeDevice' smart constructor.
data DescribeDevice = DescribeDevice'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | A unique identifier for a registered user\'s device.
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
-- 'fleetArn', 'describeDevice_fleetArn' - The ARN of the fleet.
--
-- 'deviceId', 'describeDevice_deviceId' - A unique identifier for a registered user\'s device.
newDescribeDevice ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  DescribeDevice
newDescribeDevice pFleetArn_ pDeviceId_ =
  DescribeDevice'
    { fleetArn = pFleetArn_,
      deviceId = pDeviceId_
    }

-- | The ARN of the fleet.
describeDevice_fleetArn :: Lens.Lens' DescribeDevice Prelude.Text
describeDevice_fleetArn = Lens.lens (\DescribeDevice' {fleetArn} -> fleetArn) (\s@DescribeDevice' {} a -> s {fleetArn = a} :: DescribeDevice)

-- | A unique identifier for a registered user\'s device.
describeDevice_deviceId :: Lens.Lens' DescribeDevice Prelude.Text
describeDevice_deviceId = Lens.lens (\DescribeDevice' {deviceId} -> deviceId) (\s@DescribeDevice' {} a -> s {deviceId = a} :: DescribeDevice)

instance Core.AWSRequest DescribeDevice where
  type
    AWSResponse DescribeDevice =
      DescribeDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Manufacturer")
            Prelude.<*> (x Core..?> "LastAccessedTime")
            Prelude.<*> (x Core..?> "OperatingSystem")
            Prelude.<*> (x Core..?> "Username")
            Prelude.<*> (x Core..?> "Model")
            Prelude.<*> (x Core..?> "OperatingSystemVersion")
            Prelude.<*> (x Core..?> "FirstAccessedTime")
            Prelude.<*> (x Core..?> "PatchLevel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDevice

instance Prelude.NFData DescribeDevice

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

instance Core.ToJSON DescribeDevice where
  toJSON DescribeDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("DeviceId" Core..= deviceId)
          ]
      )

instance Core.ToPath DescribeDevice where
  toPath = Prelude.const "/describeDevice"

instance Core.ToQuery DescribeDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeviceResponse' smart constructor.
data DescribeDeviceResponse = DescribeDeviceResponse'
  { -- | The current state of the device.
    status :: Prelude.Maybe DeviceStatus,
    -- | The manufacturer of the device.
    manufacturer :: Prelude.Maybe Prelude.Text,
    -- | The date that the device last accessed Amazon WorkLink.
    lastAccessedTime :: Prelude.Maybe Core.POSIX,
    -- | The operating system of the device.
    operatingSystem :: Prelude.Maybe Prelude.Text,
    -- | The user name associated with the device.
    username :: Prelude.Maybe Prelude.Text,
    -- | The model of the device.
    model :: Prelude.Maybe Prelude.Text,
    -- | The operating system version of the device.
    operatingSystemVersion :: Prelude.Maybe Prelude.Text,
    -- | The date that the device first signed in to Amazon WorkLink.
    firstAccessedTime :: Prelude.Maybe Core.POSIX,
    -- | The operating system patch level of the device.
    patchLevel :: Prelude.Maybe Prelude.Text,
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
-- 'status', 'describeDeviceResponse_status' - The current state of the device.
--
-- 'manufacturer', 'describeDeviceResponse_manufacturer' - The manufacturer of the device.
--
-- 'lastAccessedTime', 'describeDeviceResponse_lastAccessedTime' - The date that the device last accessed Amazon WorkLink.
--
-- 'operatingSystem', 'describeDeviceResponse_operatingSystem' - The operating system of the device.
--
-- 'username', 'describeDeviceResponse_username' - The user name associated with the device.
--
-- 'model', 'describeDeviceResponse_model' - The model of the device.
--
-- 'operatingSystemVersion', 'describeDeviceResponse_operatingSystemVersion' - The operating system version of the device.
--
-- 'firstAccessedTime', 'describeDeviceResponse_firstAccessedTime' - The date that the device first signed in to Amazon WorkLink.
--
-- 'patchLevel', 'describeDeviceResponse_patchLevel' - The operating system patch level of the device.
--
-- 'httpStatus', 'describeDeviceResponse_httpStatus' - The response's http status code.
newDescribeDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDeviceResponse
newDescribeDeviceResponse pHttpStatus_ =
  DescribeDeviceResponse'
    { status = Prelude.Nothing,
      manufacturer = Prelude.Nothing,
      lastAccessedTime = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      username = Prelude.Nothing,
      model = Prelude.Nothing,
      operatingSystemVersion = Prelude.Nothing,
      firstAccessedTime = Prelude.Nothing,
      patchLevel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the device.
describeDeviceResponse_status :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe DeviceStatus)
describeDeviceResponse_status = Lens.lens (\DescribeDeviceResponse' {status} -> status) (\s@DescribeDeviceResponse' {} a -> s {status = a} :: DescribeDeviceResponse)

-- | The manufacturer of the device.
describeDeviceResponse_manufacturer :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_manufacturer = Lens.lens (\DescribeDeviceResponse' {manufacturer} -> manufacturer) (\s@DescribeDeviceResponse' {} a -> s {manufacturer = a} :: DescribeDeviceResponse)

-- | The date that the device last accessed Amazon WorkLink.
describeDeviceResponse_lastAccessedTime :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_lastAccessedTime = Lens.lens (\DescribeDeviceResponse' {lastAccessedTime} -> lastAccessedTime) (\s@DescribeDeviceResponse' {} a -> s {lastAccessedTime = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Core._Time

-- | The operating system of the device.
describeDeviceResponse_operatingSystem :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_operatingSystem = Lens.lens (\DescribeDeviceResponse' {operatingSystem} -> operatingSystem) (\s@DescribeDeviceResponse' {} a -> s {operatingSystem = a} :: DescribeDeviceResponse)

-- | The user name associated with the device.
describeDeviceResponse_username :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_username = Lens.lens (\DescribeDeviceResponse' {username} -> username) (\s@DescribeDeviceResponse' {} a -> s {username = a} :: DescribeDeviceResponse)

-- | The model of the device.
describeDeviceResponse_model :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_model = Lens.lens (\DescribeDeviceResponse' {model} -> model) (\s@DescribeDeviceResponse' {} a -> s {model = a} :: DescribeDeviceResponse)

-- | The operating system version of the device.
describeDeviceResponse_operatingSystemVersion :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_operatingSystemVersion = Lens.lens (\DescribeDeviceResponse' {operatingSystemVersion} -> operatingSystemVersion) (\s@DescribeDeviceResponse' {} a -> s {operatingSystemVersion = a} :: DescribeDeviceResponse)

-- | The date that the device first signed in to Amazon WorkLink.
describeDeviceResponse_firstAccessedTime :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_firstAccessedTime = Lens.lens (\DescribeDeviceResponse' {firstAccessedTime} -> firstAccessedTime) (\s@DescribeDeviceResponse' {} a -> s {firstAccessedTime = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Core._Time

-- | The operating system patch level of the device.
describeDeviceResponse_patchLevel :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_patchLevel = Lens.lens (\DescribeDeviceResponse' {patchLevel} -> patchLevel) (\s@DescribeDeviceResponse' {} a -> s {patchLevel = a} :: DescribeDeviceResponse)

-- | The response's http status code.
describeDeviceResponse_httpStatus :: Lens.Lens' DescribeDeviceResponse Prelude.Int
describeDeviceResponse_httpStatus = Lens.lens (\DescribeDeviceResponse' {httpStatus} -> httpStatus) (\s@DescribeDeviceResponse' {} a -> s {httpStatus = a} :: DescribeDeviceResponse)

instance Prelude.NFData DescribeDeviceResponse
