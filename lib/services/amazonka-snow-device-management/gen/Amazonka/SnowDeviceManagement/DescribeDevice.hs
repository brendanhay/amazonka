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
-- Module      : Amazonka.SnowDeviceManagement.DescribeDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks device-specific information, such as the device type, software
-- version, IP addresses, and lock status.
module Amazonka.SnowDeviceManagement.DescribeDevice
  ( -- * Creating a Request
    DescribeDevice (..),
    newDescribeDevice,

    -- * Request Lenses
    describeDevice_managedDeviceId,

    -- * Destructuring the Response
    DescribeDeviceResponse (..),
    newDescribeDeviceResponse,

    -- * Response Lenses
    describeDeviceResponse_deviceState,
    describeDeviceResponse_deviceCapacities,
    describeDeviceResponse_lastUpdatedAt,
    describeDeviceResponse_associatedWithJob,
    describeDeviceResponse_lastReachedOutAt,
    describeDeviceResponse_software,
    describeDeviceResponse_physicalNetworkInterfaces,
    describeDeviceResponse_managedDeviceId,
    describeDeviceResponse_managedDeviceArn,
    describeDeviceResponse_deviceType,
    describeDeviceResponse_tags,
    describeDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SnowDeviceManagement.Types

-- | /See:/ 'newDescribeDevice' smart constructor.
data DescribeDevice = DescribeDevice'
  { -- | The ID of the device that you are checking the information of.
    managedDeviceId :: Prelude.Text
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
-- 'managedDeviceId', 'describeDevice_managedDeviceId' - The ID of the device that you are checking the information of.
newDescribeDevice ::
  -- | 'managedDeviceId'
  Prelude.Text ->
  DescribeDevice
newDescribeDevice pManagedDeviceId_ =
  DescribeDevice'
    { managedDeviceId =
        pManagedDeviceId_
    }

-- | The ID of the device that you are checking the information of.
describeDevice_managedDeviceId :: Lens.Lens' DescribeDevice Prelude.Text
describeDevice_managedDeviceId = Lens.lens (\DescribeDevice' {managedDeviceId} -> managedDeviceId) (\s@DescribeDevice' {} a -> s {managedDeviceId = a} :: DescribeDevice)

instance Core.AWSRequest DescribeDevice where
  type
    AWSResponse DescribeDevice =
      DescribeDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceResponse'
            Prelude.<$> (x Core..?> "deviceState")
            Prelude.<*> ( x Core..?> "deviceCapacities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "lastUpdatedAt")
            Prelude.<*> (x Core..?> "associatedWithJob")
            Prelude.<*> (x Core..?> "lastReachedOutAt")
            Prelude.<*> (x Core..?> "software")
            Prelude.<*> ( x Core..?> "physicalNetworkInterfaces"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "managedDeviceId")
            Prelude.<*> (x Core..?> "managedDeviceArn")
            Prelude.<*> (x Core..?> "deviceType")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDevice where
  hashWithSalt salt' DescribeDevice' {..} =
    salt' `Prelude.hashWithSalt` managedDeviceId

instance Prelude.NFData DescribeDevice where
  rnf DescribeDevice' {..} = Prelude.rnf managedDeviceId

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
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeDevice where
  toPath DescribeDevice' {..} =
    Prelude.mconcat
      [ "/managed-device/",
        Core.toBS managedDeviceId,
        "/describe"
      ]

instance Core.ToQuery DescribeDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeviceResponse' smart constructor.
data DescribeDeviceResponse = DescribeDeviceResponse'
  { -- | The current state of the device.
    deviceState :: Prelude.Maybe UnlockState,
    -- | The hardware specifications of the device.
    deviceCapacities :: Prelude.Maybe [Capacity],
    -- | When the device last pushed an update to the Amazon Web Services Cloud.
    -- Indicates when the device cache was refreshed.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the job used when ordering the device.
    associatedWithJob :: Prelude.Maybe Prelude.Text,
    -- | When the device last contacted the Amazon Web Services Cloud. Indicates
    -- that the device is online.
    lastReachedOutAt :: Prelude.Maybe Core.POSIX,
    -- | The software installed on the device.
    software :: Prelude.Maybe SoftwareInformation,
    -- | The network interfaces available on the device.
    physicalNetworkInterfaces :: Prelude.Maybe [PhysicalNetworkInterface],
    -- | The ID of the device that you checked the information for.
    managedDeviceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the device.
    managedDeviceArn :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services Snow Family device.
    deviceType :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata that you assign to a resource. You can use tags to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'deviceState', 'describeDeviceResponse_deviceState' - The current state of the device.
--
-- 'deviceCapacities', 'describeDeviceResponse_deviceCapacities' - The hardware specifications of the device.
--
-- 'lastUpdatedAt', 'describeDeviceResponse_lastUpdatedAt' - When the device last pushed an update to the Amazon Web Services Cloud.
-- Indicates when the device cache was refreshed.
--
-- 'associatedWithJob', 'describeDeviceResponse_associatedWithJob' - The ID of the job used when ordering the device.
--
-- 'lastReachedOutAt', 'describeDeviceResponse_lastReachedOutAt' - When the device last contacted the Amazon Web Services Cloud. Indicates
-- that the device is online.
--
-- 'software', 'describeDeviceResponse_software' - The software installed on the device.
--
-- 'physicalNetworkInterfaces', 'describeDeviceResponse_physicalNetworkInterfaces' - The network interfaces available on the device.
--
-- 'managedDeviceId', 'describeDeviceResponse_managedDeviceId' - The ID of the device that you checked the information for.
--
-- 'managedDeviceArn', 'describeDeviceResponse_managedDeviceArn' - The Amazon Resource Name (ARN) of the device.
--
-- 'deviceType', 'describeDeviceResponse_deviceType' - The type of Amazon Web Services Snow Family device.
--
-- 'tags', 'describeDeviceResponse_tags' - Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
--
-- 'httpStatus', 'describeDeviceResponse_httpStatus' - The response's http status code.
newDescribeDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDeviceResponse
newDescribeDeviceResponse pHttpStatus_ =
  DescribeDeviceResponse'
    { deviceState =
        Prelude.Nothing,
      deviceCapacities = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      associatedWithJob = Prelude.Nothing,
      lastReachedOutAt = Prelude.Nothing,
      software = Prelude.Nothing,
      physicalNetworkInterfaces = Prelude.Nothing,
      managedDeviceId = Prelude.Nothing,
      managedDeviceArn = Prelude.Nothing,
      deviceType = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the device.
describeDeviceResponse_deviceState :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe UnlockState)
describeDeviceResponse_deviceState = Lens.lens (\DescribeDeviceResponse' {deviceState} -> deviceState) (\s@DescribeDeviceResponse' {} a -> s {deviceState = a} :: DescribeDeviceResponse)

-- | The hardware specifications of the device.
describeDeviceResponse_deviceCapacities :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe [Capacity])
describeDeviceResponse_deviceCapacities = Lens.lens (\DescribeDeviceResponse' {deviceCapacities} -> deviceCapacities) (\s@DescribeDeviceResponse' {} a -> s {deviceCapacities = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | When the device last pushed an update to the Amazon Web Services Cloud.
-- Indicates when the device cache was refreshed.
describeDeviceResponse_lastUpdatedAt :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_lastUpdatedAt = Lens.lens (\DescribeDeviceResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeDeviceResponse' {} a -> s {lastUpdatedAt = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the job used when ordering the device.
describeDeviceResponse_associatedWithJob :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_associatedWithJob = Lens.lens (\DescribeDeviceResponse' {associatedWithJob} -> associatedWithJob) (\s@DescribeDeviceResponse' {} a -> s {associatedWithJob = a} :: DescribeDeviceResponse)

-- | When the device last contacted the Amazon Web Services Cloud. Indicates
-- that the device is online.
describeDeviceResponse_lastReachedOutAt :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceResponse_lastReachedOutAt = Lens.lens (\DescribeDeviceResponse' {lastReachedOutAt} -> lastReachedOutAt) (\s@DescribeDeviceResponse' {} a -> s {lastReachedOutAt = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Core._Time

-- | The software installed on the device.
describeDeviceResponse_software :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe SoftwareInformation)
describeDeviceResponse_software = Lens.lens (\DescribeDeviceResponse' {software} -> software) (\s@DescribeDeviceResponse' {} a -> s {software = a} :: DescribeDeviceResponse)

-- | The network interfaces available on the device.
describeDeviceResponse_physicalNetworkInterfaces :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe [PhysicalNetworkInterface])
describeDeviceResponse_physicalNetworkInterfaces = Lens.lens (\DescribeDeviceResponse' {physicalNetworkInterfaces} -> physicalNetworkInterfaces) (\s@DescribeDeviceResponse' {} a -> s {physicalNetworkInterfaces = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the device that you checked the information for.
describeDeviceResponse_managedDeviceId :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_managedDeviceId = Lens.lens (\DescribeDeviceResponse' {managedDeviceId} -> managedDeviceId) (\s@DescribeDeviceResponse' {} a -> s {managedDeviceId = a} :: DescribeDeviceResponse)

-- | The Amazon Resource Name (ARN) of the device.
describeDeviceResponse_managedDeviceArn :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_managedDeviceArn = Lens.lens (\DescribeDeviceResponse' {managedDeviceArn} -> managedDeviceArn) (\s@DescribeDeviceResponse' {} a -> s {managedDeviceArn = a} :: DescribeDeviceResponse)

-- | The type of Amazon Web Services Snow Family device.
describeDeviceResponse_deviceType :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe Prelude.Text)
describeDeviceResponse_deviceType = Lens.lens (\DescribeDeviceResponse' {deviceType} -> deviceType) (\s@DescribeDeviceResponse' {} a -> s {deviceType = a} :: DescribeDeviceResponse)

-- | Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
describeDeviceResponse_tags :: Lens.Lens' DescribeDeviceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeDeviceResponse_tags = Lens.lens (\DescribeDeviceResponse' {tags} -> tags) (\s@DescribeDeviceResponse' {} a -> s {tags = a} :: DescribeDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDeviceResponse_httpStatus :: Lens.Lens' DescribeDeviceResponse Prelude.Int
describeDeviceResponse_httpStatus = Lens.lens (\DescribeDeviceResponse' {httpStatus} -> httpStatus) (\s@DescribeDeviceResponse' {} a -> s {httpStatus = a} :: DescribeDeviceResponse)

instance Prelude.NFData DescribeDeviceResponse where
  rnf DescribeDeviceResponse' {..} =
    Prelude.rnf deviceState
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf deviceType
      `Prelude.seq` Prelude.rnf managedDeviceArn
      `Prelude.seq` Prelude.rnf managedDeviceId
      `Prelude.seq` Prelude.rnf physicalNetworkInterfaces
      `Prelude.seq` Prelude.rnf software
      `Prelude.seq` Prelude.rnf lastReachedOutAt
      `Prelude.seq` Prelude.rnf associatedWithJob
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf deviceCapacities
