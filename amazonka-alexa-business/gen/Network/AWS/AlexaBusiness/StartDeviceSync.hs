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
-- Module      : Network.AWS.AlexaBusiness.StartDeviceSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a device and its account to the known default settings. This
-- clears all information and settings set by previous users in the
-- following ways:
--
-- -   Bluetooth - This unpairs all bluetooth devices paired with your echo
--     device.
--
-- -   Volume - This resets the echo device\'s volume to the default value.
--
-- -   Notifications - This clears all notifications from your echo device.
--
-- -   Lists - This clears all to-do items from your echo device.
--
-- -   Settings - This internally syncs the room\'s profile (if the device
--     is assigned to a room), contacts, address books, delegation access
--     for account linking, and communications (if enabled on the room
--     profile).
module Network.AWS.AlexaBusiness.StartDeviceSync
  ( -- * Creating a Request
    StartDeviceSync (..),
    newStartDeviceSync,

    -- * Request Lenses
    startDeviceSync_deviceArn,
    startDeviceSync_roomArn,
    startDeviceSync_features,

    -- * Destructuring the Response
    StartDeviceSyncResponse (..),
    newStartDeviceSyncResponse,

    -- * Response Lenses
    startDeviceSyncResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartDeviceSync' smart constructor.
data StartDeviceSync = StartDeviceSync'
  { -- | The ARN of the device to sync. Required.
    deviceArn :: Core.Maybe Core.Text,
    -- | The ARN of the room with which the device to sync is associated.
    -- Required.
    roomArn :: Core.Maybe Core.Text,
    -- | Request structure to start the device sync. Required.
    features :: [Feature]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartDeviceSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'startDeviceSync_deviceArn' - The ARN of the device to sync. Required.
--
-- 'roomArn', 'startDeviceSync_roomArn' - The ARN of the room with which the device to sync is associated.
-- Required.
--
-- 'features', 'startDeviceSync_features' - Request structure to start the device sync. Required.
newStartDeviceSync ::
  StartDeviceSync
newStartDeviceSync =
  StartDeviceSync'
    { deviceArn = Core.Nothing,
      roomArn = Core.Nothing,
      features = Core.mempty
    }

-- | The ARN of the device to sync. Required.
startDeviceSync_deviceArn :: Lens.Lens' StartDeviceSync (Core.Maybe Core.Text)
startDeviceSync_deviceArn = Lens.lens (\StartDeviceSync' {deviceArn} -> deviceArn) (\s@StartDeviceSync' {} a -> s {deviceArn = a} :: StartDeviceSync)

-- | The ARN of the room with which the device to sync is associated.
-- Required.
startDeviceSync_roomArn :: Lens.Lens' StartDeviceSync (Core.Maybe Core.Text)
startDeviceSync_roomArn = Lens.lens (\StartDeviceSync' {roomArn} -> roomArn) (\s@StartDeviceSync' {} a -> s {roomArn = a} :: StartDeviceSync)

-- | Request structure to start the device sync. Required.
startDeviceSync_features :: Lens.Lens' StartDeviceSync [Feature]
startDeviceSync_features = Lens.lens (\StartDeviceSync' {features} -> features) (\s@StartDeviceSync' {} a -> s {features = a} :: StartDeviceSync) Core.. Lens._Coerce

instance Core.AWSRequest StartDeviceSync where
  type
    AWSResponse StartDeviceSync =
      StartDeviceSyncResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartDeviceSyncResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartDeviceSync

instance Core.NFData StartDeviceSync

instance Core.ToHeaders StartDeviceSync where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.StartDeviceSync" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartDeviceSync where
  toJSON StartDeviceSync' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeviceArn" Core..=) Core.<$> deviceArn,
            ("RoomArn" Core..=) Core.<$> roomArn,
            Core.Just ("Features" Core..= features)
          ]
      )

instance Core.ToPath StartDeviceSync where
  toPath = Core.const "/"

instance Core.ToQuery StartDeviceSync where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartDeviceSyncResponse' smart constructor.
data StartDeviceSyncResponse = StartDeviceSyncResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartDeviceSyncResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startDeviceSyncResponse_httpStatus' - The response's http status code.
newStartDeviceSyncResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartDeviceSyncResponse
newStartDeviceSyncResponse pHttpStatus_ =
  StartDeviceSyncResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startDeviceSyncResponse_httpStatus :: Lens.Lens' StartDeviceSyncResponse Core.Int
startDeviceSyncResponse_httpStatus = Lens.lens (\StartDeviceSyncResponse' {httpStatus} -> httpStatus) (\s@StartDeviceSyncResponse' {} a -> s {httpStatus = a} :: StartDeviceSyncResponse)

instance Core.NFData StartDeviceSyncResponse
