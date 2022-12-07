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
-- Module      : Amazonka.AlexaBusiness.StartDeviceSync
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.AlexaBusiness.StartDeviceSync
  ( -- * Creating a Request
    StartDeviceSync (..),
    newStartDeviceSync,

    -- * Request Lenses
    startDeviceSync_roomArn,
    startDeviceSync_deviceArn,
    startDeviceSync_features,

    -- * Destructuring the Response
    StartDeviceSyncResponse (..),
    newStartDeviceSyncResponse,

    -- * Response Lenses
    startDeviceSyncResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDeviceSync' smart constructor.
data StartDeviceSync = StartDeviceSync'
  { -- | The ARN of the room with which the device to sync is associated.
    -- Required.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the device to sync. Required.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | Request structure to start the device sync. Required.
    features :: [Feature]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDeviceSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'startDeviceSync_roomArn' - The ARN of the room with which the device to sync is associated.
-- Required.
--
-- 'deviceArn', 'startDeviceSync_deviceArn' - The ARN of the device to sync. Required.
--
-- 'features', 'startDeviceSync_features' - Request structure to start the device sync. Required.
newStartDeviceSync ::
  StartDeviceSync
newStartDeviceSync =
  StartDeviceSync'
    { roomArn = Prelude.Nothing,
      deviceArn = Prelude.Nothing,
      features = Prelude.mempty
    }

-- | The ARN of the room with which the device to sync is associated.
-- Required.
startDeviceSync_roomArn :: Lens.Lens' StartDeviceSync (Prelude.Maybe Prelude.Text)
startDeviceSync_roomArn = Lens.lens (\StartDeviceSync' {roomArn} -> roomArn) (\s@StartDeviceSync' {} a -> s {roomArn = a} :: StartDeviceSync)

-- | The ARN of the device to sync. Required.
startDeviceSync_deviceArn :: Lens.Lens' StartDeviceSync (Prelude.Maybe Prelude.Text)
startDeviceSync_deviceArn = Lens.lens (\StartDeviceSync' {deviceArn} -> deviceArn) (\s@StartDeviceSync' {} a -> s {deviceArn = a} :: StartDeviceSync)

-- | Request structure to start the device sync. Required.
startDeviceSync_features :: Lens.Lens' StartDeviceSync [Feature]
startDeviceSync_features = Lens.lens (\StartDeviceSync' {features} -> features) (\s@StartDeviceSync' {} a -> s {features = a} :: StartDeviceSync) Prelude.. Lens.coerced

instance Core.AWSRequest StartDeviceSync where
  type
    AWSResponse StartDeviceSync =
      StartDeviceSyncResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartDeviceSyncResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDeviceSync where
  hashWithSalt _salt StartDeviceSync' {..} =
    _salt `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` deviceArn
      `Prelude.hashWithSalt` features

instance Prelude.NFData StartDeviceSync where
  rnf StartDeviceSync' {..} =
    Prelude.rnf roomArn
      `Prelude.seq` Prelude.rnf deviceArn
      `Prelude.seq` Prelude.rnf features

instance Data.ToHeaders StartDeviceSync where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.StartDeviceSync" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartDeviceSync where
  toJSON StartDeviceSync' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoomArn" Data..=) Prelude.<$> roomArn,
            ("DeviceArn" Data..=) Prelude.<$> deviceArn,
            Prelude.Just ("Features" Data..= features)
          ]
      )

instance Data.ToPath StartDeviceSync where
  toPath = Prelude.const "/"

instance Data.ToQuery StartDeviceSync where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDeviceSyncResponse' smart constructor.
data StartDeviceSyncResponse = StartDeviceSyncResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartDeviceSyncResponse
newStartDeviceSyncResponse pHttpStatus_ =
  StartDeviceSyncResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startDeviceSyncResponse_httpStatus :: Lens.Lens' StartDeviceSyncResponse Prelude.Int
startDeviceSyncResponse_httpStatus = Lens.lens (\StartDeviceSyncResponse' {httpStatus} -> httpStatus) (\s@StartDeviceSyncResponse' {} a -> s {httpStatus = a} :: StartDeviceSyncResponse)

instance Prelude.NFData StartDeviceSyncResponse where
  rnf StartDeviceSyncResponse' {..} =
    Prelude.rnf httpStatus
