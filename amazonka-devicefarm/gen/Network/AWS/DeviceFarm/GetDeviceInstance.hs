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
-- Module      : Network.AWS.DeviceFarm.GetDeviceInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a device instance that belongs to a private
-- device fleet.
module Network.AWS.DeviceFarm.GetDeviceInstance
  ( -- * Creating a Request
    GetDeviceInstance (..),
    newGetDeviceInstance,

    -- * Request Lenses
    getDeviceInstance_arn,

    -- * Destructuring the Response
    GetDeviceInstanceResponse (..),
    newGetDeviceInstanceResponse,

    -- * Response Lenses
    getDeviceInstanceResponse_deviceInstance,
    getDeviceInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDeviceInstance' smart constructor.
data GetDeviceInstance = GetDeviceInstance'
  { -- | The Amazon Resource Name (ARN) of the instance you\'re requesting
    -- information about.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeviceInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getDeviceInstance_arn' - The Amazon Resource Name (ARN) of the instance you\'re requesting
-- information about.
newGetDeviceInstance ::
  -- | 'arn'
  Core.Text ->
  GetDeviceInstance
newGetDeviceInstance pArn_ =
  GetDeviceInstance' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the instance you\'re requesting
-- information about.
getDeviceInstance_arn :: Lens.Lens' GetDeviceInstance Core.Text
getDeviceInstance_arn = Lens.lens (\GetDeviceInstance' {arn} -> arn) (\s@GetDeviceInstance' {} a -> s {arn = a} :: GetDeviceInstance)

instance Core.AWSRequest GetDeviceInstance where
  type
    AWSResponse GetDeviceInstance =
      GetDeviceInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceInstanceResponse'
            Core.<$> (x Core..?> "deviceInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDeviceInstance

instance Core.NFData GetDeviceInstance

instance Core.ToHeaders GetDeviceInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetDeviceInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDeviceInstance where
  toJSON GetDeviceInstance' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath GetDeviceInstance where
  toPath = Core.const "/"

instance Core.ToQuery GetDeviceInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDeviceInstanceResponse' smart constructor.
data GetDeviceInstanceResponse = GetDeviceInstanceResponse'
  { -- | An object that contains information about your device instance.
    deviceInstance :: Core.Maybe DeviceInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeviceInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceInstance', 'getDeviceInstanceResponse_deviceInstance' - An object that contains information about your device instance.
--
-- 'httpStatus', 'getDeviceInstanceResponse_httpStatus' - The response's http status code.
newGetDeviceInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDeviceInstanceResponse
newGetDeviceInstanceResponse pHttpStatus_ =
  GetDeviceInstanceResponse'
    { deviceInstance =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your device instance.
getDeviceInstanceResponse_deviceInstance :: Lens.Lens' GetDeviceInstanceResponse (Core.Maybe DeviceInstance)
getDeviceInstanceResponse_deviceInstance = Lens.lens (\GetDeviceInstanceResponse' {deviceInstance} -> deviceInstance) (\s@GetDeviceInstanceResponse' {} a -> s {deviceInstance = a} :: GetDeviceInstanceResponse)

-- | The response's http status code.
getDeviceInstanceResponse_httpStatus :: Lens.Lens' GetDeviceInstanceResponse Core.Int
getDeviceInstanceResponse_httpStatus = Lens.lens (\GetDeviceInstanceResponse' {httpStatus} -> httpStatus) (\s@GetDeviceInstanceResponse' {} a -> s {httpStatus = a} :: GetDeviceInstanceResponse)

instance Core.NFData GetDeviceInstanceResponse
