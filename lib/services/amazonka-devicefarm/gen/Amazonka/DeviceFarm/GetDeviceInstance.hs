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
-- Module      : Amazonka.DeviceFarm.GetDeviceInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a device instance that belongs to a private
-- device fleet.
module Amazonka.DeviceFarm.GetDeviceInstance
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDeviceInstance' smart constructor.
data GetDeviceInstance = GetDeviceInstance'
  { -- | The Amazon Resource Name (ARN) of the instance you\'re requesting
    -- information about.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetDeviceInstance
newGetDeviceInstance pArn_ =
  GetDeviceInstance' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the instance you\'re requesting
-- information about.
getDeviceInstance_arn :: Lens.Lens' GetDeviceInstance Prelude.Text
getDeviceInstance_arn = Lens.lens (\GetDeviceInstance' {arn} -> arn) (\s@GetDeviceInstance' {} a -> s {arn = a} :: GetDeviceInstance)

instance Core.AWSRequest GetDeviceInstance where
  type
    AWSResponse GetDeviceInstance =
      GetDeviceInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceInstanceResponse'
            Prelude.<$> (x Data..?> "deviceInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeviceInstance where
  hashWithSalt _salt GetDeviceInstance' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetDeviceInstance where
  rnf GetDeviceInstance' {..} = Prelude.rnf arn

instance Data.ToHeaders GetDeviceInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.GetDeviceInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDeviceInstance where
  toJSON GetDeviceInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath GetDeviceInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDeviceInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeviceInstanceResponse' smart constructor.
data GetDeviceInstanceResponse = GetDeviceInstanceResponse'
  { -- | An object that contains information about your device instance.
    deviceInstance :: Prelude.Maybe DeviceInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetDeviceInstanceResponse
newGetDeviceInstanceResponse pHttpStatus_ =
  GetDeviceInstanceResponse'
    { deviceInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your device instance.
getDeviceInstanceResponse_deviceInstance :: Lens.Lens' GetDeviceInstanceResponse (Prelude.Maybe DeviceInstance)
getDeviceInstanceResponse_deviceInstance = Lens.lens (\GetDeviceInstanceResponse' {deviceInstance} -> deviceInstance) (\s@GetDeviceInstanceResponse' {} a -> s {deviceInstance = a} :: GetDeviceInstanceResponse)

-- | The response's http status code.
getDeviceInstanceResponse_httpStatus :: Lens.Lens' GetDeviceInstanceResponse Prelude.Int
getDeviceInstanceResponse_httpStatus = Lens.lens (\GetDeviceInstanceResponse' {httpStatus} -> httpStatus) (\s@GetDeviceInstanceResponse' {} a -> s {httpStatus = a} :: GetDeviceInstanceResponse)

instance Prelude.NFData GetDeviceInstanceResponse where
  rnf GetDeviceInstanceResponse' {..} =
    Prelude.rnf deviceInstance
      `Prelude.seq` Prelude.rnf httpStatus
