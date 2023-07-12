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
-- Module      : Amazonka.DeviceFarm.GetDevicePool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a device pool.
module Amazonka.DeviceFarm.GetDevicePool
  ( -- * Creating a Request
    GetDevicePool (..),
    newGetDevicePool,

    -- * Request Lenses
    getDevicePool_arn,

    -- * Destructuring the Response
    GetDevicePoolResponse (..),
    newGetDevicePoolResponse,

    -- * Response Lenses
    getDevicePoolResponse_devicePool,
    getDevicePoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the get device pool operation.
--
-- /See:/ 'newGetDevicePool' smart constructor.
data GetDevicePool = GetDevicePool'
  { -- | The device pool\'s ARN.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getDevicePool_arn' - The device pool\'s ARN.
newGetDevicePool ::
  -- | 'arn'
  Prelude.Text ->
  GetDevicePool
newGetDevicePool pArn_ = GetDevicePool' {arn = pArn_}

-- | The device pool\'s ARN.
getDevicePool_arn :: Lens.Lens' GetDevicePool Prelude.Text
getDevicePool_arn = Lens.lens (\GetDevicePool' {arn} -> arn) (\s@GetDevicePool' {} a -> s {arn = a} :: GetDevicePool)

instance Core.AWSRequest GetDevicePool where
  type
    AWSResponse GetDevicePool =
      GetDevicePoolResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevicePoolResponse'
            Prelude.<$> (x Data..?> "devicePool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDevicePool where
  hashWithSalt _salt GetDevicePool' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetDevicePool where
  rnf GetDevicePool' {..} = Prelude.rnf arn

instance Data.ToHeaders GetDevicePool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.GetDevicePool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDevicePool where
  toJSON GetDevicePool' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath GetDevicePool where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDevicePool where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a get device pool request.
--
-- /See:/ 'newGetDevicePoolResponse' smart constructor.
data GetDevicePoolResponse = GetDevicePoolResponse'
  { -- | An object that contains information about the requested device pool.
    devicePool :: Prelude.Maybe DevicePool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devicePool', 'getDevicePoolResponse_devicePool' - An object that contains information about the requested device pool.
--
-- 'httpStatus', 'getDevicePoolResponse_httpStatus' - The response's http status code.
newGetDevicePoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDevicePoolResponse
newGetDevicePoolResponse pHttpStatus_ =
  GetDevicePoolResponse'
    { devicePool =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about the requested device pool.
getDevicePoolResponse_devicePool :: Lens.Lens' GetDevicePoolResponse (Prelude.Maybe DevicePool)
getDevicePoolResponse_devicePool = Lens.lens (\GetDevicePoolResponse' {devicePool} -> devicePool) (\s@GetDevicePoolResponse' {} a -> s {devicePool = a} :: GetDevicePoolResponse)

-- | The response's http status code.
getDevicePoolResponse_httpStatus :: Lens.Lens' GetDevicePoolResponse Prelude.Int
getDevicePoolResponse_httpStatus = Lens.lens (\GetDevicePoolResponse' {httpStatus} -> httpStatus) (\s@GetDevicePoolResponse' {} a -> s {httpStatus = a} :: GetDevicePoolResponse)

instance Prelude.NFData GetDevicePoolResponse where
  rnf GetDevicePoolResponse' {..} =
    Prelude.rnf devicePool
      `Prelude.seq` Prelude.rnf httpStatus
