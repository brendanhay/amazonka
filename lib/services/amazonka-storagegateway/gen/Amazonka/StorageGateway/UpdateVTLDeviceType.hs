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
-- Module      : Amazonka.StorageGateway.UpdateVTLDeviceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the type of medium changer in a tape gateway. When you activate
-- a tape gateway, you select a medium changer type for the tape gateway.
-- This operation enables you to select a different type of medium changer
-- after a tape gateway is activated. This operation is only supported in
-- the tape gateway type.
module Amazonka.StorageGateway.UpdateVTLDeviceType
  ( -- * Creating a Request
    UpdateVTLDeviceType (..),
    newUpdateVTLDeviceType,

    -- * Request Lenses
    updateVTLDeviceType_vTLDeviceARN,
    updateVTLDeviceType_deviceType,

    -- * Destructuring the Response
    UpdateVTLDeviceTypeResponse (..),
    newUpdateVTLDeviceTypeResponse,

    -- * Response Lenses
    updateVTLDeviceTypeResponse_vTLDeviceARN,
    updateVTLDeviceTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newUpdateVTLDeviceType' smart constructor.
data UpdateVTLDeviceType = UpdateVTLDeviceType'
  { -- | The Amazon Resource Name (ARN) of the medium changer you want to select.
    vTLDeviceARN :: Prelude.Text,
    -- | The type of medium changer you want to select.
    --
    -- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
    deviceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVTLDeviceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vTLDeviceARN', 'updateVTLDeviceType_vTLDeviceARN' - The Amazon Resource Name (ARN) of the medium changer you want to select.
--
-- 'deviceType', 'updateVTLDeviceType_deviceType' - The type of medium changer you want to select.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
newUpdateVTLDeviceType ::
  -- | 'vTLDeviceARN'
  Prelude.Text ->
  -- | 'deviceType'
  Prelude.Text ->
  UpdateVTLDeviceType
newUpdateVTLDeviceType pVTLDeviceARN_ pDeviceType_ =
  UpdateVTLDeviceType'
    { vTLDeviceARN = pVTLDeviceARN_,
      deviceType = pDeviceType_
    }

-- | The Amazon Resource Name (ARN) of the medium changer you want to select.
updateVTLDeviceType_vTLDeviceARN :: Lens.Lens' UpdateVTLDeviceType Prelude.Text
updateVTLDeviceType_vTLDeviceARN = Lens.lens (\UpdateVTLDeviceType' {vTLDeviceARN} -> vTLDeviceARN) (\s@UpdateVTLDeviceType' {} a -> s {vTLDeviceARN = a} :: UpdateVTLDeviceType)

-- | The type of medium changer you want to select.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
updateVTLDeviceType_deviceType :: Lens.Lens' UpdateVTLDeviceType Prelude.Text
updateVTLDeviceType_deviceType = Lens.lens (\UpdateVTLDeviceType' {deviceType} -> deviceType) (\s@UpdateVTLDeviceType' {} a -> s {deviceType = a} :: UpdateVTLDeviceType)

instance Core.AWSRequest UpdateVTLDeviceType where
  type
    AWSResponse UpdateVTLDeviceType =
      UpdateVTLDeviceTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVTLDeviceTypeResponse'
            Prelude.<$> (x Data..?> "VTLDeviceARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVTLDeviceType where
  hashWithSalt _salt UpdateVTLDeviceType' {..} =
    _salt
      `Prelude.hashWithSalt` vTLDeviceARN
      `Prelude.hashWithSalt` deviceType

instance Prelude.NFData UpdateVTLDeviceType where
  rnf UpdateVTLDeviceType' {..} =
    Prelude.rnf vTLDeviceARN `Prelude.seq`
      Prelude.rnf deviceType

instance Data.ToHeaders UpdateVTLDeviceType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.UpdateVTLDeviceType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVTLDeviceType where
  toJSON UpdateVTLDeviceType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("VTLDeviceARN" Data..= vTLDeviceARN),
            Prelude.Just ("DeviceType" Data..= deviceType)
          ]
      )

instance Data.ToPath UpdateVTLDeviceType where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateVTLDeviceType where
  toQuery = Prelude.const Prelude.mempty

-- | UpdateVTLDeviceTypeOutput
--
-- /See:/ 'newUpdateVTLDeviceTypeResponse' smart constructor.
data UpdateVTLDeviceTypeResponse = UpdateVTLDeviceTypeResponse'
  { -- | The Amazon Resource Name (ARN) of the medium changer you have selected.
    vTLDeviceARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVTLDeviceTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vTLDeviceARN', 'updateVTLDeviceTypeResponse_vTLDeviceARN' - The Amazon Resource Name (ARN) of the medium changer you have selected.
--
-- 'httpStatus', 'updateVTLDeviceTypeResponse_httpStatus' - The response's http status code.
newUpdateVTLDeviceTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVTLDeviceTypeResponse
newUpdateVTLDeviceTypeResponse pHttpStatus_ =
  UpdateVTLDeviceTypeResponse'
    { vTLDeviceARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the medium changer you have selected.
updateVTLDeviceTypeResponse_vTLDeviceARN :: Lens.Lens' UpdateVTLDeviceTypeResponse (Prelude.Maybe Prelude.Text)
updateVTLDeviceTypeResponse_vTLDeviceARN = Lens.lens (\UpdateVTLDeviceTypeResponse' {vTLDeviceARN} -> vTLDeviceARN) (\s@UpdateVTLDeviceTypeResponse' {} a -> s {vTLDeviceARN = a} :: UpdateVTLDeviceTypeResponse)

-- | The response's http status code.
updateVTLDeviceTypeResponse_httpStatus :: Lens.Lens' UpdateVTLDeviceTypeResponse Prelude.Int
updateVTLDeviceTypeResponse_httpStatus = Lens.lens (\UpdateVTLDeviceTypeResponse' {httpStatus} -> httpStatus) (\s@UpdateVTLDeviceTypeResponse' {} a -> s {httpStatus = a} :: UpdateVTLDeviceTypeResponse)

instance Prelude.NFData UpdateVTLDeviceTypeResponse where
  rnf UpdateVTLDeviceTypeResponse' {..} =
    Prelude.rnf vTLDeviceARN `Prelude.seq`
      Prelude.rnf httpStatus
