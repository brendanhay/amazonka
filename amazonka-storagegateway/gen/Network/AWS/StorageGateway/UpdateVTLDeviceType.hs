{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.UpdateVTLDeviceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the type of medium changer in a tape gateway. When you activate
-- a tape gateway, you select a medium changer type for the tape gateway.
-- This operation enables you to select a different type of medium changer
-- after a tape gateway is activated. This operation is only supported in
-- the tape gateway type.
module Network.AWS.StorageGateway.UpdateVTLDeviceType
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newUpdateVTLDeviceType' smart constructor.
data UpdateVTLDeviceType = UpdateVTLDeviceType'
  { -- | The Amazon Resource Name (ARN) of the medium changer you want to select.
    vTLDeviceARN :: Prelude.Text,
    -- | The type of medium changer you want to select.
    --
    -- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
    deviceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UpdateVTLDeviceType where
  type
    Rs UpdateVTLDeviceType =
      UpdateVTLDeviceTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVTLDeviceTypeResponse'
            Prelude.<$> (x Prelude..?> "VTLDeviceARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVTLDeviceType

instance Prelude.NFData UpdateVTLDeviceType

instance Prelude.ToHeaders UpdateVTLDeviceType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.UpdateVTLDeviceType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateVTLDeviceType where
  toJSON UpdateVTLDeviceType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VTLDeviceARN" Prelude..= vTLDeviceARN),
            Prelude.Just ("DeviceType" Prelude..= deviceType)
          ]
      )

instance Prelude.ToPath UpdateVTLDeviceType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateVTLDeviceType where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateVTLDeviceTypeResponse
