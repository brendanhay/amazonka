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
-- Module      : Amazonka.AlexaBusiness.RegisterAVSDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Alexa-enabled device built by an Original Equipment
-- Manufacturer (OEM) using Alexa Voice Service (AVS).
module Amazonka.AlexaBusiness.RegisterAVSDevice
  ( -- * Creating a Request
    RegisterAVSDevice (..),
    newRegisterAVSDevice,

    -- * Request Lenses
    registerAVSDevice_deviceSerialNumber,
    registerAVSDevice_roomArn,
    registerAVSDevice_tags,
    registerAVSDevice_clientId,
    registerAVSDevice_userCode,
    registerAVSDevice_productId,
    registerAVSDevice_amazonId,

    -- * Destructuring the Response
    RegisterAVSDeviceResponse (..),
    newRegisterAVSDeviceResponse,

    -- * Response Lenses
    registerAVSDeviceResponse_deviceArn,
    registerAVSDeviceResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterAVSDevice' smart constructor.
data RegisterAVSDevice = RegisterAVSDevice'
  { -- | The key generated by the OEM that uniquely identifies a specified
    -- instance of your AVS device.
    deviceSerialNumber :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the room with which to associate your
    -- AVS device.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Prelude.Maybe [Tag],
    -- | The client ID of the OEM used for code-based linking authorization on an
    -- AVS device.
    clientId :: Prelude.Text,
    -- | The code that is obtained after your AVS device has made a POST request
    -- to LWA as a part of the Device Authorization Request component of the
    -- OAuth code-based linking specification.
    userCode :: Prelude.Text,
    -- | The product ID used to identify your AVS device during authorization.
    productId :: Prelude.Text,
    -- | The device type ID for your AVS device generated by Amazon when the OEM
    -- creates a new product on Amazon\'s Developer Console.
    amazonId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAVSDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceSerialNumber', 'registerAVSDevice_deviceSerialNumber' - The key generated by the OEM that uniquely identifies a specified
-- instance of your AVS device.
--
-- 'roomArn', 'registerAVSDevice_roomArn' - The Amazon Resource Name (ARN) of the room with which to associate your
-- AVS device.
--
-- 'tags', 'registerAVSDevice_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'clientId', 'registerAVSDevice_clientId' - The client ID of the OEM used for code-based linking authorization on an
-- AVS device.
--
-- 'userCode', 'registerAVSDevice_userCode' - The code that is obtained after your AVS device has made a POST request
-- to LWA as a part of the Device Authorization Request component of the
-- OAuth code-based linking specification.
--
-- 'productId', 'registerAVSDevice_productId' - The product ID used to identify your AVS device during authorization.
--
-- 'amazonId', 'registerAVSDevice_amazonId' - The device type ID for your AVS device generated by Amazon when the OEM
-- creates a new product on Amazon\'s Developer Console.
newRegisterAVSDevice ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'userCode'
  Prelude.Text ->
  -- | 'productId'
  Prelude.Text ->
  -- | 'amazonId'
  Prelude.Text ->
  RegisterAVSDevice
newRegisterAVSDevice
  pClientId_
  pUserCode_
  pProductId_
  pAmazonId_ =
    RegisterAVSDevice'
      { deviceSerialNumber =
          Prelude.Nothing,
        roomArn = Prelude.Nothing,
        tags = Prelude.Nothing,
        clientId = pClientId_,
        userCode = pUserCode_,
        productId = pProductId_,
        amazonId = pAmazonId_
      }

-- | The key generated by the OEM that uniquely identifies a specified
-- instance of your AVS device.
registerAVSDevice_deviceSerialNumber :: Lens.Lens' RegisterAVSDevice (Prelude.Maybe Prelude.Text)
registerAVSDevice_deviceSerialNumber = Lens.lens (\RegisterAVSDevice' {deviceSerialNumber} -> deviceSerialNumber) (\s@RegisterAVSDevice' {} a -> s {deviceSerialNumber = a} :: RegisterAVSDevice)

-- | The Amazon Resource Name (ARN) of the room with which to associate your
-- AVS device.
registerAVSDevice_roomArn :: Lens.Lens' RegisterAVSDevice (Prelude.Maybe Prelude.Text)
registerAVSDevice_roomArn = Lens.lens (\RegisterAVSDevice' {roomArn} -> roomArn) (\s@RegisterAVSDevice' {} a -> s {roomArn = a} :: RegisterAVSDevice)

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
registerAVSDevice_tags :: Lens.Lens' RegisterAVSDevice (Prelude.Maybe [Tag])
registerAVSDevice_tags = Lens.lens (\RegisterAVSDevice' {tags} -> tags) (\s@RegisterAVSDevice' {} a -> s {tags = a} :: RegisterAVSDevice) Prelude.. Lens.mapping Lens.coerced

-- | The client ID of the OEM used for code-based linking authorization on an
-- AVS device.
registerAVSDevice_clientId :: Lens.Lens' RegisterAVSDevice Prelude.Text
registerAVSDevice_clientId = Lens.lens (\RegisterAVSDevice' {clientId} -> clientId) (\s@RegisterAVSDevice' {} a -> s {clientId = a} :: RegisterAVSDevice)

-- | The code that is obtained after your AVS device has made a POST request
-- to LWA as a part of the Device Authorization Request component of the
-- OAuth code-based linking specification.
registerAVSDevice_userCode :: Lens.Lens' RegisterAVSDevice Prelude.Text
registerAVSDevice_userCode = Lens.lens (\RegisterAVSDevice' {userCode} -> userCode) (\s@RegisterAVSDevice' {} a -> s {userCode = a} :: RegisterAVSDevice)

-- | The product ID used to identify your AVS device during authorization.
registerAVSDevice_productId :: Lens.Lens' RegisterAVSDevice Prelude.Text
registerAVSDevice_productId = Lens.lens (\RegisterAVSDevice' {productId} -> productId) (\s@RegisterAVSDevice' {} a -> s {productId = a} :: RegisterAVSDevice)

-- | The device type ID for your AVS device generated by Amazon when the OEM
-- creates a new product on Amazon\'s Developer Console.
registerAVSDevice_amazonId :: Lens.Lens' RegisterAVSDevice Prelude.Text
registerAVSDevice_amazonId = Lens.lens (\RegisterAVSDevice' {amazonId} -> amazonId) (\s@RegisterAVSDevice' {} a -> s {amazonId = a} :: RegisterAVSDevice)

instance Core.AWSRequest RegisterAVSDevice where
  type
    AWSResponse RegisterAVSDevice =
      RegisterAVSDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterAVSDeviceResponse'
            Prelude.<$> (x Data..?> "DeviceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterAVSDevice where
  hashWithSalt _salt RegisterAVSDevice' {..} =
    _salt
      `Prelude.hashWithSalt` deviceSerialNumber
      `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` userCode
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` amazonId

instance Prelude.NFData RegisterAVSDevice where
  rnf RegisterAVSDevice' {..} =
    Prelude.rnf deviceSerialNumber `Prelude.seq`
      Prelude.rnf roomArn `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf clientId `Prelude.seq`
            Prelude.rnf userCode `Prelude.seq`
              Prelude.rnf productId `Prelude.seq`
                Prelude.rnf amazonId

instance Data.ToHeaders RegisterAVSDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.RegisterAVSDevice" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterAVSDevice where
  toJSON RegisterAVSDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceSerialNumber" Data..=)
              Prelude.<$> deviceSerialNumber,
            ("RoomArn" Data..=) Prelude.<$> roomArn,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ClientId" Data..= clientId),
            Prelude.Just ("UserCode" Data..= userCode),
            Prelude.Just ("ProductId" Data..= productId),
            Prelude.Just ("AmazonId" Data..= amazonId)
          ]
      )

instance Data.ToPath RegisterAVSDevice where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterAVSDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterAVSDeviceResponse' smart constructor.
data RegisterAVSDeviceResponse = RegisterAVSDeviceResponse'
  { -- | The ARN of the device.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterAVSDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'registerAVSDeviceResponse_deviceArn' - The ARN of the device.
--
-- 'httpStatus', 'registerAVSDeviceResponse_httpStatus' - The response's http status code.
newRegisterAVSDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterAVSDeviceResponse
newRegisterAVSDeviceResponse pHttpStatus_ =
  RegisterAVSDeviceResponse'
    { deviceArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the device.
registerAVSDeviceResponse_deviceArn :: Lens.Lens' RegisterAVSDeviceResponse (Prelude.Maybe Prelude.Text)
registerAVSDeviceResponse_deviceArn = Lens.lens (\RegisterAVSDeviceResponse' {deviceArn} -> deviceArn) (\s@RegisterAVSDeviceResponse' {} a -> s {deviceArn = a} :: RegisterAVSDeviceResponse)

-- | The response's http status code.
registerAVSDeviceResponse_httpStatus :: Lens.Lens' RegisterAVSDeviceResponse Prelude.Int
registerAVSDeviceResponse_httpStatus = Lens.lens (\RegisterAVSDeviceResponse' {httpStatus} -> httpStatus) (\s@RegisterAVSDeviceResponse' {} a -> s {httpStatus = a} :: RegisterAVSDeviceResponse)

instance Prelude.NFData RegisterAVSDeviceResponse where
  rnf RegisterAVSDeviceResponse' {..} =
    Prelude.rnf deviceArn `Prelude.seq`
      Prelude.rnf httpStatus
