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
-- Module      : Amazonka.IoTWireless.CreateWirelessDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a wireless device.
module Amazonka.IoTWireless.CreateWirelessDevice
  ( -- * Creating a Request
    CreateWirelessDevice (..),
    newCreateWirelessDevice,

    -- * Request Lenses
    createWirelessDevice_tags,
    createWirelessDevice_name,
    createWirelessDevice_clientRequestToken,
    createWirelessDevice_loRaWAN,
    createWirelessDevice_description,
    createWirelessDevice_type,
    createWirelessDevice_destinationName,

    -- * Destructuring the Response
    CreateWirelessDeviceResponse (..),
    newCreateWirelessDeviceResponse,

    -- * Response Lenses
    createWirelessDeviceResponse_arn,
    createWirelessDeviceResponse_id,
    createWirelessDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWirelessDevice' smart constructor.
data CreateWirelessDevice = CreateWirelessDevice'
  { -- | The tags to attach to the new wireless device. Tags are metadata that
    -- you can use to manage a resource.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the new resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | Each resource must have a unique client request token. If you try to
    -- create a new resource with the same token as a resource that already
    -- exists, an exception occurs. If you omit this value, AWS SDKs will
    -- automatically generate a unique client request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The device configuration information to use to create the wireless
    -- device.
    loRaWAN :: Prelude.Maybe LoRaWANDevice,
    -- | The description of the new resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The wireless device type.
    type' :: WirelessDeviceType,
    -- | The name of the destination to assign to the new wireless device.
    destinationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWirelessDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWirelessDevice_tags' - The tags to attach to the new wireless device. Tags are metadata that
-- you can use to manage a resource.
--
-- 'name', 'createWirelessDevice_name' - The name of the new resource.
--
-- 'clientRequestToken', 'createWirelessDevice_clientRequestToken' - Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
--
-- 'loRaWAN', 'createWirelessDevice_loRaWAN' - The device configuration information to use to create the wireless
-- device.
--
-- 'description', 'createWirelessDevice_description' - The description of the new resource.
--
-- 'type'', 'createWirelessDevice_type' - The wireless device type.
--
-- 'destinationName', 'createWirelessDevice_destinationName' - The name of the destination to assign to the new wireless device.
newCreateWirelessDevice ::
  -- | 'type''
  WirelessDeviceType ->
  -- | 'destinationName'
  Prelude.Text ->
  CreateWirelessDevice
newCreateWirelessDevice pType_ pDestinationName_ =
  CreateWirelessDevice'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = pType_,
      destinationName = pDestinationName_
    }

-- | The tags to attach to the new wireless device. Tags are metadata that
-- you can use to manage a resource.
createWirelessDevice_tags :: Lens.Lens' CreateWirelessDevice (Prelude.Maybe [Tag])
createWirelessDevice_tags = Lens.lens (\CreateWirelessDevice' {tags} -> tags) (\s@CreateWirelessDevice' {} a -> s {tags = a} :: CreateWirelessDevice) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new resource.
createWirelessDevice_name :: Lens.Lens' CreateWirelessDevice (Prelude.Maybe Prelude.Text)
createWirelessDevice_name = Lens.lens (\CreateWirelessDevice' {name} -> name) (\s@CreateWirelessDevice' {} a -> s {name = a} :: CreateWirelessDevice)

-- | Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
createWirelessDevice_clientRequestToken :: Lens.Lens' CreateWirelessDevice (Prelude.Maybe Prelude.Text)
createWirelessDevice_clientRequestToken = Lens.lens (\CreateWirelessDevice' {clientRequestToken} -> clientRequestToken) (\s@CreateWirelessDevice' {} a -> s {clientRequestToken = a} :: CreateWirelessDevice)

-- | The device configuration information to use to create the wireless
-- device.
createWirelessDevice_loRaWAN :: Lens.Lens' CreateWirelessDevice (Prelude.Maybe LoRaWANDevice)
createWirelessDevice_loRaWAN = Lens.lens (\CreateWirelessDevice' {loRaWAN} -> loRaWAN) (\s@CreateWirelessDevice' {} a -> s {loRaWAN = a} :: CreateWirelessDevice)

-- | The description of the new resource.
createWirelessDevice_description :: Lens.Lens' CreateWirelessDevice (Prelude.Maybe Prelude.Text)
createWirelessDevice_description = Lens.lens (\CreateWirelessDevice' {description} -> description) (\s@CreateWirelessDevice' {} a -> s {description = a} :: CreateWirelessDevice)

-- | The wireless device type.
createWirelessDevice_type :: Lens.Lens' CreateWirelessDevice WirelessDeviceType
createWirelessDevice_type = Lens.lens (\CreateWirelessDevice' {type'} -> type') (\s@CreateWirelessDevice' {} a -> s {type' = a} :: CreateWirelessDevice)

-- | The name of the destination to assign to the new wireless device.
createWirelessDevice_destinationName :: Lens.Lens' CreateWirelessDevice Prelude.Text
createWirelessDevice_destinationName = Lens.lens (\CreateWirelessDevice' {destinationName} -> destinationName) (\s@CreateWirelessDevice' {} a -> s {destinationName = a} :: CreateWirelessDevice)

instance Core.AWSRequest CreateWirelessDevice where
  type
    AWSResponse CreateWirelessDevice =
      CreateWirelessDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWirelessDeviceResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWirelessDevice where
  hashWithSalt _salt CreateWirelessDevice' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` destinationName

instance Prelude.NFData CreateWirelessDevice where
  rnf CreateWirelessDevice' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf destinationName

instance Core.ToHeaders CreateWirelessDevice where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateWirelessDevice where
  toJSON CreateWirelessDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Name" Core..=) Prelude.<$> name,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("LoRaWAN" Core..=) Prelude.<$> loRaWAN,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Type" Core..= type'),
            Prelude.Just
              ("DestinationName" Core..= destinationName)
          ]
      )

instance Core.ToPath CreateWirelessDevice where
  toPath = Prelude.const "/wireless-devices"

instance Core.ToQuery CreateWirelessDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWirelessDeviceResponse' smart constructor.
data CreateWirelessDeviceResponse = CreateWirelessDeviceResponse'
  { -- | The Amazon Resource Name of the new resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the new wireless device.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWirelessDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createWirelessDeviceResponse_arn' - The Amazon Resource Name of the new resource.
--
-- 'id', 'createWirelessDeviceResponse_id' - The ID of the new wireless device.
--
-- 'httpStatus', 'createWirelessDeviceResponse_httpStatus' - The response's http status code.
newCreateWirelessDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWirelessDeviceResponse
newCreateWirelessDeviceResponse pHttpStatus_ =
  CreateWirelessDeviceResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the new resource.
createWirelessDeviceResponse_arn :: Lens.Lens' CreateWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
createWirelessDeviceResponse_arn = Lens.lens (\CreateWirelessDeviceResponse' {arn} -> arn) (\s@CreateWirelessDeviceResponse' {} a -> s {arn = a} :: CreateWirelessDeviceResponse)

-- | The ID of the new wireless device.
createWirelessDeviceResponse_id :: Lens.Lens' CreateWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
createWirelessDeviceResponse_id = Lens.lens (\CreateWirelessDeviceResponse' {id} -> id) (\s@CreateWirelessDeviceResponse' {} a -> s {id = a} :: CreateWirelessDeviceResponse)

-- | The response's http status code.
createWirelessDeviceResponse_httpStatus :: Lens.Lens' CreateWirelessDeviceResponse Prelude.Int
createWirelessDeviceResponse_httpStatus = Lens.lens (\CreateWirelessDeviceResponse' {httpStatus} -> httpStatus) (\s@CreateWirelessDeviceResponse' {} a -> s {httpStatus = a} :: CreateWirelessDeviceResponse)

instance Prelude.NFData CreateWirelessDeviceResponse where
  rnf CreateWirelessDeviceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
