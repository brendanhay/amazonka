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
-- Module      : Network.AWS.IoTWireless.GetWirelessDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a wireless device.
module Network.AWS.IoTWireless.GetWirelessDevice
  ( -- * Creating a Request
    GetWirelessDevice (..),
    newGetWirelessDevice,

    -- * Request Lenses
    getWirelessDevice_identifier,
    getWirelessDevice_identifierType,

    -- * Destructuring the Response
    GetWirelessDeviceResponse (..),
    newGetWirelessDeviceResponse,

    -- * Response Lenses
    getWirelessDeviceResponse_sidewalk,
    getWirelessDeviceResponse_arn,
    getWirelessDeviceResponse_loRaWAN,
    getWirelessDeviceResponse_thingArn,
    getWirelessDeviceResponse_name,
    getWirelessDeviceResponse_id,
    getWirelessDeviceResponse_type,
    getWirelessDeviceResponse_destinationName,
    getWirelessDeviceResponse_thingName,
    getWirelessDeviceResponse_description,
    getWirelessDeviceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTWireless.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetWirelessDevice' smart constructor.
data GetWirelessDevice = GetWirelessDevice'
  { -- | The identifier of the wireless device to get.
    identifier :: Prelude.Text,
    -- | The type of identifier used in @identifier@.
    identifierType :: WirelessDeviceIdType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getWirelessDevice_identifier' - The identifier of the wireless device to get.
--
-- 'identifierType', 'getWirelessDevice_identifierType' - The type of identifier used in @identifier@.
newGetWirelessDevice ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'identifierType'
  WirelessDeviceIdType ->
  GetWirelessDevice
newGetWirelessDevice pIdentifier_ pIdentifierType_ =
  GetWirelessDevice'
    { identifier = pIdentifier_,
      identifierType = pIdentifierType_
    }

-- | The identifier of the wireless device to get.
getWirelessDevice_identifier :: Lens.Lens' GetWirelessDevice Prelude.Text
getWirelessDevice_identifier = Lens.lens (\GetWirelessDevice' {identifier} -> identifier) (\s@GetWirelessDevice' {} a -> s {identifier = a} :: GetWirelessDevice)

-- | The type of identifier used in @identifier@.
getWirelessDevice_identifierType :: Lens.Lens' GetWirelessDevice WirelessDeviceIdType
getWirelessDevice_identifierType = Lens.lens (\GetWirelessDevice' {identifierType} -> identifierType) (\s@GetWirelessDevice' {} a -> s {identifierType = a} :: GetWirelessDevice)

instance Core.AWSRequest GetWirelessDevice where
  type
    AWSResponse GetWirelessDevice =
      GetWirelessDeviceResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWirelessDeviceResponse'
            Prelude.<$> (x Core..?> "Sidewalk")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "LoRaWAN")
            Prelude.<*> (x Core..?> "ThingArn")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Type")
            Prelude.<*> (x Core..?> "DestinationName")
            Prelude.<*> (x Core..?> "ThingName")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWirelessDevice

instance Prelude.NFData GetWirelessDevice

instance Core.ToHeaders GetWirelessDevice where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetWirelessDevice where
  toPath GetWirelessDevice' {..} =
    Prelude.mconcat
      ["/wireless-devices/", Core.toBS identifier]

instance Core.ToQuery GetWirelessDevice where
  toQuery GetWirelessDevice' {..} =
    Prelude.mconcat
      ["identifierType" Core.=: identifierType]

-- | /See:/ 'newGetWirelessDeviceResponse' smart constructor.
data GetWirelessDeviceResponse = GetWirelessDeviceResponse'
  { -- | Sidewalk device object.
    sidewalk :: Prelude.Maybe SidewalkDevice,
    -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Information about the wireless device.
    loRaWAN :: Prelude.Maybe LoRaWANDevice,
    -- | The ARN of the thing associated with the wireless device.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the wireless device.
    id :: Prelude.Maybe Prelude.Text,
    -- | The wireless device type.
    type' :: Prelude.Maybe WirelessDeviceType,
    -- | The name of the destination to which the device is assigned.
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing associated with the wireless device. The value is
    -- empty if a thing isn\'t associated with the device.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalk', 'getWirelessDeviceResponse_sidewalk' - Sidewalk device object.
--
-- 'arn', 'getWirelessDeviceResponse_arn' - The Amazon Resource Name of the resource.
--
-- 'loRaWAN', 'getWirelessDeviceResponse_loRaWAN' - Information about the wireless device.
--
-- 'thingArn', 'getWirelessDeviceResponse_thingArn' - The ARN of the thing associated with the wireless device.
--
-- 'name', 'getWirelessDeviceResponse_name' - The name of the resource.
--
-- 'id', 'getWirelessDeviceResponse_id' - The ID of the wireless device.
--
-- 'type'', 'getWirelessDeviceResponse_type' - The wireless device type.
--
-- 'destinationName', 'getWirelessDeviceResponse_destinationName' - The name of the destination to which the device is assigned.
--
-- 'thingName', 'getWirelessDeviceResponse_thingName' - The name of the thing associated with the wireless device. The value is
-- empty if a thing isn\'t associated with the device.
--
-- 'description', 'getWirelessDeviceResponse_description' - The description of the resource.
--
-- 'httpStatus', 'getWirelessDeviceResponse_httpStatus' - The response's http status code.
newGetWirelessDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWirelessDeviceResponse
newGetWirelessDeviceResponse pHttpStatus_ =
  GetWirelessDeviceResponse'
    { sidewalk =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      thingArn = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing,
      destinationName = Prelude.Nothing,
      thingName = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Sidewalk device object.
getWirelessDeviceResponse_sidewalk :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe SidewalkDevice)
getWirelessDeviceResponse_sidewalk = Lens.lens (\GetWirelessDeviceResponse' {sidewalk} -> sidewalk) (\s@GetWirelessDeviceResponse' {} a -> s {sidewalk = a} :: GetWirelessDeviceResponse)

-- | The Amazon Resource Name of the resource.
getWirelessDeviceResponse_arn :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_arn = Lens.lens (\GetWirelessDeviceResponse' {arn} -> arn) (\s@GetWirelessDeviceResponse' {} a -> s {arn = a} :: GetWirelessDeviceResponse)

-- | Information about the wireless device.
getWirelessDeviceResponse_loRaWAN :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe LoRaWANDevice)
getWirelessDeviceResponse_loRaWAN = Lens.lens (\GetWirelessDeviceResponse' {loRaWAN} -> loRaWAN) (\s@GetWirelessDeviceResponse' {} a -> s {loRaWAN = a} :: GetWirelessDeviceResponse)

-- | The ARN of the thing associated with the wireless device.
getWirelessDeviceResponse_thingArn :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_thingArn = Lens.lens (\GetWirelessDeviceResponse' {thingArn} -> thingArn) (\s@GetWirelessDeviceResponse' {} a -> s {thingArn = a} :: GetWirelessDeviceResponse)

-- | The name of the resource.
getWirelessDeviceResponse_name :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_name = Lens.lens (\GetWirelessDeviceResponse' {name} -> name) (\s@GetWirelessDeviceResponse' {} a -> s {name = a} :: GetWirelessDeviceResponse)

-- | The ID of the wireless device.
getWirelessDeviceResponse_id :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_id = Lens.lens (\GetWirelessDeviceResponse' {id} -> id) (\s@GetWirelessDeviceResponse' {} a -> s {id = a} :: GetWirelessDeviceResponse)

-- | The wireless device type.
getWirelessDeviceResponse_type :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe WirelessDeviceType)
getWirelessDeviceResponse_type = Lens.lens (\GetWirelessDeviceResponse' {type'} -> type') (\s@GetWirelessDeviceResponse' {} a -> s {type' = a} :: GetWirelessDeviceResponse)

-- | The name of the destination to which the device is assigned.
getWirelessDeviceResponse_destinationName :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_destinationName = Lens.lens (\GetWirelessDeviceResponse' {destinationName} -> destinationName) (\s@GetWirelessDeviceResponse' {} a -> s {destinationName = a} :: GetWirelessDeviceResponse)

-- | The name of the thing associated with the wireless device. The value is
-- empty if a thing isn\'t associated with the device.
getWirelessDeviceResponse_thingName :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_thingName = Lens.lens (\GetWirelessDeviceResponse' {thingName} -> thingName) (\s@GetWirelessDeviceResponse' {} a -> s {thingName = a} :: GetWirelessDeviceResponse)

-- | The description of the resource.
getWirelessDeviceResponse_description :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_description = Lens.lens (\GetWirelessDeviceResponse' {description} -> description) (\s@GetWirelessDeviceResponse' {} a -> s {description = a} :: GetWirelessDeviceResponse)

-- | The response's http status code.
getWirelessDeviceResponse_httpStatus :: Lens.Lens' GetWirelessDeviceResponse Prelude.Int
getWirelessDeviceResponse_httpStatus = Lens.lens (\GetWirelessDeviceResponse' {httpStatus} -> httpStatus) (\s@GetWirelessDeviceResponse' {} a -> s {httpStatus = a} :: GetWirelessDeviceResponse)

instance Prelude.NFData GetWirelessDeviceResponse
