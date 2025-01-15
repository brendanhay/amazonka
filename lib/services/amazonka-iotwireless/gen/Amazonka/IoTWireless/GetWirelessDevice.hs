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
-- Module      : Amazonka.IoTWireless.GetWirelessDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a wireless device.
module Amazonka.IoTWireless.GetWirelessDevice
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
    getWirelessDeviceResponse_arn,
    getWirelessDeviceResponse_description,
    getWirelessDeviceResponse_destinationName,
    getWirelessDeviceResponse_id,
    getWirelessDeviceResponse_loRaWAN,
    getWirelessDeviceResponse_name,
    getWirelessDeviceResponse_positioning,
    getWirelessDeviceResponse_sidewalk,
    getWirelessDeviceResponse_thingArn,
    getWirelessDeviceResponse_thingName,
    getWirelessDeviceResponse_type,
    getWirelessDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWirelessDeviceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "DestinationName")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LoRaWAN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Positioning")
            Prelude.<*> (x Data..?> "Sidewalk")
            Prelude.<*> (x Data..?> "ThingArn")
            Prelude.<*> (x Data..?> "ThingName")
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWirelessDevice where
  hashWithSalt _salt GetWirelessDevice' {..} =
    _salt
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` identifierType

instance Prelude.NFData GetWirelessDevice where
  rnf GetWirelessDevice' {..} =
    Prelude.rnf identifier `Prelude.seq`
      Prelude.rnf identifierType

instance Data.ToHeaders GetWirelessDevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetWirelessDevice where
  toPath GetWirelessDevice' {..} =
    Prelude.mconcat
      ["/wireless-devices/", Data.toBS identifier]

instance Data.ToQuery GetWirelessDevice where
  toQuery GetWirelessDevice' {..} =
    Prelude.mconcat
      ["identifierType" Data.=: identifierType]

-- | /See:/ 'newGetWirelessDeviceResponse' smart constructor.
data GetWirelessDeviceResponse = GetWirelessDeviceResponse'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the destination to which the device is assigned.
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the wireless device.
    id :: Prelude.Maybe Prelude.Text,
    -- | Information about the wireless device.
    loRaWAN :: Prelude.Maybe LoRaWANDevice,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | FPort values for the GNSS, stream, and ClockSync functions of the
    -- positioning information.
    positioning :: Prelude.Maybe PositioningConfigStatus,
    -- | Sidewalk device object.
    sidewalk :: Prelude.Maybe SidewalkDevice,
    -- | The ARN of the thing associated with the wireless device.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing associated with the wireless device. The value is
    -- empty if a thing isn\'t associated with the device.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The wireless device type.
    type' :: Prelude.Maybe WirelessDeviceType,
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
-- 'arn', 'getWirelessDeviceResponse_arn' - The Amazon Resource Name of the resource.
--
-- 'description', 'getWirelessDeviceResponse_description' - The description of the resource.
--
-- 'destinationName', 'getWirelessDeviceResponse_destinationName' - The name of the destination to which the device is assigned.
--
-- 'id', 'getWirelessDeviceResponse_id' - The ID of the wireless device.
--
-- 'loRaWAN', 'getWirelessDeviceResponse_loRaWAN' - Information about the wireless device.
--
-- 'name', 'getWirelessDeviceResponse_name' - The name of the resource.
--
-- 'positioning', 'getWirelessDeviceResponse_positioning' - FPort values for the GNSS, stream, and ClockSync functions of the
-- positioning information.
--
-- 'sidewalk', 'getWirelessDeviceResponse_sidewalk' - Sidewalk device object.
--
-- 'thingArn', 'getWirelessDeviceResponse_thingArn' - The ARN of the thing associated with the wireless device.
--
-- 'thingName', 'getWirelessDeviceResponse_thingName' - The name of the thing associated with the wireless device. The value is
-- empty if a thing isn\'t associated with the device.
--
-- 'type'', 'getWirelessDeviceResponse_type' - The wireless device type.
--
-- 'httpStatus', 'getWirelessDeviceResponse_httpStatus' - The response's http status code.
newGetWirelessDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWirelessDeviceResponse
newGetWirelessDeviceResponse pHttpStatus_ =
  GetWirelessDeviceResponse'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      destinationName = Prelude.Nothing,
      id = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      name = Prelude.Nothing,
      positioning = Prelude.Nothing,
      sidewalk = Prelude.Nothing,
      thingArn = Prelude.Nothing,
      thingName = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the resource.
getWirelessDeviceResponse_arn :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_arn = Lens.lens (\GetWirelessDeviceResponse' {arn} -> arn) (\s@GetWirelessDeviceResponse' {} a -> s {arn = a} :: GetWirelessDeviceResponse)

-- | The description of the resource.
getWirelessDeviceResponse_description :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_description = Lens.lens (\GetWirelessDeviceResponse' {description} -> description) (\s@GetWirelessDeviceResponse' {} a -> s {description = a} :: GetWirelessDeviceResponse)

-- | The name of the destination to which the device is assigned.
getWirelessDeviceResponse_destinationName :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_destinationName = Lens.lens (\GetWirelessDeviceResponse' {destinationName} -> destinationName) (\s@GetWirelessDeviceResponse' {} a -> s {destinationName = a} :: GetWirelessDeviceResponse)

-- | The ID of the wireless device.
getWirelessDeviceResponse_id :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_id = Lens.lens (\GetWirelessDeviceResponse' {id} -> id) (\s@GetWirelessDeviceResponse' {} a -> s {id = a} :: GetWirelessDeviceResponse)

-- | Information about the wireless device.
getWirelessDeviceResponse_loRaWAN :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe LoRaWANDevice)
getWirelessDeviceResponse_loRaWAN = Lens.lens (\GetWirelessDeviceResponse' {loRaWAN} -> loRaWAN) (\s@GetWirelessDeviceResponse' {} a -> s {loRaWAN = a} :: GetWirelessDeviceResponse)

-- | The name of the resource.
getWirelessDeviceResponse_name :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_name = Lens.lens (\GetWirelessDeviceResponse' {name} -> name) (\s@GetWirelessDeviceResponse' {} a -> s {name = a} :: GetWirelessDeviceResponse)

-- | FPort values for the GNSS, stream, and ClockSync functions of the
-- positioning information.
getWirelessDeviceResponse_positioning :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe PositioningConfigStatus)
getWirelessDeviceResponse_positioning = Lens.lens (\GetWirelessDeviceResponse' {positioning} -> positioning) (\s@GetWirelessDeviceResponse' {} a -> s {positioning = a} :: GetWirelessDeviceResponse)

-- | Sidewalk device object.
getWirelessDeviceResponse_sidewalk :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe SidewalkDevice)
getWirelessDeviceResponse_sidewalk = Lens.lens (\GetWirelessDeviceResponse' {sidewalk} -> sidewalk) (\s@GetWirelessDeviceResponse' {} a -> s {sidewalk = a} :: GetWirelessDeviceResponse)

-- | The ARN of the thing associated with the wireless device.
getWirelessDeviceResponse_thingArn :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_thingArn = Lens.lens (\GetWirelessDeviceResponse' {thingArn} -> thingArn) (\s@GetWirelessDeviceResponse' {} a -> s {thingArn = a} :: GetWirelessDeviceResponse)

-- | The name of the thing associated with the wireless device. The value is
-- empty if a thing isn\'t associated with the device.
getWirelessDeviceResponse_thingName :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceResponse_thingName = Lens.lens (\GetWirelessDeviceResponse' {thingName} -> thingName) (\s@GetWirelessDeviceResponse' {} a -> s {thingName = a} :: GetWirelessDeviceResponse)

-- | The wireless device type.
getWirelessDeviceResponse_type :: Lens.Lens' GetWirelessDeviceResponse (Prelude.Maybe WirelessDeviceType)
getWirelessDeviceResponse_type = Lens.lens (\GetWirelessDeviceResponse' {type'} -> type') (\s@GetWirelessDeviceResponse' {} a -> s {type' = a} :: GetWirelessDeviceResponse)

-- | The response's http status code.
getWirelessDeviceResponse_httpStatus :: Lens.Lens' GetWirelessDeviceResponse Prelude.Int
getWirelessDeviceResponse_httpStatus = Lens.lens (\GetWirelessDeviceResponse' {httpStatus} -> httpStatus) (\s@GetWirelessDeviceResponse' {} a -> s {httpStatus = a} :: GetWirelessDeviceResponse)

instance Prelude.NFData GetWirelessDeviceResponse where
  rnf GetWirelessDeviceResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf destinationName `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf loRaWAN `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf positioning `Prelude.seq`
                  Prelude.rnf sidewalk `Prelude.seq`
                    Prelude.rnf thingArn `Prelude.seq`
                      Prelude.rnf thingName `Prelude.seq`
                        Prelude.rnf type' `Prelude.seq`
                          Prelude.rnf httpStatus
