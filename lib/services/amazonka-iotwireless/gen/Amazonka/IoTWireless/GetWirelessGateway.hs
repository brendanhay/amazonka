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
-- Module      : Amazonka.IoTWireless.GetWirelessGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a wireless gateway.
module Amazonka.IoTWireless.GetWirelessGateway
  ( -- * Creating a Request
    GetWirelessGateway (..),
    newGetWirelessGateway,

    -- * Request Lenses
    getWirelessGateway_identifier,
    getWirelessGateway_identifierType,

    -- * Destructuring the Response
    GetWirelessGatewayResponse (..),
    newGetWirelessGatewayResponse,

    -- * Response Lenses
    getWirelessGatewayResponse_name,
    getWirelessGatewayResponse_thingName,
    getWirelessGatewayResponse_thingArn,
    getWirelessGatewayResponse_loRaWAN,
    getWirelessGatewayResponse_arn,
    getWirelessGatewayResponse_id,
    getWirelessGatewayResponse_description,
    getWirelessGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWirelessGateway' smart constructor.
data GetWirelessGateway = GetWirelessGateway'
  { -- | The identifier of the wireless gateway to get.
    identifier :: Prelude.Text,
    -- | The type of identifier used in @identifier@.
    identifierType :: WirelessGatewayIdType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getWirelessGateway_identifier' - The identifier of the wireless gateway to get.
--
-- 'identifierType', 'getWirelessGateway_identifierType' - The type of identifier used in @identifier@.
newGetWirelessGateway ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'identifierType'
  WirelessGatewayIdType ->
  GetWirelessGateway
newGetWirelessGateway pIdentifier_ pIdentifierType_ =
  GetWirelessGateway'
    { identifier = pIdentifier_,
      identifierType = pIdentifierType_
    }

-- | The identifier of the wireless gateway to get.
getWirelessGateway_identifier :: Lens.Lens' GetWirelessGateway Prelude.Text
getWirelessGateway_identifier = Lens.lens (\GetWirelessGateway' {identifier} -> identifier) (\s@GetWirelessGateway' {} a -> s {identifier = a} :: GetWirelessGateway)

-- | The type of identifier used in @identifier@.
getWirelessGateway_identifierType :: Lens.Lens' GetWirelessGateway WirelessGatewayIdType
getWirelessGateway_identifierType = Lens.lens (\GetWirelessGateway' {identifierType} -> identifierType) (\s@GetWirelessGateway' {} a -> s {identifierType = a} :: GetWirelessGateway)

instance Core.AWSRequest GetWirelessGateway where
  type
    AWSResponse GetWirelessGateway =
      GetWirelessGatewayResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWirelessGatewayResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "ThingName")
            Prelude.<*> (x Core..?> "ThingArn")
            Prelude.<*> (x Core..?> "LoRaWAN")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWirelessGateway where
  hashWithSalt _salt GetWirelessGateway' {..} =
    _salt `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` identifierType

instance Prelude.NFData GetWirelessGateway where
  rnf GetWirelessGateway' {..} =
    Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf identifierType

instance Core.ToHeaders GetWirelessGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetWirelessGateway where
  toPath GetWirelessGateway' {..} =
    Prelude.mconcat
      ["/wireless-gateways/", Core.toBS identifier]

instance Core.ToQuery GetWirelessGateway where
  toQuery GetWirelessGateway' {..} =
    Prelude.mconcat
      ["identifierType" Core.=: identifierType]

-- | /See:/ 'newGetWirelessGatewayResponse' smart constructor.
data GetWirelessGatewayResponse = GetWirelessGatewayResponse'
  { -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing associated with the wireless gateway. The value is
    -- empty if a thing isn\'t associated with the gateway.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the thing associated with the wireless gateway.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the wireless gateway.
    loRaWAN :: Prelude.Maybe LoRaWANGateway,
    -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the wireless gateway.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getWirelessGatewayResponse_name' - The name of the resource.
--
-- 'thingName', 'getWirelessGatewayResponse_thingName' - The name of the thing associated with the wireless gateway. The value is
-- empty if a thing isn\'t associated with the gateway.
--
-- 'thingArn', 'getWirelessGatewayResponse_thingArn' - The ARN of the thing associated with the wireless gateway.
--
-- 'loRaWAN', 'getWirelessGatewayResponse_loRaWAN' - Information about the wireless gateway.
--
-- 'arn', 'getWirelessGatewayResponse_arn' - The Amazon Resource Name of the resource.
--
-- 'id', 'getWirelessGatewayResponse_id' - The ID of the wireless gateway.
--
-- 'description', 'getWirelessGatewayResponse_description' - The description of the resource.
--
-- 'httpStatus', 'getWirelessGatewayResponse_httpStatus' - The response's http status code.
newGetWirelessGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWirelessGatewayResponse
newGetWirelessGatewayResponse pHttpStatus_ =
  GetWirelessGatewayResponse'
    { name = Prelude.Nothing,
      thingName = Prelude.Nothing,
      thingArn = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the resource.
getWirelessGatewayResponse_name :: Lens.Lens' GetWirelessGatewayResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayResponse_name = Lens.lens (\GetWirelessGatewayResponse' {name} -> name) (\s@GetWirelessGatewayResponse' {} a -> s {name = a} :: GetWirelessGatewayResponse)

-- | The name of the thing associated with the wireless gateway. The value is
-- empty if a thing isn\'t associated with the gateway.
getWirelessGatewayResponse_thingName :: Lens.Lens' GetWirelessGatewayResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayResponse_thingName = Lens.lens (\GetWirelessGatewayResponse' {thingName} -> thingName) (\s@GetWirelessGatewayResponse' {} a -> s {thingName = a} :: GetWirelessGatewayResponse)

-- | The ARN of the thing associated with the wireless gateway.
getWirelessGatewayResponse_thingArn :: Lens.Lens' GetWirelessGatewayResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayResponse_thingArn = Lens.lens (\GetWirelessGatewayResponse' {thingArn} -> thingArn) (\s@GetWirelessGatewayResponse' {} a -> s {thingArn = a} :: GetWirelessGatewayResponse)

-- | Information about the wireless gateway.
getWirelessGatewayResponse_loRaWAN :: Lens.Lens' GetWirelessGatewayResponse (Prelude.Maybe LoRaWANGateway)
getWirelessGatewayResponse_loRaWAN = Lens.lens (\GetWirelessGatewayResponse' {loRaWAN} -> loRaWAN) (\s@GetWirelessGatewayResponse' {} a -> s {loRaWAN = a} :: GetWirelessGatewayResponse)

-- | The Amazon Resource Name of the resource.
getWirelessGatewayResponse_arn :: Lens.Lens' GetWirelessGatewayResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayResponse_arn = Lens.lens (\GetWirelessGatewayResponse' {arn} -> arn) (\s@GetWirelessGatewayResponse' {} a -> s {arn = a} :: GetWirelessGatewayResponse)

-- | The ID of the wireless gateway.
getWirelessGatewayResponse_id :: Lens.Lens' GetWirelessGatewayResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayResponse_id = Lens.lens (\GetWirelessGatewayResponse' {id} -> id) (\s@GetWirelessGatewayResponse' {} a -> s {id = a} :: GetWirelessGatewayResponse)

-- | The description of the resource.
getWirelessGatewayResponse_description :: Lens.Lens' GetWirelessGatewayResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayResponse_description = Lens.lens (\GetWirelessGatewayResponse' {description} -> description) (\s@GetWirelessGatewayResponse' {} a -> s {description = a} :: GetWirelessGatewayResponse)

-- | The response's http status code.
getWirelessGatewayResponse_httpStatus :: Lens.Lens' GetWirelessGatewayResponse Prelude.Int
getWirelessGatewayResponse_httpStatus = Lens.lens (\GetWirelessGatewayResponse' {httpStatus} -> httpStatus) (\s@GetWirelessGatewayResponse' {} a -> s {httpStatus = a} :: GetWirelessGatewayResponse)

instance Prelude.NFData GetWirelessGatewayResponse where
  rnf GetWirelessGatewayResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
