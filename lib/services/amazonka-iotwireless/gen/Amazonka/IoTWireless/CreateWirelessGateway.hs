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
-- Module      : Amazonka.IoTWireless.CreateWirelessGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a wireless gateway.
module Amazonka.IoTWireless.CreateWirelessGateway
  ( -- * Creating a Request
    CreateWirelessGateway (..),
    newCreateWirelessGateway,

    -- * Request Lenses
    createWirelessGateway_clientRequestToken,
    createWirelessGateway_description,
    createWirelessGateway_name,
    createWirelessGateway_tags,
    createWirelessGateway_loRaWAN,

    -- * Destructuring the Response
    CreateWirelessGatewayResponse (..),
    newCreateWirelessGatewayResponse,

    -- * Response Lenses
    createWirelessGatewayResponse_arn,
    createWirelessGatewayResponse_id,
    createWirelessGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWirelessGateway' smart constructor.
data CreateWirelessGateway = CreateWirelessGateway'
  { -- | Each resource must have a unique client request token. If you try to
    -- create a new resource with the same token as a resource that already
    -- exists, an exception occurs. If you omit this value, AWS SDKs will
    -- automatically generate a unique client request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the new resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the new resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags to attach to the new wireless gateway. Tags are metadata that
    -- you can use to manage a resource.
    tags :: Prelude.Maybe [Tag],
    -- | The gateway configuration information to use to create the wireless
    -- gateway.
    loRaWAN :: LoRaWANGateway
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWirelessGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createWirelessGateway_clientRequestToken' - Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
--
-- 'description', 'createWirelessGateway_description' - The description of the new resource.
--
-- 'name', 'createWirelessGateway_name' - The name of the new resource.
--
-- 'tags', 'createWirelessGateway_tags' - The tags to attach to the new wireless gateway. Tags are metadata that
-- you can use to manage a resource.
--
-- 'loRaWAN', 'createWirelessGateway_loRaWAN' - The gateway configuration information to use to create the wireless
-- gateway.
newCreateWirelessGateway ::
  -- | 'loRaWAN'
  LoRaWANGateway ->
  CreateWirelessGateway
newCreateWirelessGateway pLoRaWAN_ =
  CreateWirelessGateway'
    { clientRequestToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      loRaWAN = pLoRaWAN_
    }

-- | Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
createWirelessGateway_clientRequestToken :: Lens.Lens' CreateWirelessGateway (Prelude.Maybe Prelude.Text)
createWirelessGateway_clientRequestToken = Lens.lens (\CreateWirelessGateway' {clientRequestToken} -> clientRequestToken) (\s@CreateWirelessGateway' {} a -> s {clientRequestToken = a} :: CreateWirelessGateway)

-- | The description of the new resource.
createWirelessGateway_description :: Lens.Lens' CreateWirelessGateway (Prelude.Maybe Prelude.Text)
createWirelessGateway_description = Lens.lens (\CreateWirelessGateway' {description} -> description) (\s@CreateWirelessGateway' {} a -> s {description = a} :: CreateWirelessGateway)

-- | The name of the new resource.
createWirelessGateway_name :: Lens.Lens' CreateWirelessGateway (Prelude.Maybe Prelude.Text)
createWirelessGateway_name = Lens.lens (\CreateWirelessGateway' {name} -> name) (\s@CreateWirelessGateway' {} a -> s {name = a} :: CreateWirelessGateway)

-- | The tags to attach to the new wireless gateway. Tags are metadata that
-- you can use to manage a resource.
createWirelessGateway_tags :: Lens.Lens' CreateWirelessGateway (Prelude.Maybe [Tag])
createWirelessGateway_tags = Lens.lens (\CreateWirelessGateway' {tags} -> tags) (\s@CreateWirelessGateway' {} a -> s {tags = a} :: CreateWirelessGateway) Prelude.. Lens.mapping Lens.coerced

-- | The gateway configuration information to use to create the wireless
-- gateway.
createWirelessGateway_loRaWAN :: Lens.Lens' CreateWirelessGateway LoRaWANGateway
createWirelessGateway_loRaWAN = Lens.lens (\CreateWirelessGateway' {loRaWAN} -> loRaWAN) (\s@CreateWirelessGateway' {} a -> s {loRaWAN = a} :: CreateWirelessGateway)

instance Core.AWSRequest CreateWirelessGateway where
  type
    AWSResponse CreateWirelessGateway =
      CreateWirelessGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWirelessGatewayResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWirelessGateway where
  hashWithSalt _salt CreateWirelessGateway' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` loRaWAN

instance Prelude.NFData CreateWirelessGateway where
  rnf CreateWirelessGateway' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf loRaWAN

instance Data.ToHeaders CreateWirelessGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateWirelessGateway where
  toJSON CreateWirelessGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("LoRaWAN" Data..= loRaWAN)
          ]
      )

instance Data.ToPath CreateWirelessGateway where
  toPath = Prelude.const "/wireless-gateways"

instance Data.ToQuery CreateWirelessGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWirelessGatewayResponse' smart constructor.
data CreateWirelessGatewayResponse = CreateWirelessGatewayResponse'
  { -- | The Amazon Resource Name of the new resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the new wireless gateway.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWirelessGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createWirelessGatewayResponse_arn' - The Amazon Resource Name of the new resource.
--
-- 'id', 'createWirelessGatewayResponse_id' - The ID of the new wireless gateway.
--
-- 'httpStatus', 'createWirelessGatewayResponse_httpStatus' - The response's http status code.
newCreateWirelessGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWirelessGatewayResponse
newCreateWirelessGatewayResponse pHttpStatus_ =
  CreateWirelessGatewayResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the new resource.
createWirelessGatewayResponse_arn :: Lens.Lens' CreateWirelessGatewayResponse (Prelude.Maybe Prelude.Text)
createWirelessGatewayResponse_arn = Lens.lens (\CreateWirelessGatewayResponse' {arn} -> arn) (\s@CreateWirelessGatewayResponse' {} a -> s {arn = a} :: CreateWirelessGatewayResponse)

-- | The ID of the new wireless gateway.
createWirelessGatewayResponse_id :: Lens.Lens' CreateWirelessGatewayResponse (Prelude.Maybe Prelude.Text)
createWirelessGatewayResponse_id = Lens.lens (\CreateWirelessGatewayResponse' {id} -> id) (\s@CreateWirelessGatewayResponse' {} a -> s {id = a} :: CreateWirelessGatewayResponse)

-- | The response's http status code.
createWirelessGatewayResponse_httpStatus :: Lens.Lens' CreateWirelessGatewayResponse Prelude.Int
createWirelessGatewayResponse_httpStatus = Lens.lens (\CreateWirelessGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateWirelessGatewayResponse' {} a -> s {httpStatus = a} :: CreateWirelessGatewayResponse)

instance Prelude.NFData CreateWirelessGatewayResponse where
  rnf CreateWirelessGatewayResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
