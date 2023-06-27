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
-- Module      : Amazonka.IoTWireless.CreateServiceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new service profile.
module Amazonka.IoTWireless.CreateServiceProfile
  ( -- * Creating a Request
    CreateServiceProfile (..),
    newCreateServiceProfile,

    -- * Request Lenses
    createServiceProfile_clientRequestToken,
    createServiceProfile_loRaWAN,
    createServiceProfile_name,
    createServiceProfile_tags,

    -- * Destructuring the Response
    CreateServiceProfileResponse (..),
    newCreateServiceProfileResponse,

    -- * Response Lenses
    createServiceProfileResponse_arn,
    createServiceProfileResponse_id,
    createServiceProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateServiceProfile' smart constructor.
data CreateServiceProfile = CreateServiceProfile'
  { -- | Each resource must have a unique client request token. If you try to
    -- create a new resource with the same token as a resource that already
    -- exists, an exception occurs. If you omit this value, AWS SDKs will
    -- automatically generate a unique client request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The service profile information to use to create the service profile.
    loRaWAN :: Prelude.Maybe LoRaWANServiceProfile,
    -- | The name of the new resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags to attach to the new service profile. Tags are metadata that
    -- you can use to manage a resource.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createServiceProfile_clientRequestToken' - Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
--
-- 'loRaWAN', 'createServiceProfile_loRaWAN' - The service profile information to use to create the service profile.
--
-- 'name', 'createServiceProfile_name' - The name of the new resource.
--
-- 'tags', 'createServiceProfile_tags' - The tags to attach to the new service profile. Tags are metadata that
-- you can use to manage a resource.
newCreateServiceProfile ::
  CreateServiceProfile
newCreateServiceProfile =
  CreateServiceProfile'
    { clientRequestToken =
        Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
createServiceProfile_clientRequestToken :: Lens.Lens' CreateServiceProfile (Prelude.Maybe Prelude.Text)
createServiceProfile_clientRequestToken = Lens.lens (\CreateServiceProfile' {clientRequestToken} -> clientRequestToken) (\s@CreateServiceProfile' {} a -> s {clientRequestToken = a} :: CreateServiceProfile)

-- | The service profile information to use to create the service profile.
createServiceProfile_loRaWAN :: Lens.Lens' CreateServiceProfile (Prelude.Maybe LoRaWANServiceProfile)
createServiceProfile_loRaWAN = Lens.lens (\CreateServiceProfile' {loRaWAN} -> loRaWAN) (\s@CreateServiceProfile' {} a -> s {loRaWAN = a} :: CreateServiceProfile)

-- | The name of the new resource.
createServiceProfile_name :: Lens.Lens' CreateServiceProfile (Prelude.Maybe Prelude.Text)
createServiceProfile_name = Lens.lens (\CreateServiceProfile' {name} -> name) (\s@CreateServiceProfile' {} a -> s {name = a} :: CreateServiceProfile)

-- | The tags to attach to the new service profile. Tags are metadata that
-- you can use to manage a resource.
createServiceProfile_tags :: Lens.Lens' CreateServiceProfile (Prelude.Maybe [Tag])
createServiceProfile_tags = Lens.lens (\CreateServiceProfile' {tags} -> tags) (\s@CreateServiceProfile' {} a -> s {tags = a} :: CreateServiceProfile) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateServiceProfile where
  type
    AWSResponse CreateServiceProfile =
      CreateServiceProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceProfileResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateServiceProfile where
  hashWithSalt _salt CreateServiceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateServiceProfile where
  rnf CreateServiceProfile' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders CreateServiceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateServiceProfile where
  toJSON CreateServiceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("LoRaWAN" Data..=) Prelude.<$> loRaWAN,
            ("Name" Data..=) Prelude.<$> name,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateServiceProfile where
  toPath = Prelude.const "/service-profiles"

instance Data.ToQuery CreateServiceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceProfileResponse' smart constructor.
data CreateServiceProfileResponse = CreateServiceProfileResponse'
  { -- | The Amazon Resource Name of the new resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the new service profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createServiceProfileResponse_arn' - The Amazon Resource Name of the new resource.
--
-- 'id', 'createServiceProfileResponse_id' - The ID of the new service profile.
--
-- 'httpStatus', 'createServiceProfileResponse_httpStatus' - The response's http status code.
newCreateServiceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceProfileResponse
newCreateServiceProfileResponse pHttpStatus_ =
  CreateServiceProfileResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the new resource.
createServiceProfileResponse_arn :: Lens.Lens' CreateServiceProfileResponse (Prelude.Maybe Prelude.Text)
createServiceProfileResponse_arn = Lens.lens (\CreateServiceProfileResponse' {arn} -> arn) (\s@CreateServiceProfileResponse' {} a -> s {arn = a} :: CreateServiceProfileResponse)

-- | The ID of the new service profile.
createServiceProfileResponse_id :: Lens.Lens' CreateServiceProfileResponse (Prelude.Maybe Prelude.Text)
createServiceProfileResponse_id = Lens.lens (\CreateServiceProfileResponse' {id} -> id) (\s@CreateServiceProfileResponse' {} a -> s {id = a} :: CreateServiceProfileResponse)

-- | The response's http status code.
createServiceProfileResponse_httpStatus :: Lens.Lens' CreateServiceProfileResponse Prelude.Int
createServiceProfileResponse_httpStatus = Lens.lens (\CreateServiceProfileResponse' {httpStatus} -> httpStatus) (\s@CreateServiceProfileResponse' {} a -> s {httpStatus = a} :: CreateServiceProfileResponse)

instance Prelude.NFData CreateServiceProfileResponse where
  rnf CreateServiceProfileResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
