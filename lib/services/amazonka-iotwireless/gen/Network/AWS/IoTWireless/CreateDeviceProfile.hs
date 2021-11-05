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
-- Module      : Network.AWS.IoTWireless.CreateDeviceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new device profile.
module Network.AWS.IoTWireless.CreateDeviceProfile
  ( -- * Creating a Request
    CreateDeviceProfile (..),
    newCreateDeviceProfile,

    -- * Request Lenses
    createDeviceProfile_loRaWAN,
    createDeviceProfile_name,
    createDeviceProfile_clientRequestToken,
    createDeviceProfile_tags,

    -- * Destructuring the Response
    CreateDeviceProfileResponse (..),
    newCreateDeviceProfileResponse,

    -- * Response Lenses
    createDeviceProfileResponse_arn,
    createDeviceProfileResponse_id,
    createDeviceProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTWireless.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDeviceProfile' smart constructor.
data CreateDeviceProfile = CreateDeviceProfile'
  { -- | The device profile information to use to create the device profile.
    loRaWAN :: Prelude.Maybe LoRaWANDeviceProfile,
    -- | The name of the new resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | Each resource must have a unique client request token. If you try to
    -- create a new resource with the same token as a resource that already
    -- exists, an exception occurs. If you omit this value, AWS SDKs will
    -- automatically generate a unique client request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The tags to attach to the new device profile. Tags are metadata that you
    -- can use to manage a resource.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeviceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loRaWAN', 'createDeviceProfile_loRaWAN' - The device profile information to use to create the device profile.
--
-- 'name', 'createDeviceProfile_name' - The name of the new resource.
--
-- 'clientRequestToken', 'createDeviceProfile_clientRequestToken' - Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
--
-- 'tags', 'createDeviceProfile_tags' - The tags to attach to the new device profile. Tags are metadata that you
-- can use to manage a resource.
newCreateDeviceProfile ::
  CreateDeviceProfile
newCreateDeviceProfile =
  CreateDeviceProfile'
    { loRaWAN = Prelude.Nothing,
      name = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The device profile information to use to create the device profile.
createDeviceProfile_loRaWAN :: Lens.Lens' CreateDeviceProfile (Prelude.Maybe LoRaWANDeviceProfile)
createDeviceProfile_loRaWAN = Lens.lens (\CreateDeviceProfile' {loRaWAN} -> loRaWAN) (\s@CreateDeviceProfile' {} a -> s {loRaWAN = a} :: CreateDeviceProfile)

-- | The name of the new resource.
createDeviceProfile_name :: Lens.Lens' CreateDeviceProfile (Prelude.Maybe Prelude.Text)
createDeviceProfile_name = Lens.lens (\CreateDeviceProfile' {name} -> name) (\s@CreateDeviceProfile' {} a -> s {name = a} :: CreateDeviceProfile)

-- | Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
createDeviceProfile_clientRequestToken :: Lens.Lens' CreateDeviceProfile (Prelude.Maybe Prelude.Text)
createDeviceProfile_clientRequestToken = Lens.lens (\CreateDeviceProfile' {clientRequestToken} -> clientRequestToken) (\s@CreateDeviceProfile' {} a -> s {clientRequestToken = a} :: CreateDeviceProfile)

-- | The tags to attach to the new device profile. Tags are metadata that you
-- can use to manage a resource.
createDeviceProfile_tags :: Lens.Lens' CreateDeviceProfile (Prelude.Maybe [Tag])
createDeviceProfile_tags = Lens.lens (\CreateDeviceProfile' {tags} -> tags) (\s@CreateDeviceProfile' {} a -> s {tags = a} :: CreateDeviceProfile) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateDeviceProfile where
  type
    AWSResponse CreateDeviceProfile =
      CreateDeviceProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeviceProfileResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeviceProfile

instance Prelude.NFData CreateDeviceProfile

instance Core.ToHeaders CreateDeviceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateDeviceProfile where
  toJSON CreateDeviceProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LoRaWAN" Core..=) Prelude.<$> loRaWAN,
            ("Name" Core..=) Prelude.<$> name,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("Tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateDeviceProfile where
  toPath = Prelude.const "/device-profiles"

instance Core.ToQuery CreateDeviceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeviceProfileResponse' smart constructor.
data CreateDeviceProfileResponse = CreateDeviceProfileResponse'
  { -- | The Amazon Resource Name of the new resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the new device profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeviceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createDeviceProfileResponse_arn' - The Amazon Resource Name of the new resource.
--
-- 'id', 'createDeviceProfileResponse_id' - The ID of the new device profile.
--
-- 'httpStatus', 'createDeviceProfileResponse_httpStatus' - The response's http status code.
newCreateDeviceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeviceProfileResponse
newCreateDeviceProfileResponse pHttpStatus_ =
  CreateDeviceProfileResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the new resource.
createDeviceProfileResponse_arn :: Lens.Lens' CreateDeviceProfileResponse (Prelude.Maybe Prelude.Text)
createDeviceProfileResponse_arn = Lens.lens (\CreateDeviceProfileResponse' {arn} -> arn) (\s@CreateDeviceProfileResponse' {} a -> s {arn = a} :: CreateDeviceProfileResponse)

-- | The ID of the new device profile.
createDeviceProfileResponse_id :: Lens.Lens' CreateDeviceProfileResponse (Prelude.Maybe Prelude.Text)
createDeviceProfileResponse_id = Lens.lens (\CreateDeviceProfileResponse' {id} -> id) (\s@CreateDeviceProfileResponse' {} a -> s {id = a} :: CreateDeviceProfileResponse)

-- | The response's http status code.
createDeviceProfileResponse_httpStatus :: Lens.Lens' CreateDeviceProfileResponse Prelude.Int
createDeviceProfileResponse_httpStatus = Lens.lens (\CreateDeviceProfileResponse' {httpStatus} -> httpStatus) (\s@CreateDeviceProfileResponse' {} a -> s {httpStatus = a} :: CreateDeviceProfileResponse)

instance Prelude.NFData CreateDeviceProfileResponse
