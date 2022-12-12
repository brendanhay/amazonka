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
-- Module      : Amazonka.IoTWireless.GetServiceProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a service profile.
module Amazonka.IoTWireless.GetServiceProfile
  ( -- * Creating a Request
    GetServiceProfile (..),
    newGetServiceProfile,

    -- * Request Lenses
    getServiceProfile_id,

    -- * Destructuring the Response
    GetServiceProfileResponse (..),
    newGetServiceProfileResponse,

    -- * Response Lenses
    getServiceProfileResponse_arn,
    getServiceProfileResponse_id,
    getServiceProfileResponse_loRaWAN,
    getServiceProfileResponse_name,
    getServiceProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceProfile' smart constructor.
data GetServiceProfile = GetServiceProfile'
  { -- | The ID of the resource to get.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getServiceProfile_id' - The ID of the resource to get.
newGetServiceProfile ::
  -- | 'id'
  Prelude.Text ->
  GetServiceProfile
newGetServiceProfile pId_ =
  GetServiceProfile' {id = pId_}

-- | The ID of the resource to get.
getServiceProfile_id :: Lens.Lens' GetServiceProfile Prelude.Text
getServiceProfile_id = Lens.lens (\GetServiceProfile' {id} -> id) (\s@GetServiceProfile' {} a -> s {id = a} :: GetServiceProfile)

instance Core.AWSRequest GetServiceProfile where
  type
    AWSResponse GetServiceProfile =
      GetServiceProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceProfileResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LoRaWAN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceProfile where
  hashWithSalt _salt GetServiceProfile' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetServiceProfile where
  rnf GetServiceProfile' {..} = Prelude.rnf id

instance Data.ToHeaders GetServiceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetServiceProfile where
  toPath GetServiceProfile' {..} =
    Prelude.mconcat
      ["/service-profiles/", Data.toBS id]

instance Data.ToQuery GetServiceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceProfileResponse' smart constructor.
data GetServiceProfileResponse = GetServiceProfileResponse'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | Information about the service profile.
    loRaWAN :: Prelude.Maybe LoRaWANGetServiceProfileInfo,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getServiceProfileResponse_arn' - The Amazon Resource Name of the resource.
--
-- 'id', 'getServiceProfileResponse_id' - The ID of the service profile.
--
-- 'loRaWAN', 'getServiceProfileResponse_loRaWAN' - Information about the service profile.
--
-- 'name', 'getServiceProfileResponse_name' - The name of the resource.
--
-- 'httpStatus', 'getServiceProfileResponse_httpStatus' - The response's http status code.
newGetServiceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceProfileResponse
newGetServiceProfileResponse pHttpStatus_ =
  GetServiceProfileResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the resource.
getServiceProfileResponse_arn :: Lens.Lens' GetServiceProfileResponse (Prelude.Maybe Prelude.Text)
getServiceProfileResponse_arn = Lens.lens (\GetServiceProfileResponse' {arn} -> arn) (\s@GetServiceProfileResponse' {} a -> s {arn = a} :: GetServiceProfileResponse)

-- | The ID of the service profile.
getServiceProfileResponse_id :: Lens.Lens' GetServiceProfileResponse (Prelude.Maybe Prelude.Text)
getServiceProfileResponse_id = Lens.lens (\GetServiceProfileResponse' {id} -> id) (\s@GetServiceProfileResponse' {} a -> s {id = a} :: GetServiceProfileResponse)

-- | Information about the service profile.
getServiceProfileResponse_loRaWAN :: Lens.Lens' GetServiceProfileResponse (Prelude.Maybe LoRaWANGetServiceProfileInfo)
getServiceProfileResponse_loRaWAN = Lens.lens (\GetServiceProfileResponse' {loRaWAN} -> loRaWAN) (\s@GetServiceProfileResponse' {} a -> s {loRaWAN = a} :: GetServiceProfileResponse)

-- | The name of the resource.
getServiceProfileResponse_name :: Lens.Lens' GetServiceProfileResponse (Prelude.Maybe Prelude.Text)
getServiceProfileResponse_name = Lens.lens (\GetServiceProfileResponse' {name} -> name) (\s@GetServiceProfileResponse' {} a -> s {name = a} :: GetServiceProfileResponse)

-- | The response's http status code.
getServiceProfileResponse_httpStatus :: Lens.Lens' GetServiceProfileResponse Prelude.Int
getServiceProfileResponse_httpStatus = Lens.lens (\GetServiceProfileResponse' {httpStatus} -> httpStatus) (\s@GetServiceProfileResponse' {} a -> s {httpStatus = a} :: GetServiceProfileResponse)

instance Prelude.NFData GetServiceProfileResponse where
  rnf GetServiceProfileResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
