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
-- Module      : Amazonka.Lightsail.GetContainerServicePowers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of powers that can be specified for your Amazon
-- Lightsail container services.
--
-- The power specifies the amount of memory, the number of vCPUs, and the
-- base price of the container service.
module Amazonka.Lightsail.GetContainerServicePowers
  ( -- * Creating a Request
    GetContainerServicePowers (..),
    newGetContainerServicePowers,

    -- * Destructuring the Response
    GetContainerServicePowersResponse (..),
    newGetContainerServicePowersResponse,

    -- * Response Lenses
    getContainerServicePowersResponse_powers,
    getContainerServicePowersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContainerServicePowers' smart constructor.
data GetContainerServicePowers = GetContainerServicePowers'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerServicePowers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetContainerServicePowers ::
  GetContainerServicePowers
newGetContainerServicePowers =
  GetContainerServicePowers'

instance Core.AWSRequest GetContainerServicePowers where
  type
    AWSResponse GetContainerServicePowers =
      GetContainerServicePowersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerServicePowersResponse'
            Prelude.<$> (x Data..?> "powers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContainerServicePowers where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetContainerServicePowers where
  rnf _ = ()

instance Data.ToHeaders GetContainerServicePowers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetContainerServicePowers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetContainerServicePowers where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetContainerServicePowers where
  toPath = Prelude.const "/"

instance Data.ToQuery GetContainerServicePowers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContainerServicePowersResponse' smart constructor.
data GetContainerServicePowersResponse = GetContainerServicePowersResponse'
  { -- | An array of objects that describe the powers that can be specified for a
    -- container service.
    powers :: Prelude.Maybe [ContainerServicePower],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerServicePowersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'powers', 'getContainerServicePowersResponse_powers' - An array of objects that describe the powers that can be specified for a
-- container service.
--
-- 'httpStatus', 'getContainerServicePowersResponse_httpStatus' - The response's http status code.
newGetContainerServicePowersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContainerServicePowersResponse
newGetContainerServicePowersResponse pHttpStatus_ =
  GetContainerServicePowersResponse'
    { powers =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the powers that can be specified for a
-- container service.
getContainerServicePowersResponse_powers :: Lens.Lens' GetContainerServicePowersResponse (Prelude.Maybe [ContainerServicePower])
getContainerServicePowersResponse_powers = Lens.lens (\GetContainerServicePowersResponse' {powers} -> powers) (\s@GetContainerServicePowersResponse' {} a -> s {powers = a} :: GetContainerServicePowersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getContainerServicePowersResponse_httpStatus :: Lens.Lens' GetContainerServicePowersResponse Prelude.Int
getContainerServicePowersResponse_httpStatus = Lens.lens (\GetContainerServicePowersResponse' {httpStatus} -> httpStatus) (\s@GetContainerServicePowersResponse' {} a -> s {httpStatus = a} :: GetContainerServicePowersResponse)

instance
  Prelude.NFData
    GetContainerServicePowersResponse
  where
  rnf GetContainerServicePowersResponse' {..} =
    Prelude.rnf powers
      `Prelude.seq` Prelude.rnf httpStatus
