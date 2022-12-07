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
-- Module      : Amazonka.IoT.GetRegistrationCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a registration code used to register a CA certificate with IoT.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetRegistrationCode>
-- action.
module Amazonka.IoT.GetRegistrationCode
  ( -- * Creating a Request
    GetRegistrationCode (..),
    newGetRegistrationCode,

    -- * Destructuring the Response
    GetRegistrationCodeResponse (..),
    newGetRegistrationCodeResponse,

    -- * Response Lenses
    getRegistrationCodeResponse_registrationCode,
    getRegistrationCodeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input to the GetRegistrationCode operation.
--
-- /See:/ 'newGetRegistrationCode' smart constructor.
data GetRegistrationCode = GetRegistrationCode'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegistrationCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetRegistrationCode ::
  GetRegistrationCode
newGetRegistrationCode = GetRegistrationCode'

instance Core.AWSRequest GetRegistrationCode where
  type
    AWSResponse GetRegistrationCode =
      GetRegistrationCodeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegistrationCodeResponse'
            Prelude.<$> (x Data..?> "registrationCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRegistrationCode where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetRegistrationCode where
  rnf _ = ()

instance Data.ToHeaders GetRegistrationCode where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetRegistrationCode where
  toPath = Prelude.const "/registrationcode"

instance Data.ToQuery GetRegistrationCode where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the GetRegistrationCode operation.
--
-- /See:/ 'newGetRegistrationCodeResponse' smart constructor.
data GetRegistrationCodeResponse = GetRegistrationCodeResponse'
  { -- | The CA certificate registration code.
    registrationCode :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegistrationCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registrationCode', 'getRegistrationCodeResponse_registrationCode' - The CA certificate registration code.
--
-- 'httpStatus', 'getRegistrationCodeResponse_httpStatus' - The response's http status code.
newGetRegistrationCodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRegistrationCodeResponse
newGetRegistrationCodeResponse pHttpStatus_ =
  GetRegistrationCodeResponse'
    { registrationCode =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The CA certificate registration code.
getRegistrationCodeResponse_registrationCode :: Lens.Lens' GetRegistrationCodeResponse (Prelude.Maybe Prelude.Text)
getRegistrationCodeResponse_registrationCode = Lens.lens (\GetRegistrationCodeResponse' {registrationCode} -> registrationCode) (\s@GetRegistrationCodeResponse' {} a -> s {registrationCode = a} :: GetRegistrationCodeResponse)

-- | The response's http status code.
getRegistrationCodeResponse_httpStatus :: Lens.Lens' GetRegistrationCodeResponse Prelude.Int
getRegistrationCodeResponse_httpStatus = Lens.lens (\GetRegistrationCodeResponse' {httpStatus} -> httpStatus) (\s@GetRegistrationCodeResponse' {} a -> s {httpStatus = a} :: GetRegistrationCodeResponse)

instance Prelude.NFData GetRegistrationCodeResponse where
  rnf GetRegistrationCodeResponse' {..} =
    Prelude.rnf registrationCode
      `Prelude.seq` Prelude.rnf httpStatus
