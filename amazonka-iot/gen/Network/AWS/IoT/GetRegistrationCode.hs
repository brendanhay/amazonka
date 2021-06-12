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
-- Module      : Network.AWS.IoT.GetRegistrationCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a registration code used to register a CA certificate with AWS IoT.
module Network.AWS.IoT.GetRegistrationCode
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the GetRegistrationCode operation.
--
-- /See:/ 'newGetRegistrationCode' smart constructor.
data GetRegistrationCode = GetRegistrationCode'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegistrationCodeResponse'
            Core.<$> (x Core..?> "registrationCode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRegistrationCode

instance Core.NFData GetRegistrationCode

instance Core.ToHeaders GetRegistrationCode where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetRegistrationCode where
  toPath = Core.const "/registrationcode"

instance Core.ToQuery GetRegistrationCode where
  toQuery = Core.const Core.mempty

-- | The output from the GetRegistrationCode operation.
--
-- /See:/ 'newGetRegistrationCodeResponse' smart constructor.
data GetRegistrationCodeResponse = GetRegistrationCodeResponse'
  { -- | The CA certificate registration code.
    registrationCode :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetRegistrationCodeResponse
newGetRegistrationCodeResponse pHttpStatus_ =
  GetRegistrationCodeResponse'
    { registrationCode =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The CA certificate registration code.
getRegistrationCodeResponse_registrationCode :: Lens.Lens' GetRegistrationCodeResponse (Core.Maybe Core.Text)
getRegistrationCodeResponse_registrationCode = Lens.lens (\GetRegistrationCodeResponse' {registrationCode} -> registrationCode) (\s@GetRegistrationCodeResponse' {} a -> s {registrationCode = a} :: GetRegistrationCodeResponse)

-- | The response's http status code.
getRegistrationCodeResponse_httpStatus :: Lens.Lens' GetRegistrationCodeResponse Core.Int
getRegistrationCodeResponse_httpStatus = Lens.lens (\GetRegistrationCodeResponse' {httpStatus} -> httpStatus) (\s@GetRegistrationCodeResponse' {} a -> s {httpStatus = a} :: GetRegistrationCodeResponse)

instance Core.NFData GetRegistrationCodeResponse
