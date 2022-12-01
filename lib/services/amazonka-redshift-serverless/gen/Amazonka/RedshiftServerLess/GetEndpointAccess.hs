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
-- Module      : Amazonka.RedshiftServerLess.GetEndpointAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information, such as the name, about a VPC endpoint.
module Amazonka.RedshiftServerLess.GetEndpointAccess
  ( -- * Creating a Request
    GetEndpointAccess (..),
    newGetEndpointAccess,

    -- * Request Lenses
    getEndpointAccess_endpointName,

    -- * Destructuring the Response
    GetEndpointAccessResponse (..),
    newGetEndpointAccessResponse,

    -- * Response Lenses
    getEndpointAccessResponse_endpoint,
    getEndpointAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEndpointAccess' smart constructor.
data GetEndpointAccess = GetEndpointAccess'
  { -- | The name of the VPC endpoint to return information for.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'getEndpointAccess_endpointName' - The name of the VPC endpoint to return information for.
newGetEndpointAccess ::
  -- | 'endpointName'
  Prelude.Text ->
  GetEndpointAccess
newGetEndpointAccess pEndpointName_ =
  GetEndpointAccess' {endpointName = pEndpointName_}

-- | The name of the VPC endpoint to return information for.
getEndpointAccess_endpointName :: Lens.Lens' GetEndpointAccess Prelude.Text
getEndpointAccess_endpointName = Lens.lens (\GetEndpointAccess' {endpointName} -> endpointName) (\s@GetEndpointAccess' {} a -> s {endpointName = a} :: GetEndpointAccess)

instance Core.AWSRequest GetEndpointAccess where
  type
    AWSResponse GetEndpointAccess =
      GetEndpointAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEndpointAccessResponse'
            Prelude.<$> (x Core..?> "endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEndpointAccess where
  hashWithSalt _salt GetEndpointAccess' {..} =
    _salt `Prelude.hashWithSalt` endpointName

instance Prelude.NFData GetEndpointAccess where
  rnf GetEndpointAccess' {..} = Prelude.rnf endpointName

instance Core.ToHeaders GetEndpointAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RedshiftServerless.GetEndpointAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEndpointAccess where
  toJSON GetEndpointAccess' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("endpointName" Core..= endpointName)]
      )

instance Core.ToPath GetEndpointAccess where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEndpointAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEndpointAccessResponse' smart constructor.
data GetEndpointAccessResponse = GetEndpointAccessResponse'
  { -- | The returned VPC endpoint.
    endpoint :: Prelude.Maybe EndpointAccess,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEndpointAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'getEndpointAccessResponse_endpoint' - The returned VPC endpoint.
--
-- 'httpStatus', 'getEndpointAccessResponse_httpStatus' - The response's http status code.
newGetEndpointAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEndpointAccessResponse
newGetEndpointAccessResponse pHttpStatus_ =
  GetEndpointAccessResponse'
    { endpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned VPC endpoint.
getEndpointAccessResponse_endpoint :: Lens.Lens' GetEndpointAccessResponse (Prelude.Maybe EndpointAccess)
getEndpointAccessResponse_endpoint = Lens.lens (\GetEndpointAccessResponse' {endpoint} -> endpoint) (\s@GetEndpointAccessResponse' {} a -> s {endpoint = a} :: GetEndpointAccessResponse)

-- | The response's http status code.
getEndpointAccessResponse_httpStatus :: Lens.Lens' GetEndpointAccessResponse Prelude.Int
getEndpointAccessResponse_httpStatus = Lens.lens (\GetEndpointAccessResponse' {httpStatus} -> httpStatus) (\s@GetEndpointAccessResponse' {} a -> s {httpStatus = a} :: GetEndpointAccessResponse)

instance Prelude.NFData GetEndpointAccessResponse where
  rnf GetEndpointAccessResponse' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf httpStatus
