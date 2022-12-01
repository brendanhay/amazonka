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
-- Module      : Amazonka.Glue.GetDevEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified development endpoint.
--
-- When you create a development endpoint in a virtual private cloud (VPC),
-- Glue returns only a private IP address, and the public IP address field
-- is not populated. When you create a non-VPC development endpoint, Glue
-- returns only a public IP address.
module Amazonka.Glue.GetDevEndpoint
  ( -- * Creating a Request
    GetDevEndpoint (..),
    newGetDevEndpoint,

    -- * Request Lenses
    getDevEndpoint_endpointName,

    -- * Destructuring the Response
    GetDevEndpointResponse (..),
    newGetDevEndpointResponse,

    -- * Response Lenses
    getDevEndpointResponse_devEndpoint,
    getDevEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDevEndpoint' smart constructor.
data GetDevEndpoint = GetDevEndpoint'
  { -- | Name of the @DevEndpoint@ to retrieve information for.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'getDevEndpoint_endpointName' - Name of the @DevEndpoint@ to retrieve information for.
newGetDevEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  GetDevEndpoint
newGetDevEndpoint pEndpointName_ =
  GetDevEndpoint' {endpointName = pEndpointName_}

-- | Name of the @DevEndpoint@ to retrieve information for.
getDevEndpoint_endpointName :: Lens.Lens' GetDevEndpoint Prelude.Text
getDevEndpoint_endpointName = Lens.lens (\GetDevEndpoint' {endpointName} -> endpointName) (\s@GetDevEndpoint' {} a -> s {endpointName = a} :: GetDevEndpoint)

instance Core.AWSRequest GetDevEndpoint where
  type
    AWSResponse GetDevEndpoint =
      GetDevEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevEndpointResponse'
            Prelude.<$> (x Core..?> "DevEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDevEndpoint where
  hashWithSalt _salt GetDevEndpoint' {..} =
    _salt `Prelude.hashWithSalt` endpointName

instance Prelude.NFData GetDevEndpoint where
  rnf GetDevEndpoint' {..} = Prelude.rnf endpointName

instance Core.ToHeaders GetDevEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetDevEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDevEndpoint where
  toJSON GetDevEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("EndpointName" Core..= endpointName)]
      )

instance Core.ToPath GetDevEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDevEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDevEndpointResponse' smart constructor.
data GetDevEndpointResponse = GetDevEndpointResponse'
  { -- | A @DevEndpoint@ definition.
    devEndpoint :: Prelude.Maybe DevEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devEndpoint', 'getDevEndpointResponse_devEndpoint' - A @DevEndpoint@ definition.
--
-- 'httpStatus', 'getDevEndpointResponse_httpStatus' - The response's http status code.
newGetDevEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDevEndpointResponse
newGetDevEndpointResponse pHttpStatus_ =
  GetDevEndpointResponse'
    { devEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @DevEndpoint@ definition.
getDevEndpointResponse_devEndpoint :: Lens.Lens' GetDevEndpointResponse (Prelude.Maybe DevEndpoint)
getDevEndpointResponse_devEndpoint = Lens.lens (\GetDevEndpointResponse' {devEndpoint} -> devEndpoint) (\s@GetDevEndpointResponse' {} a -> s {devEndpoint = a} :: GetDevEndpointResponse)

-- | The response's http status code.
getDevEndpointResponse_httpStatus :: Lens.Lens' GetDevEndpointResponse Prelude.Int
getDevEndpointResponse_httpStatus = Lens.lens (\GetDevEndpointResponse' {httpStatus} -> httpStatus) (\s@GetDevEndpointResponse' {} a -> s {httpStatus = a} :: GetDevEndpointResponse)

instance Prelude.NFData GetDevEndpointResponse where
  rnf GetDevEndpointResponse' {..} =
    Prelude.rnf devEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
