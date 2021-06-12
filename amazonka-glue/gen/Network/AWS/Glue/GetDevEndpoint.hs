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
-- Module      : Network.AWS.Glue.GetDevEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified development endpoint.
--
-- When you create a development endpoint in a virtual private cloud (VPC),
-- AWS Glue returns only a private IP address, and the public IP address
-- field is not populated. When you create a non-VPC development endpoint,
-- AWS Glue returns only a public IP address.
module Network.AWS.Glue.GetDevEndpoint
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDevEndpoint' smart constructor.
data GetDevEndpoint = GetDevEndpoint'
  { -- | Name of the @DevEndpoint@ to retrieve information for.
    endpointName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetDevEndpoint
newGetDevEndpoint pEndpointName_ =
  GetDevEndpoint' {endpointName = pEndpointName_}

-- | Name of the @DevEndpoint@ to retrieve information for.
getDevEndpoint_endpointName :: Lens.Lens' GetDevEndpoint Core.Text
getDevEndpoint_endpointName = Lens.lens (\GetDevEndpoint' {endpointName} -> endpointName) (\s@GetDevEndpoint' {} a -> s {endpointName = a} :: GetDevEndpoint)

instance Core.AWSRequest GetDevEndpoint where
  type
    AWSResponse GetDevEndpoint =
      GetDevEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevEndpointResponse'
            Core.<$> (x Core..?> "DevEndpoint")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDevEndpoint

instance Core.NFData GetDevEndpoint

instance Core.ToHeaders GetDevEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetDevEndpoint" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDevEndpoint where
  toJSON GetDevEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EndpointName" Core..= endpointName)]
      )

instance Core.ToPath GetDevEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery GetDevEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDevEndpointResponse' smart constructor.
data GetDevEndpointResponse = GetDevEndpointResponse'
  { -- | A @DevEndpoint@ definition.
    devEndpoint :: Core.Maybe DevEndpoint,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetDevEndpointResponse
newGetDevEndpointResponse pHttpStatus_ =
  GetDevEndpointResponse'
    { devEndpoint = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @DevEndpoint@ definition.
getDevEndpointResponse_devEndpoint :: Lens.Lens' GetDevEndpointResponse (Core.Maybe DevEndpoint)
getDevEndpointResponse_devEndpoint = Lens.lens (\GetDevEndpointResponse' {devEndpoint} -> devEndpoint) (\s@GetDevEndpointResponse' {} a -> s {devEndpoint = a} :: GetDevEndpointResponse)

-- | The response's http status code.
getDevEndpointResponse_httpStatus :: Lens.Lens' GetDevEndpointResponse Core.Int
getDevEndpointResponse_httpStatus = Lens.lens (\GetDevEndpointResponse' {httpStatus} -> httpStatus) (\s@GetDevEndpointResponse' {} a -> s {httpStatus = a} :: GetDevEndpointResponse)

instance Core.NFData GetDevEndpointResponse
