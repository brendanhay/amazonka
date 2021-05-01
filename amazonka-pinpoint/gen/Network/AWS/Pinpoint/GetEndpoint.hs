{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.GetEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the settings and attributes of a specific
-- endpoint for an application.
module Network.AWS.Pinpoint.GetEndpoint
  ( -- * Creating a Request
    GetEndpoint (..),
    newGetEndpoint,

    -- * Request Lenses
    getEndpoint_applicationId,
    getEndpoint_endpointId,

    -- * Destructuring the Response
    GetEndpointResponse (..),
    newGetEndpointResponse,

    -- * Response Lenses
    getEndpointResponse_httpStatus,
    getEndpointResponse_endpointResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEndpoint' smart constructor.
data GetEndpoint = GetEndpoint'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the endpoint.
    endpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getEndpoint_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'endpointId', 'getEndpoint_endpointId' - The unique identifier for the endpoint.
newGetEndpoint ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'endpointId'
  Prelude.Text ->
  GetEndpoint
newGetEndpoint pApplicationId_ pEndpointId_ =
  GetEndpoint'
    { applicationId = pApplicationId_,
      endpointId = pEndpointId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getEndpoint_applicationId :: Lens.Lens' GetEndpoint Prelude.Text
getEndpoint_applicationId = Lens.lens (\GetEndpoint' {applicationId} -> applicationId) (\s@GetEndpoint' {} a -> s {applicationId = a} :: GetEndpoint)

-- | The unique identifier for the endpoint.
getEndpoint_endpointId :: Lens.Lens' GetEndpoint Prelude.Text
getEndpoint_endpointId = Lens.lens (\GetEndpoint' {endpointId} -> endpointId) (\s@GetEndpoint' {} a -> s {endpointId = a} :: GetEndpoint)

instance Prelude.AWSRequest GetEndpoint where
  type Rs GetEndpoint = GetEndpointResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable GetEndpoint

instance Prelude.NFData GetEndpoint

instance Prelude.ToHeaders GetEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetEndpoint where
  toPath GetEndpoint' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/endpoints/",
        Prelude.toBS endpointId
      ]

instance Prelude.ToQuery GetEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEndpointResponse' smart constructor.
data GetEndpointResponse = GetEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    endpointResponse :: EndpointResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getEndpointResponse_httpStatus' - The response's http status code.
--
-- 'endpointResponse', 'getEndpointResponse_endpointResponse' - Undocumented member.
newGetEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'endpointResponse'
  EndpointResponse ->
  GetEndpointResponse
newGetEndpointResponse
  pHttpStatus_
  pEndpointResponse_ =
    GetEndpointResponse'
      { httpStatus = pHttpStatus_,
        endpointResponse = pEndpointResponse_
      }

-- | The response's http status code.
getEndpointResponse_httpStatus :: Lens.Lens' GetEndpointResponse Prelude.Int
getEndpointResponse_httpStatus = Lens.lens (\GetEndpointResponse' {httpStatus} -> httpStatus) (\s@GetEndpointResponse' {} a -> s {httpStatus = a} :: GetEndpointResponse)

-- | Undocumented member.
getEndpointResponse_endpointResponse :: Lens.Lens' GetEndpointResponse EndpointResponse
getEndpointResponse_endpointResponse = Lens.lens (\GetEndpointResponse' {endpointResponse} -> endpointResponse) (\s@GetEndpointResponse' {} a -> s {endpointResponse = a} :: GetEndpointResponse)

instance Prelude.NFData GetEndpointResponse
