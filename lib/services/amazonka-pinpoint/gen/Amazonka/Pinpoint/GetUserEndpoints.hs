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
-- Module      : Amazonka.Pinpoint.GetUserEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the endpoints that are associated with a
-- specific user ID.
module Amazonka.Pinpoint.GetUserEndpoints
  ( -- * Creating a Request
    GetUserEndpoints (..),
    newGetUserEndpoints,

    -- * Request Lenses
    getUserEndpoints_applicationId,
    getUserEndpoints_userId,

    -- * Destructuring the Response
    GetUserEndpointsResponse (..),
    newGetUserEndpointsResponse,

    -- * Response Lenses
    getUserEndpointsResponse_httpStatus,
    getUserEndpointsResponse_endpointsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUserEndpoints' smart constructor.
data GetUserEndpoints = GetUserEndpoints'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getUserEndpoints_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'userId', 'getUserEndpoints_userId' - The unique identifier for the user.
newGetUserEndpoints ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  GetUserEndpoints
newGetUserEndpoints pApplicationId_ pUserId_ =
  GetUserEndpoints'
    { applicationId = pApplicationId_,
      userId = pUserId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getUserEndpoints_applicationId :: Lens.Lens' GetUserEndpoints Prelude.Text
getUserEndpoints_applicationId = Lens.lens (\GetUserEndpoints' {applicationId} -> applicationId) (\s@GetUserEndpoints' {} a -> s {applicationId = a} :: GetUserEndpoints)

-- | The unique identifier for the user.
getUserEndpoints_userId :: Lens.Lens' GetUserEndpoints Prelude.Text
getUserEndpoints_userId = Lens.lens (\GetUserEndpoints' {userId} -> userId) (\s@GetUserEndpoints' {} a -> s {userId = a} :: GetUserEndpoints)

instance Core.AWSRequest GetUserEndpoints where
  type
    AWSResponse GetUserEndpoints =
      GetUserEndpointsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserEndpointsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetUserEndpoints where
  hashWithSalt _salt GetUserEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData GetUserEndpoints where
  rnf GetUserEndpoints' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf userId

instance Data.ToHeaders GetUserEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetUserEndpoints where
  toPath GetUserEndpoints' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/users/",
        Data.toBS userId
      ]

instance Data.ToQuery GetUserEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUserEndpointsResponse' smart constructor.
data GetUserEndpointsResponse = GetUserEndpointsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    endpointsResponse :: EndpointsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getUserEndpointsResponse_httpStatus' - The response's http status code.
--
-- 'endpointsResponse', 'getUserEndpointsResponse_endpointsResponse' - Undocumented member.
newGetUserEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'endpointsResponse'
  EndpointsResponse ->
  GetUserEndpointsResponse
newGetUserEndpointsResponse
  pHttpStatus_
  pEndpointsResponse_ =
    GetUserEndpointsResponse'
      { httpStatus =
          pHttpStatus_,
        endpointsResponse = pEndpointsResponse_
      }

-- | The response's http status code.
getUserEndpointsResponse_httpStatus :: Lens.Lens' GetUserEndpointsResponse Prelude.Int
getUserEndpointsResponse_httpStatus = Lens.lens (\GetUserEndpointsResponse' {httpStatus} -> httpStatus) (\s@GetUserEndpointsResponse' {} a -> s {httpStatus = a} :: GetUserEndpointsResponse)

-- | Undocumented member.
getUserEndpointsResponse_endpointsResponse :: Lens.Lens' GetUserEndpointsResponse EndpointsResponse
getUserEndpointsResponse_endpointsResponse = Lens.lens (\GetUserEndpointsResponse' {endpointsResponse} -> endpointsResponse) (\s@GetUserEndpointsResponse' {} a -> s {endpointsResponse = a} :: GetUserEndpointsResponse)

instance Prelude.NFData GetUserEndpointsResponse where
  rnf GetUserEndpointsResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf endpointsResponse
