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
-- Module      : Network.AWS.ECRPublic.GetAuthorizationToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an authorization token. An authorization token represents your
-- IAM authentication credentials and can be used to access any Amazon ECR
-- registry that your IAM principal has access to. The authorization token
-- is valid for 12 hours. This API requires the
-- @ecr-public:GetAuthorizationToken@ and @sts:GetServiceBearerToken@
-- permissions.
module Network.AWS.ECRPublic.GetAuthorizationToken
  ( -- * Creating a Request
    GetAuthorizationToken (..),
    newGetAuthorizationToken,

    -- * Destructuring the Response
    GetAuthorizationTokenResponse (..),
    newGetAuthorizationTokenResponse,

    -- * Response Lenses
    getAuthorizationTokenResponse_authorizationData,
    getAuthorizationTokenResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECRPublic.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAuthorizationToken' smart constructor.
data GetAuthorizationToken = GetAuthorizationToken'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAuthorizationToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAuthorizationToken ::
  GetAuthorizationToken
newGetAuthorizationToken = GetAuthorizationToken'

instance Core.AWSRequest GetAuthorizationToken where
  type
    AWSResponse GetAuthorizationToken =
      GetAuthorizationTokenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAuthorizationTokenResponse'
            Prelude.<$> (x Core..?> "authorizationData")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAuthorizationToken

instance Prelude.NFData GetAuthorizationToken

instance Core.ToHeaders GetAuthorizationToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SpencerFrontendService.GetAuthorizationToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetAuthorizationToken where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetAuthorizationToken where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAuthorizationToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAuthorizationTokenResponse' smart constructor.
data GetAuthorizationTokenResponse = GetAuthorizationTokenResponse'
  { -- | An authorization token data object that corresponds to a public
    -- registry.
    authorizationData :: Prelude.Maybe AuthorizationData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAuthorizationTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationData', 'getAuthorizationTokenResponse_authorizationData' - An authorization token data object that corresponds to a public
-- registry.
--
-- 'httpStatus', 'getAuthorizationTokenResponse_httpStatus' - The response's http status code.
newGetAuthorizationTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAuthorizationTokenResponse
newGetAuthorizationTokenResponse pHttpStatus_ =
  GetAuthorizationTokenResponse'
    { authorizationData =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An authorization token data object that corresponds to a public
-- registry.
getAuthorizationTokenResponse_authorizationData :: Lens.Lens' GetAuthorizationTokenResponse (Prelude.Maybe AuthorizationData)
getAuthorizationTokenResponse_authorizationData = Lens.lens (\GetAuthorizationTokenResponse' {authorizationData} -> authorizationData) (\s@GetAuthorizationTokenResponse' {} a -> s {authorizationData = a} :: GetAuthorizationTokenResponse)

-- | The response's http status code.
getAuthorizationTokenResponse_httpStatus :: Lens.Lens' GetAuthorizationTokenResponse Prelude.Int
getAuthorizationTokenResponse_httpStatus = Lens.lens (\GetAuthorizationTokenResponse' {httpStatus} -> httpStatus) (\s@GetAuthorizationTokenResponse' {} a -> s {httpStatus = a} :: GetAuthorizationTokenResponse)

instance Prelude.NFData GetAuthorizationTokenResponse
