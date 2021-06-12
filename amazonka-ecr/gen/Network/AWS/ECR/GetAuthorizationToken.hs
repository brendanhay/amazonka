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
-- Module      : Network.AWS.ECR.GetAuthorizationToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an authorization token. An authorization token represents your
-- IAM authentication credentials and can be used to access any Amazon ECR
-- registry that your IAM principal has access to. The authorization token
-- is valid for 12 hours.
--
-- The @authorizationToken@ returned is a base64 encoded string that can be
-- decoded and used in a @docker login@ command to authenticate to a
-- registry. The AWS CLI offers an @get-login-password@ command that
-- simplifies the login process. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/Registries.html#registry_auth Registry Authentication>
-- in the /Amazon Elastic Container Registry User Guide/.
module Network.AWS.ECR.GetAuthorizationToken
  ( -- * Creating a Request
    GetAuthorizationToken (..),
    newGetAuthorizationToken,

    -- * Request Lenses
    getAuthorizationToken_registryIds,

    -- * Destructuring the Response
    GetAuthorizationTokenResponse (..),
    newGetAuthorizationTokenResponse,

    -- * Response Lenses
    getAuthorizationTokenResponse_authorizationData,
    getAuthorizationTokenResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAuthorizationToken' smart constructor.
data GetAuthorizationToken = GetAuthorizationToken'
  { -- | A list of AWS account IDs that are associated with the registries for
    -- which to get AuthorizationData objects. If you do not specify a
    -- registry, the default registry is assumed.
    registryIds :: Core.Maybe (Core.NonEmpty Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAuthorizationToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryIds', 'getAuthorizationToken_registryIds' - A list of AWS account IDs that are associated with the registries for
-- which to get AuthorizationData objects. If you do not specify a
-- registry, the default registry is assumed.
newGetAuthorizationToken ::
  GetAuthorizationToken
newGetAuthorizationToken =
  GetAuthorizationToken' {registryIds = Core.Nothing}

-- | A list of AWS account IDs that are associated with the registries for
-- which to get AuthorizationData objects. If you do not specify a
-- registry, the default registry is assumed.
getAuthorizationToken_registryIds :: Lens.Lens' GetAuthorizationToken (Core.Maybe (Core.NonEmpty Core.Text))
getAuthorizationToken_registryIds = Lens.lens (\GetAuthorizationToken' {registryIds} -> registryIds) (\s@GetAuthorizationToken' {} a -> s {registryIds = a} :: GetAuthorizationToken) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest GetAuthorizationToken where
  type
    AWSResponse GetAuthorizationToken =
      GetAuthorizationTokenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAuthorizationTokenResponse'
            Core.<$> (x Core..?> "authorizationData" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAuthorizationToken

instance Core.NFData GetAuthorizationToken

instance Core.ToHeaders GetAuthorizationToken where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.GetAuthorizationToken" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAuthorizationToken where
  toJSON GetAuthorizationToken' {..} =
    Core.object
      ( Core.catMaybes
          [("registryIds" Core..=) Core.<$> registryIds]
      )

instance Core.ToPath GetAuthorizationToken where
  toPath = Core.const "/"

instance Core.ToQuery GetAuthorizationToken where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAuthorizationTokenResponse' smart constructor.
data GetAuthorizationTokenResponse = GetAuthorizationTokenResponse'
  { -- | A list of authorization token data objects that correspond to the
    -- @registryIds@ values in the request.
    authorizationData :: Core.Maybe [AuthorizationData],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAuthorizationTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationData', 'getAuthorizationTokenResponse_authorizationData' - A list of authorization token data objects that correspond to the
-- @registryIds@ values in the request.
--
-- 'httpStatus', 'getAuthorizationTokenResponse_httpStatus' - The response's http status code.
newGetAuthorizationTokenResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAuthorizationTokenResponse
newGetAuthorizationTokenResponse pHttpStatus_ =
  GetAuthorizationTokenResponse'
    { authorizationData =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of authorization token data objects that correspond to the
-- @registryIds@ values in the request.
getAuthorizationTokenResponse_authorizationData :: Lens.Lens' GetAuthorizationTokenResponse (Core.Maybe [AuthorizationData])
getAuthorizationTokenResponse_authorizationData = Lens.lens (\GetAuthorizationTokenResponse' {authorizationData} -> authorizationData) (\s@GetAuthorizationTokenResponse' {} a -> s {authorizationData = a} :: GetAuthorizationTokenResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAuthorizationTokenResponse_httpStatus :: Lens.Lens' GetAuthorizationTokenResponse Core.Int
getAuthorizationTokenResponse_httpStatus = Lens.lens (\GetAuthorizationTokenResponse' {httpStatus} -> httpStatus) (\s@GetAuthorizationTokenResponse' {} a -> s {httpStatus = a} :: GetAuthorizationTokenResponse)

instance Core.NFData GetAuthorizationTokenResponse
