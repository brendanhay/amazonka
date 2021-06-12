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
-- Module      : Network.AWS.IoT.UpdateAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an authorizer.
module Network.AWS.IoT.UpdateAuthorizer
  ( -- * Creating a Request
    UpdateAuthorizer (..),
    newUpdateAuthorizer,

    -- * Request Lenses
    updateAuthorizer_status,
    updateAuthorizer_authorizerFunctionArn,
    updateAuthorizer_tokenSigningPublicKeys,
    updateAuthorizer_tokenKeyName,
    updateAuthorizer_authorizerName,

    -- * Destructuring the Response
    UpdateAuthorizerResponse (..),
    newUpdateAuthorizerResponse,

    -- * Response Lenses
    updateAuthorizerResponse_authorizerArn,
    updateAuthorizerResponse_authorizerName,
    updateAuthorizerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
  { -- | The status of the update authorizer request.
    status :: Core.Maybe AuthorizerStatus,
    -- | The ARN of the authorizer\'s Lambda function.
    authorizerFunctionArn :: Core.Maybe Core.Text,
    -- | The public keys used to verify the token signature.
    tokenSigningPublicKeys :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The key used to extract the token from the HTTP headers.
    tokenKeyName :: Core.Maybe Core.Text,
    -- | The authorizer name.
    authorizerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateAuthorizer_status' - The status of the update authorizer request.
--
-- 'authorizerFunctionArn', 'updateAuthorizer_authorizerFunctionArn' - The ARN of the authorizer\'s Lambda function.
--
-- 'tokenSigningPublicKeys', 'updateAuthorizer_tokenSigningPublicKeys' - The public keys used to verify the token signature.
--
-- 'tokenKeyName', 'updateAuthorizer_tokenKeyName' - The key used to extract the token from the HTTP headers.
--
-- 'authorizerName', 'updateAuthorizer_authorizerName' - The authorizer name.
newUpdateAuthorizer ::
  -- | 'authorizerName'
  Core.Text ->
  UpdateAuthorizer
newUpdateAuthorizer pAuthorizerName_ =
  UpdateAuthorizer'
    { status = Core.Nothing,
      authorizerFunctionArn = Core.Nothing,
      tokenSigningPublicKeys = Core.Nothing,
      tokenKeyName = Core.Nothing,
      authorizerName = pAuthorizerName_
    }

-- | The status of the update authorizer request.
updateAuthorizer_status :: Lens.Lens' UpdateAuthorizer (Core.Maybe AuthorizerStatus)
updateAuthorizer_status = Lens.lens (\UpdateAuthorizer' {status} -> status) (\s@UpdateAuthorizer' {} a -> s {status = a} :: UpdateAuthorizer)

-- | The ARN of the authorizer\'s Lambda function.
updateAuthorizer_authorizerFunctionArn :: Lens.Lens' UpdateAuthorizer (Core.Maybe Core.Text)
updateAuthorizer_authorizerFunctionArn = Lens.lens (\UpdateAuthorizer' {authorizerFunctionArn} -> authorizerFunctionArn) (\s@UpdateAuthorizer' {} a -> s {authorizerFunctionArn = a} :: UpdateAuthorizer)

-- | The public keys used to verify the token signature.
updateAuthorizer_tokenSigningPublicKeys :: Lens.Lens' UpdateAuthorizer (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateAuthorizer_tokenSigningPublicKeys = Lens.lens (\UpdateAuthorizer' {tokenSigningPublicKeys} -> tokenSigningPublicKeys) (\s@UpdateAuthorizer' {} a -> s {tokenSigningPublicKeys = a} :: UpdateAuthorizer) Core.. Lens.mapping Lens._Coerce

-- | The key used to extract the token from the HTTP headers.
updateAuthorizer_tokenKeyName :: Lens.Lens' UpdateAuthorizer (Core.Maybe Core.Text)
updateAuthorizer_tokenKeyName = Lens.lens (\UpdateAuthorizer' {tokenKeyName} -> tokenKeyName) (\s@UpdateAuthorizer' {} a -> s {tokenKeyName = a} :: UpdateAuthorizer)

-- | The authorizer name.
updateAuthorizer_authorizerName :: Lens.Lens' UpdateAuthorizer Core.Text
updateAuthorizer_authorizerName = Lens.lens (\UpdateAuthorizer' {authorizerName} -> authorizerName) (\s@UpdateAuthorizer' {} a -> s {authorizerName = a} :: UpdateAuthorizer)

instance Core.AWSRequest UpdateAuthorizer where
  type
    AWSResponse UpdateAuthorizer =
      UpdateAuthorizerResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAuthorizerResponse'
            Core.<$> (x Core..?> "authorizerArn")
            Core.<*> (x Core..?> "authorizerName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateAuthorizer

instance Core.NFData UpdateAuthorizer

instance Core.ToHeaders UpdateAuthorizer where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateAuthorizer where
  toJSON UpdateAuthorizer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("status" Core..=) Core.<$> status,
            ("authorizerFunctionArn" Core..=)
              Core.<$> authorizerFunctionArn,
            ("tokenSigningPublicKeys" Core..=)
              Core.<$> tokenSigningPublicKeys,
            ("tokenKeyName" Core..=) Core.<$> tokenKeyName
          ]
      )

instance Core.ToPath UpdateAuthorizer where
  toPath UpdateAuthorizer' {..} =
    Core.mconcat
      ["/authorizer/", Core.toBS authorizerName]

instance Core.ToQuery UpdateAuthorizer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAuthorizerResponse' smart constructor.
data UpdateAuthorizerResponse = UpdateAuthorizerResponse'
  { -- | The authorizer ARN.
    authorizerArn :: Core.Maybe Core.Text,
    -- | The authorizer name.
    authorizerName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerArn', 'updateAuthorizerResponse_authorizerArn' - The authorizer ARN.
--
-- 'authorizerName', 'updateAuthorizerResponse_authorizerName' - The authorizer name.
--
-- 'httpStatus', 'updateAuthorizerResponse_httpStatus' - The response's http status code.
newUpdateAuthorizerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateAuthorizerResponse
newUpdateAuthorizerResponse pHttpStatus_ =
  UpdateAuthorizerResponse'
    { authorizerArn =
        Core.Nothing,
      authorizerName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authorizer ARN.
updateAuthorizerResponse_authorizerArn :: Lens.Lens' UpdateAuthorizerResponse (Core.Maybe Core.Text)
updateAuthorizerResponse_authorizerArn = Lens.lens (\UpdateAuthorizerResponse' {authorizerArn} -> authorizerArn) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerArn = a} :: UpdateAuthorizerResponse)

-- | The authorizer name.
updateAuthorizerResponse_authorizerName :: Lens.Lens' UpdateAuthorizerResponse (Core.Maybe Core.Text)
updateAuthorizerResponse_authorizerName = Lens.lens (\UpdateAuthorizerResponse' {authorizerName} -> authorizerName) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerName = a} :: UpdateAuthorizerResponse)

-- | The response's http status code.
updateAuthorizerResponse_httpStatus :: Lens.Lens' UpdateAuthorizerResponse Core.Int
updateAuthorizerResponse_httpStatus = Lens.lens (\UpdateAuthorizerResponse' {httpStatus} -> httpStatus) (\s@UpdateAuthorizerResponse' {} a -> s {httpStatus = a} :: UpdateAuthorizerResponse)

instance Core.NFData UpdateAuthorizerResponse
