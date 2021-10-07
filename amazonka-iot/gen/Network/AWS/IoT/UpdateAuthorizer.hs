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
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateAuthorizer>
-- action.
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
  { -- | The status of the update authorizer request.
    status :: Prelude.Maybe AuthorizerStatus,
    -- | The ARN of the authorizer\'s Lambda function.
    authorizerFunctionArn :: Prelude.Maybe Prelude.Text,
    -- | The public keys used to verify the token signature.
    tokenSigningPublicKeys :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The key used to extract the token from the HTTP headers.
    tokenKeyName :: Prelude.Maybe Prelude.Text,
    -- | The authorizer name.
    authorizerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateAuthorizer
newUpdateAuthorizer pAuthorizerName_ =
  UpdateAuthorizer'
    { status = Prelude.Nothing,
      authorizerFunctionArn = Prelude.Nothing,
      tokenSigningPublicKeys = Prelude.Nothing,
      tokenKeyName = Prelude.Nothing,
      authorizerName = pAuthorizerName_
    }

-- | The status of the update authorizer request.
updateAuthorizer_status :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe AuthorizerStatus)
updateAuthorizer_status = Lens.lens (\UpdateAuthorizer' {status} -> status) (\s@UpdateAuthorizer' {} a -> s {status = a} :: UpdateAuthorizer)

-- | The ARN of the authorizer\'s Lambda function.
updateAuthorizer_authorizerFunctionArn :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe Prelude.Text)
updateAuthorizer_authorizerFunctionArn = Lens.lens (\UpdateAuthorizer' {authorizerFunctionArn} -> authorizerFunctionArn) (\s@UpdateAuthorizer' {} a -> s {authorizerFunctionArn = a} :: UpdateAuthorizer)

-- | The public keys used to verify the token signature.
updateAuthorizer_tokenSigningPublicKeys :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateAuthorizer_tokenSigningPublicKeys = Lens.lens (\UpdateAuthorizer' {tokenSigningPublicKeys} -> tokenSigningPublicKeys) (\s@UpdateAuthorizer' {} a -> s {tokenSigningPublicKeys = a} :: UpdateAuthorizer) Prelude.. Lens.mapping Lens._Coerce

-- | The key used to extract the token from the HTTP headers.
updateAuthorizer_tokenKeyName :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe Prelude.Text)
updateAuthorizer_tokenKeyName = Lens.lens (\UpdateAuthorizer' {tokenKeyName} -> tokenKeyName) (\s@UpdateAuthorizer' {} a -> s {tokenKeyName = a} :: UpdateAuthorizer)

-- | The authorizer name.
updateAuthorizer_authorizerName :: Lens.Lens' UpdateAuthorizer Prelude.Text
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
            Prelude.<$> (x Core..?> "authorizerArn")
            Prelude.<*> (x Core..?> "authorizerName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAuthorizer

instance Prelude.NFData UpdateAuthorizer

instance Core.ToHeaders UpdateAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateAuthorizer where
  toJSON UpdateAuthorizer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            ("authorizerFunctionArn" Core..=)
              Prelude.<$> authorizerFunctionArn,
            ("tokenSigningPublicKeys" Core..=)
              Prelude.<$> tokenSigningPublicKeys,
            ("tokenKeyName" Core..=) Prelude.<$> tokenKeyName
          ]
      )

instance Core.ToPath UpdateAuthorizer where
  toPath UpdateAuthorizer' {..} =
    Prelude.mconcat
      ["/authorizer/", Core.toBS authorizerName]

instance Core.ToQuery UpdateAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAuthorizerResponse' smart constructor.
data UpdateAuthorizerResponse = UpdateAuthorizerResponse'
  { -- | The authorizer ARN.
    authorizerArn :: Prelude.Maybe Prelude.Text,
    -- | The authorizer name.
    authorizerName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateAuthorizerResponse
newUpdateAuthorizerResponse pHttpStatus_ =
  UpdateAuthorizerResponse'
    { authorizerArn =
        Prelude.Nothing,
      authorizerName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authorizer ARN.
updateAuthorizerResponse_authorizerArn :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Text)
updateAuthorizerResponse_authorizerArn = Lens.lens (\UpdateAuthorizerResponse' {authorizerArn} -> authorizerArn) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerArn = a} :: UpdateAuthorizerResponse)

-- | The authorizer name.
updateAuthorizerResponse_authorizerName :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Text)
updateAuthorizerResponse_authorizerName = Lens.lens (\UpdateAuthorizerResponse' {authorizerName} -> authorizerName) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerName = a} :: UpdateAuthorizerResponse)

-- | The response's http status code.
updateAuthorizerResponse_httpStatus :: Lens.Lens' UpdateAuthorizerResponse Prelude.Int
updateAuthorizerResponse_httpStatus = Lens.lens (\UpdateAuthorizerResponse' {httpStatus} -> httpStatus) (\s@UpdateAuthorizerResponse' {} a -> s {httpStatus = a} :: UpdateAuthorizerResponse)

instance Prelude.NFData UpdateAuthorizerResponse
