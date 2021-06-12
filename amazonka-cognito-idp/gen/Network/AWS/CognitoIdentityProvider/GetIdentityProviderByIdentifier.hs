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
-- Module      : Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified identity provider.
module Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
  ( -- * Creating a Request
    GetIdentityProviderByIdentifier (..),
    newGetIdentityProviderByIdentifier,

    -- * Request Lenses
    getIdentityProviderByIdentifier_userPoolId,
    getIdentityProviderByIdentifier_idpIdentifier,

    -- * Destructuring the Response
    GetIdentityProviderByIdentifierResponse (..),
    newGetIdentityProviderByIdentifierResponse,

    -- * Response Lenses
    getIdentityProviderByIdentifierResponse_httpStatus,
    getIdentityProviderByIdentifierResponse_identityProvider,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetIdentityProviderByIdentifier' smart constructor.
data GetIdentityProviderByIdentifier = GetIdentityProviderByIdentifier'
  { -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The identity provider ID.
    idpIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIdentityProviderByIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'getIdentityProviderByIdentifier_userPoolId' - The user pool ID.
--
-- 'idpIdentifier', 'getIdentityProviderByIdentifier_idpIdentifier' - The identity provider ID.
newGetIdentityProviderByIdentifier ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'idpIdentifier'
  Core.Text ->
  GetIdentityProviderByIdentifier
newGetIdentityProviderByIdentifier
  pUserPoolId_
  pIdpIdentifier_ =
    GetIdentityProviderByIdentifier'
      { userPoolId =
          pUserPoolId_,
        idpIdentifier = pIdpIdentifier_
      }

-- | The user pool ID.
getIdentityProviderByIdentifier_userPoolId :: Lens.Lens' GetIdentityProviderByIdentifier Core.Text
getIdentityProviderByIdentifier_userPoolId = Lens.lens (\GetIdentityProviderByIdentifier' {userPoolId} -> userPoolId) (\s@GetIdentityProviderByIdentifier' {} a -> s {userPoolId = a} :: GetIdentityProviderByIdentifier)

-- | The identity provider ID.
getIdentityProviderByIdentifier_idpIdentifier :: Lens.Lens' GetIdentityProviderByIdentifier Core.Text
getIdentityProviderByIdentifier_idpIdentifier = Lens.lens (\GetIdentityProviderByIdentifier' {idpIdentifier} -> idpIdentifier) (\s@GetIdentityProviderByIdentifier' {} a -> s {idpIdentifier = a} :: GetIdentityProviderByIdentifier)

instance
  Core.AWSRequest
    GetIdentityProviderByIdentifier
  where
  type
    AWSResponse GetIdentityProviderByIdentifier =
      GetIdentityProviderByIdentifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentityProviderByIdentifierResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "IdentityProvider")
      )

instance
  Core.Hashable
    GetIdentityProviderByIdentifier

instance Core.NFData GetIdentityProviderByIdentifier

instance
  Core.ToHeaders
    GetIdentityProviderByIdentifier
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetIdentityProviderByIdentifier" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetIdentityProviderByIdentifier where
  toJSON GetIdentityProviderByIdentifier' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("IdpIdentifier" Core..= idpIdentifier)
          ]
      )

instance Core.ToPath GetIdentityProviderByIdentifier where
  toPath = Core.const "/"

instance Core.ToQuery GetIdentityProviderByIdentifier where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetIdentityProviderByIdentifierResponse' smart constructor.
data GetIdentityProviderByIdentifierResponse = GetIdentityProviderByIdentifierResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The identity provider object.
    identityProvider :: IdentityProviderType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIdentityProviderByIdentifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getIdentityProviderByIdentifierResponse_httpStatus' - The response's http status code.
--
-- 'identityProvider', 'getIdentityProviderByIdentifierResponse_identityProvider' - The identity provider object.
newGetIdentityProviderByIdentifierResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'identityProvider'
  IdentityProviderType ->
  GetIdentityProviderByIdentifierResponse
newGetIdentityProviderByIdentifierResponse
  pHttpStatus_
  pIdentityProvider_ =
    GetIdentityProviderByIdentifierResponse'
      { httpStatus =
          pHttpStatus_,
        identityProvider =
          pIdentityProvider_
      }

-- | The response's http status code.
getIdentityProviderByIdentifierResponse_httpStatus :: Lens.Lens' GetIdentityProviderByIdentifierResponse Core.Int
getIdentityProviderByIdentifierResponse_httpStatus = Lens.lens (\GetIdentityProviderByIdentifierResponse' {httpStatus} -> httpStatus) (\s@GetIdentityProviderByIdentifierResponse' {} a -> s {httpStatus = a} :: GetIdentityProviderByIdentifierResponse)

-- | The identity provider object.
getIdentityProviderByIdentifierResponse_identityProvider :: Lens.Lens' GetIdentityProviderByIdentifierResponse IdentityProviderType
getIdentityProviderByIdentifierResponse_identityProvider = Lens.lens (\GetIdentityProviderByIdentifierResponse' {identityProvider} -> identityProvider) (\s@GetIdentityProviderByIdentifierResponse' {} a -> s {identityProvider = a} :: GetIdentityProviderByIdentifierResponse)

instance
  Core.NFData
    GetIdentityProviderByIdentifierResponse
