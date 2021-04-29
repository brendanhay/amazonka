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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetIdentityProviderByIdentifier' smart constructor.
data GetIdentityProviderByIdentifier = GetIdentityProviderByIdentifier'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The identity provider ID.
    idpIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'idpIdentifier'
  Prelude.Text ->
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
getIdentityProviderByIdentifier_userPoolId :: Lens.Lens' GetIdentityProviderByIdentifier Prelude.Text
getIdentityProviderByIdentifier_userPoolId = Lens.lens (\GetIdentityProviderByIdentifier' {userPoolId} -> userPoolId) (\s@GetIdentityProviderByIdentifier' {} a -> s {userPoolId = a} :: GetIdentityProviderByIdentifier)

-- | The identity provider ID.
getIdentityProviderByIdentifier_idpIdentifier :: Lens.Lens' GetIdentityProviderByIdentifier Prelude.Text
getIdentityProviderByIdentifier_idpIdentifier = Lens.lens (\GetIdentityProviderByIdentifier' {idpIdentifier} -> idpIdentifier) (\s@GetIdentityProviderByIdentifier' {} a -> s {idpIdentifier = a} :: GetIdentityProviderByIdentifier)

instance
  Prelude.AWSRequest
    GetIdentityProviderByIdentifier
  where
  type
    Rs GetIdentityProviderByIdentifier =
      GetIdentityProviderByIdentifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentityProviderByIdentifierResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "IdentityProvider")
      )

instance
  Prelude.Hashable
    GetIdentityProviderByIdentifier

instance
  Prelude.NFData
    GetIdentityProviderByIdentifier

instance
  Prelude.ToHeaders
    GetIdentityProviderByIdentifier
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.GetIdentityProviderByIdentifier" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    GetIdentityProviderByIdentifier
  where
  toJSON GetIdentityProviderByIdentifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just
              ("IdpIdentifier" Prelude..= idpIdentifier)
          ]
      )

instance
  Prelude.ToPath
    GetIdentityProviderByIdentifier
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    GetIdentityProviderByIdentifier
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIdentityProviderByIdentifierResponse' smart constructor.
data GetIdentityProviderByIdentifierResponse = GetIdentityProviderByIdentifierResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identity provider object.
    identityProvider :: IdentityProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
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
getIdentityProviderByIdentifierResponse_httpStatus :: Lens.Lens' GetIdentityProviderByIdentifierResponse Prelude.Int
getIdentityProviderByIdentifierResponse_httpStatus = Lens.lens (\GetIdentityProviderByIdentifierResponse' {httpStatus} -> httpStatus) (\s@GetIdentityProviderByIdentifierResponse' {} a -> s {httpStatus = a} :: GetIdentityProviderByIdentifierResponse)

-- | The identity provider object.
getIdentityProviderByIdentifierResponse_identityProvider :: Lens.Lens' GetIdentityProviderByIdentifierResponse IdentityProviderType
getIdentityProviderByIdentifierResponse_identityProvider = Lens.lens (\GetIdentityProviderByIdentifierResponse' {identityProvider} -> identityProvider) (\s@GetIdentityProviderByIdentifierResponse' {} a -> s {identityProvider = a} :: GetIdentityProviderByIdentifierResponse)

instance
  Prelude.NFData
    GetIdentityProviderByIdentifierResponse
