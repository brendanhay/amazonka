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
-- Module      : Network.AWS.CognitoIdentity.GetCredentialsForIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns credentials for the provided identity ID. Any provided logins
-- will be validated against supported login providers. If the token is for
-- cognito-identity.amazonaws.com, it will be passed through to AWS
-- Security Token Service with the appropriate role for the token.
--
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.GetCredentialsForIdentity
  ( -- * Creating a Request
    GetCredentialsForIdentity (..),
    newGetCredentialsForIdentity,

    -- * Request Lenses
    getCredentialsForIdentity_logins,
    getCredentialsForIdentity_customRoleArn,
    getCredentialsForIdentity_identityId,

    -- * Destructuring the Response
    GetCredentialsForIdentityResponse (..),
    newGetCredentialsForIdentityResponse,

    -- * Response Lenses
    getCredentialsForIdentityResponse_identityId,
    getCredentialsForIdentityResponse_credentials,
    getCredentialsForIdentityResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @GetCredentialsForIdentity@ action.
--
-- /See:/ 'newGetCredentialsForIdentity' smart constructor.
data GetCredentialsForIdentity = GetCredentialsForIdentity'
  { -- | A set of optional name-value pairs that map provider names to provider
    -- tokens. The name-value pair will follow the syntax \"provider_name\":
    -- \"provider_user_identifier\".
    --
    -- Logins should not be specified when trying to get credentials for an
    -- unauthenticated identity.
    --
    -- The Logins parameter is required when using identities associated with
    -- external identity providers such as Facebook. For examples of @Logins@
    -- maps, see the code examples in the
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/external-identity-providers.html External Identity Providers>
    -- section of the Amazon Cognito Developer Guide.
    logins :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the role to be assumed when multiple
    -- roles were received in the token from the identity provider. For
    -- example, a SAML-based identity provider. This parameter is optional for
    -- identity providers that do not support role customization.
    customRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCredentialsForIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logins', 'getCredentialsForIdentity_logins' - A set of optional name-value pairs that map provider names to provider
-- tokens. The name-value pair will follow the syntax \"provider_name\":
-- \"provider_user_identifier\".
--
-- Logins should not be specified when trying to get credentials for an
-- unauthenticated identity.
--
-- The Logins parameter is required when using identities associated with
-- external identity providers such as Facebook. For examples of @Logins@
-- maps, see the code examples in the
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/external-identity-providers.html External Identity Providers>
-- section of the Amazon Cognito Developer Guide.
--
-- 'customRoleArn', 'getCredentialsForIdentity_customRoleArn' - The Amazon Resource Name (ARN) of the role to be assumed when multiple
-- roles were received in the token from the identity provider. For
-- example, a SAML-based identity provider. This parameter is optional for
-- identity providers that do not support role customization.
--
-- 'identityId', 'getCredentialsForIdentity_identityId' - A unique identifier in the format REGION:GUID.
newGetCredentialsForIdentity ::
  -- | 'identityId'
  Prelude.Text ->
  GetCredentialsForIdentity
newGetCredentialsForIdentity pIdentityId_ =
  GetCredentialsForIdentity'
    { logins =
        Prelude.Nothing,
      customRoleArn = Prelude.Nothing,
      identityId = pIdentityId_
    }

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. The name-value pair will follow the syntax \"provider_name\":
-- \"provider_user_identifier\".
--
-- Logins should not be specified when trying to get credentials for an
-- unauthenticated identity.
--
-- The Logins parameter is required when using identities associated with
-- external identity providers such as Facebook. For examples of @Logins@
-- maps, see the code examples in the
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/external-identity-providers.html External Identity Providers>
-- section of the Amazon Cognito Developer Guide.
getCredentialsForIdentity_logins :: Lens.Lens' GetCredentialsForIdentity (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCredentialsForIdentity_logins = Lens.lens (\GetCredentialsForIdentity' {logins} -> logins) (\s@GetCredentialsForIdentity' {} a -> s {logins = a} :: GetCredentialsForIdentity) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the role to be assumed when multiple
-- roles were received in the token from the identity provider. For
-- example, a SAML-based identity provider. This parameter is optional for
-- identity providers that do not support role customization.
getCredentialsForIdentity_customRoleArn :: Lens.Lens' GetCredentialsForIdentity (Prelude.Maybe Prelude.Text)
getCredentialsForIdentity_customRoleArn = Lens.lens (\GetCredentialsForIdentity' {customRoleArn} -> customRoleArn) (\s@GetCredentialsForIdentity' {} a -> s {customRoleArn = a} :: GetCredentialsForIdentity)

-- | A unique identifier in the format REGION:GUID.
getCredentialsForIdentity_identityId :: Lens.Lens' GetCredentialsForIdentity Prelude.Text
getCredentialsForIdentity_identityId = Lens.lens (\GetCredentialsForIdentity' {identityId} -> identityId) (\s@GetCredentialsForIdentity' {} a -> s {identityId = a} :: GetCredentialsForIdentity)

instance Prelude.AWSRequest GetCredentialsForIdentity where
  type
    Rs GetCredentialsForIdentity =
      GetCredentialsForIdentityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCredentialsForIdentityResponse'
            Prelude.<$> (x Prelude..?> "IdentityId")
            Prelude.<*> (x Prelude..?> "Credentials")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCredentialsForIdentity

instance Prelude.NFData GetCredentialsForIdentity

instance Prelude.ToHeaders GetCredentialsForIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityService.GetCredentialsForIdentity" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetCredentialsForIdentity where
  toJSON GetCredentialsForIdentity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Logins" Prelude..=) Prelude.<$> logins,
            ("CustomRoleArn" Prelude..=)
              Prelude.<$> customRoleArn,
            Prelude.Just ("IdentityId" Prelude..= identityId)
          ]
      )

instance Prelude.ToPath GetCredentialsForIdentity where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCredentialsForIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | Returned in response to a successful @GetCredentialsForIdentity@
-- operation.
--
-- /See:/ 'newGetCredentialsForIdentityResponse' smart constructor.
data GetCredentialsForIdentityResponse = GetCredentialsForIdentityResponse'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | Credentials for the provided identity ID.
    credentials :: Prelude.Maybe Credentials,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCredentialsForIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'getCredentialsForIdentityResponse_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'credentials', 'getCredentialsForIdentityResponse_credentials' - Credentials for the provided identity ID.
--
-- 'httpStatus', 'getCredentialsForIdentityResponse_httpStatus' - The response's http status code.
newGetCredentialsForIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCredentialsForIdentityResponse
newGetCredentialsForIdentityResponse pHttpStatus_ =
  GetCredentialsForIdentityResponse'
    { identityId =
        Prelude.Nothing,
      credentials = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier in the format REGION:GUID.
getCredentialsForIdentityResponse_identityId :: Lens.Lens' GetCredentialsForIdentityResponse (Prelude.Maybe Prelude.Text)
getCredentialsForIdentityResponse_identityId = Lens.lens (\GetCredentialsForIdentityResponse' {identityId} -> identityId) (\s@GetCredentialsForIdentityResponse' {} a -> s {identityId = a} :: GetCredentialsForIdentityResponse)

-- | Credentials for the provided identity ID.
getCredentialsForIdentityResponse_credentials :: Lens.Lens' GetCredentialsForIdentityResponse (Prelude.Maybe Credentials)
getCredentialsForIdentityResponse_credentials = Lens.lens (\GetCredentialsForIdentityResponse' {credentials} -> credentials) (\s@GetCredentialsForIdentityResponse' {} a -> s {credentials = a} :: GetCredentialsForIdentityResponse)

-- | The response's http status code.
getCredentialsForIdentityResponse_httpStatus :: Lens.Lens' GetCredentialsForIdentityResponse Prelude.Int
getCredentialsForIdentityResponse_httpStatus = Lens.lens (\GetCredentialsForIdentityResponse' {httpStatus} -> httpStatus) (\s@GetCredentialsForIdentityResponse' {} a -> s {httpStatus = a} :: GetCredentialsForIdentityResponse)

instance
  Prelude.NFData
    GetCredentialsForIdentityResponse
