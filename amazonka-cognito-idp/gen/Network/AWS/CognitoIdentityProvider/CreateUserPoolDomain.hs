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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain for a user pool.
module Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain
  ( -- * Creating a Request
    CreateUserPoolDomain (..),
    newCreateUserPoolDomain,

    -- * Request Lenses
    createUserPoolDomain_customDomainConfig,
    createUserPoolDomain_domain,
    createUserPoolDomain_userPoolId,

    -- * Destructuring the Response
    CreateUserPoolDomainResponse (..),
    newCreateUserPoolDomainResponse,

    -- * Response Lenses
    createUserPoolDomainResponse_cloudFrontDomain,
    createUserPoolDomainResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUserPoolDomain' smart constructor.
data CreateUserPoolDomain = CreateUserPoolDomain'
  { -- | The configuration for a custom domain that hosts the sign-up and sign-in
    -- webpages for your application.
    --
    -- Provide this parameter only if you want to use a custom domain for your
    -- user pool. Otherwise, you can exclude this parameter and use the Amazon
    -- Cognito hosted domain instead.
    --
    -- For more information about the hosted domain and custom domains, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-assign-domain.html Configuring a User Pool Domain>.
    customDomainConfig :: Prelude.Maybe CustomDomainConfigType,
    -- | The domain string.
    domain :: Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateUserPoolDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customDomainConfig', 'createUserPoolDomain_customDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in
-- webpages for your application.
--
-- Provide this parameter only if you want to use a custom domain for your
-- user pool. Otherwise, you can exclude this parameter and use the Amazon
-- Cognito hosted domain instead.
--
-- For more information about the hosted domain and custom domains, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-assign-domain.html Configuring a User Pool Domain>.
--
-- 'domain', 'createUserPoolDomain_domain' - The domain string.
--
-- 'userPoolId', 'createUserPoolDomain_userPoolId' - The user pool ID.
newCreateUserPoolDomain ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'userPoolId'
  Prelude.Text ->
  CreateUserPoolDomain
newCreateUserPoolDomain pDomain_ pUserPoolId_ =
  CreateUserPoolDomain'
    { customDomainConfig =
        Prelude.Nothing,
      domain = pDomain_,
      userPoolId = pUserPoolId_
    }

-- | The configuration for a custom domain that hosts the sign-up and sign-in
-- webpages for your application.
--
-- Provide this parameter only if you want to use a custom domain for your
-- user pool. Otherwise, you can exclude this parameter and use the Amazon
-- Cognito hosted domain instead.
--
-- For more information about the hosted domain and custom domains, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-assign-domain.html Configuring a User Pool Domain>.
createUserPoolDomain_customDomainConfig :: Lens.Lens' CreateUserPoolDomain (Prelude.Maybe CustomDomainConfigType)
createUserPoolDomain_customDomainConfig = Lens.lens (\CreateUserPoolDomain' {customDomainConfig} -> customDomainConfig) (\s@CreateUserPoolDomain' {} a -> s {customDomainConfig = a} :: CreateUserPoolDomain)

-- | The domain string.
createUserPoolDomain_domain :: Lens.Lens' CreateUserPoolDomain Prelude.Text
createUserPoolDomain_domain = Lens.lens (\CreateUserPoolDomain' {domain} -> domain) (\s@CreateUserPoolDomain' {} a -> s {domain = a} :: CreateUserPoolDomain)

-- | The user pool ID.
createUserPoolDomain_userPoolId :: Lens.Lens' CreateUserPoolDomain Prelude.Text
createUserPoolDomain_userPoolId = Lens.lens (\CreateUserPoolDomain' {userPoolId} -> userPoolId) (\s@CreateUserPoolDomain' {} a -> s {userPoolId = a} :: CreateUserPoolDomain)

instance Prelude.AWSRequest CreateUserPoolDomain where
  type
    Rs CreateUserPoolDomain =
      CreateUserPoolDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserPoolDomainResponse'
            Prelude.<$> (x Prelude..?> "CloudFrontDomain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserPoolDomain

instance Prelude.NFData CreateUserPoolDomain

instance Prelude.ToHeaders CreateUserPoolDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.CreateUserPoolDomain" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateUserPoolDomain where
  toJSON CreateUserPoolDomain' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CustomDomainConfig" Prelude..=)
              Prelude.<$> customDomainConfig,
            Prelude.Just ("Domain" Prelude..= domain),
            Prelude.Just ("UserPoolId" Prelude..= userPoolId)
          ]
      )

instance Prelude.ToPath CreateUserPoolDomain where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateUserPoolDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserPoolDomainResponse' smart constructor.
data CreateUserPoolDomainResponse = CreateUserPoolDomainResponse'
  { -- | The Amazon CloudFront endpoint that you use as the target of the alias
    -- that you set up with your Domain Name Service (DNS) provider.
    cloudFrontDomain :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateUserPoolDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFrontDomain', 'createUserPoolDomainResponse_cloudFrontDomain' - The Amazon CloudFront endpoint that you use as the target of the alias
-- that you set up with your Domain Name Service (DNS) provider.
--
-- 'httpStatus', 'createUserPoolDomainResponse_httpStatus' - The response's http status code.
newCreateUserPoolDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserPoolDomainResponse
newCreateUserPoolDomainResponse pHttpStatus_ =
  CreateUserPoolDomainResponse'
    { cloudFrontDomain =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon CloudFront endpoint that you use as the target of the alias
-- that you set up with your Domain Name Service (DNS) provider.
createUserPoolDomainResponse_cloudFrontDomain :: Lens.Lens' CreateUserPoolDomainResponse (Prelude.Maybe Prelude.Text)
createUserPoolDomainResponse_cloudFrontDomain = Lens.lens (\CreateUserPoolDomainResponse' {cloudFrontDomain} -> cloudFrontDomain) (\s@CreateUserPoolDomainResponse' {} a -> s {cloudFrontDomain = a} :: CreateUserPoolDomainResponse)

-- | The response's http status code.
createUserPoolDomainResponse_httpStatus :: Lens.Lens' CreateUserPoolDomainResponse Prelude.Int
createUserPoolDomainResponse_httpStatus = Lens.lens (\CreateUserPoolDomainResponse' {httpStatus} -> httpStatus) (\s@CreateUserPoolDomainResponse' {} a -> s {httpStatus = a} :: CreateUserPoolDomainResponse)

instance Prelude.NFData CreateUserPoolDomainResponse
