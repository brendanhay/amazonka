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
-- Module      : Amazonka.CognitoIdentityProvider.CreateUserPoolDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain for a user pool.
module Amazonka.CognitoIdentityProvider.CreateUserPoolDomain
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- | The domain string. For custom domains, this is the fully-qualified
    -- domain name, such as @auth.example.com@. For Amazon Cognito prefix
    -- domains, this is the prefix alone, such as @auth@.
    domain :: Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'domain', 'createUserPoolDomain_domain' - The domain string. For custom domains, this is the fully-qualified
-- domain name, such as @auth.example.com@. For Amazon Cognito prefix
-- domains, this is the prefix alone, such as @auth@.
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

-- | The domain string. For custom domains, this is the fully-qualified
-- domain name, such as @auth.example.com@. For Amazon Cognito prefix
-- domains, this is the prefix alone, such as @auth@.
createUserPoolDomain_domain :: Lens.Lens' CreateUserPoolDomain Prelude.Text
createUserPoolDomain_domain = Lens.lens (\CreateUserPoolDomain' {domain} -> domain) (\s@CreateUserPoolDomain' {} a -> s {domain = a} :: CreateUserPoolDomain)

-- | The user pool ID.
createUserPoolDomain_userPoolId :: Lens.Lens' CreateUserPoolDomain Prelude.Text
createUserPoolDomain_userPoolId = Lens.lens (\CreateUserPoolDomain' {userPoolId} -> userPoolId) (\s@CreateUserPoolDomain' {} a -> s {userPoolId = a} :: CreateUserPoolDomain)

instance Core.AWSRequest CreateUserPoolDomain where
  type
    AWSResponse CreateUserPoolDomain =
      CreateUserPoolDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserPoolDomainResponse'
            Prelude.<$> (x Data..?> "CloudFrontDomain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserPoolDomain where
  hashWithSalt _salt CreateUserPoolDomain' {..} =
    _salt `Prelude.hashWithSalt` customDomainConfig
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData CreateUserPoolDomain where
  rnf CreateUserPoolDomain' {..} =
    Prelude.rnf customDomainConfig
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders CreateUserPoolDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.CreateUserPoolDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUserPoolDomain where
  toJSON CreateUserPoolDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomDomainConfig" Data..=)
              Prelude.<$> customDomainConfig,
            Prelude.Just ("Domain" Data..= domain),
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath CreateUserPoolDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUserPoolDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserPoolDomainResponse' smart constructor.
data CreateUserPoolDomainResponse = CreateUserPoolDomainResponse'
  { -- | The Amazon CloudFront endpoint that you use as the target of the alias
    -- that you set up with your Domain Name Service (DNS) provider.
    cloudFrontDomain :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateUserPoolDomainResponse where
  rnf CreateUserPoolDomainResponse' {..} =
    Prelude.rnf cloudFrontDomain
      `Prelude.seq` Prelude.rnf httpStatus
