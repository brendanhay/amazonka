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
-- Module      : Amazonka.CognitoIdentityProvider.UpdateUserPoolDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Secure Sockets Layer (SSL) certificate for the custom domain
-- for your user pool.
--
-- You can use this operation to provide the Amazon Resource Name (ARN) of
-- a new certificate to Amazon Cognito. You can\'t use it to change the
-- domain for a user pool.
--
-- A custom domain is used to host the Amazon Cognito hosted UI, which
-- provides sign-up and sign-in pages for your application. When you set up
-- a custom domain, you provide a certificate that you manage with
-- Certificate Manager (ACM). When necessary, you can use this operation to
-- change the certificate that you applied to your custom domain.
--
-- Usually, this is unnecessary following routine certificate renewal with
-- ACM. When you renew your existing certificate in ACM, the ARN for your
-- certificate remains the same, and your custom domain uses the new
-- certificate automatically.
--
-- However, if you replace your existing certificate with a new one, ACM
-- gives the new certificate a new ARN. To apply the new certificate to
-- your custom domain, you must provide this ARN to Amazon Cognito.
--
-- When you add your new certificate in ACM, you must choose US East (N.
-- Virginia) as the Amazon Web Services Region.
--
-- After you submit your request, Amazon Cognito requires up to 1 hour to
-- distribute your new certificate to your custom domain.
--
-- For more information about adding a custom domain to your user pool, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
module Amazonka.CognitoIdentityProvider.UpdateUserPoolDomain
  ( -- * Creating a Request
    UpdateUserPoolDomain (..),
    newUpdateUserPoolDomain,

    -- * Request Lenses
    updateUserPoolDomain_domain,
    updateUserPoolDomain_userPoolId,
    updateUserPoolDomain_customDomainConfig,

    -- * Destructuring the Response
    UpdateUserPoolDomainResponse (..),
    newUpdateUserPoolDomainResponse,

    -- * Response Lenses
    updateUserPoolDomainResponse_cloudFrontDomain,
    updateUserPoolDomainResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The UpdateUserPoolDomain request input.
--
-- /See:/ 'newUpdateUserPoolDomain' smart constructor.
data UpdateUserPoolDomain = UpdateUserPoolDomain'
  { -- | The domain name for the custom domain that hosts the sign-up and sign-in
    -- pages for your application. One example might be @auth.example.com@.
    --
    -- This string can include only lowercase letters, numbers, and hyphens.
    -- Don\'t use a hyphen for the first or last character. Use periods to
    -- separate subdomain names.
    domain :: Prelude.Text,
    -- | The ID of the user pool that is associated with the custom domain whose
    -- certificate you\'re updating.
    userPoolId :: Prelude.Text,
    -- | The configuration for a custom domain that hosts the sign-up and sign-in
    -- pages for your application. Use this object to specify an SSL
    -- certificate that is managed by ACM.
    customDomainConfig :: CustomDomainConfigType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserPoolDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'updateUserPoolDomain_domain' - The domain name for the custom domain that hosts the sign-up and sign-in
-- pages for your application. One example might be @auth.example.com@.
--
-- This string can include only lowercase letters, numbers, and hyphens.
-- Don\'t use a hyphen for the first or last character. Use periods to
-- separate subdomain names.
--
-- 'userPoolId', 'updateUserPoolDomain_userPoolId' - The ID of the user pool that is associated with the custom domain whose
-- certificate you\'re updating.
--
-- 'customDomainConfig', 'updateUserPoolDomain_customDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in
-- pages for your application. Use this object to specify an SSL
-- certificate that is managed by ACM.
newUpdateUserPoolDomain ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'customDomainConfig'
  CustomDomainConfigType ->
  UpdateUserPoolDomain
newUpdateUserPoolDomain
  pDomain_
  pUserPoolId_
  pCustomDomainConfig_ =
    UpdateUserPoolDomain'
      { domain = pDomain_,
        userPoolId = pUserPoolId_,
        customDomainConfig = pCustomDomainConfig_
      }

-- | The domain name for the custom domain that hosts the sign-up and sign-in
-- pages for your application. One example might be @auth.example.com@.
--
-- This string can include only lowercase letters, numbers, and hyphens.
-- Don\'t use a hyphen for the first or last character. Use periods to
-- separate subdomain names.
updateUserPoolDomain_domain :: Lens.Lens' UpdateUserPoolDomain Prelude.Text
updateUserPoolDomain_domain = Lens.lens (\UpdateUserPoolDomain' {domain} -> domain) (\s@UpdateUserPoolDomain' {} a -> s {domain = a} :: UpdateUserPoolDomain)

-- | The ID of the user pool that is associated with the custom domain whose
-- certificate you\'re updating.
updateUserPoolDomain_userPoolId :: Lens.Lens' UpdateUserPoolDomain Prelude.Text
updateUserPoolDomain_userPoolId = Lens.lens (\UpdateUserPoolDomain' {userPoolId} -> userPoolId) (\s@UpdateUserPoolDomain' {} a -> s {userPoolId = a} :: UpdateUserPoolDomain)

-- | The configuration for a custom domain that hosts the sign-up and sign-in
-- pages for your application. Use this object to specify an SSL
-- certificate that is managed by ACM.
updateUserPoolDomain_customDomainConfig :: Lens.Lens' UpdateUserPoolDomain CustomDomainConfigType
updateUserPoolDomain_customDomainConfig = Lens.lens (\UpdateUserPoolDomain' {customDomainConfig} -> customDomainConfig) (\s@UpdateUserPoolDomain' {} a -> s {customDomainConfig = a} :: UpdateUserPoolDomain)

instance Core.AWSRequest UpdateUserPoolDomain where
  type
    AWSResponse UpdateUserPoolDomain =
      UpdateUserPoolDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserPoolDomainResponse'
            Prelude.<$> (x Data..?> "CloudFrontDomain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUserPoolDomain where
  hashWithSalt _salt UpdateUserPoolDomain' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` customDomainConfig

instance Prelude.NFData UpdateUserPoolDomain where
  rnf UpdateUserPoolDomain' {..} =
    Prelude.rnf domain `Prelude.seq`
      Prelude.rnf userPoolId `Prelude.seq`
        Prelude.rnf customDomainConfig

instance Data.ToHeaders UpdateUserPoolDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.UpdateUserPoolDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUserPoolDomain where
  toJSON UpdateUserPoolDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Domain" Data..= domain),
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just
              ("CustomDomainConfig" Data..= customDomainConfig)
          ]
      )

instance Data.ToPath UpdateUserPoolDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateUserPoolDomain where
  toQuery = Prelude.const Prelude.mempty

-- | The UpdateUserPoolDomain response output.
--
-- /See:/ 'newUpdateUserPoolDomainResponse' smart constructor.
data UpdateUserPoolDomainResponse = UpdateUserPoolDomainResponse'
  { -- | The Amazon CloudFront endpoint that Amazon Cognito set up when you added
    -- the custom domain to your user pool.
    cloudFrontDomain :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserPoolDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFrontDomain', 'updateUserPoolDomainResponse_cloudFrontDomain' - The Amazon CloudFront endpoint that Amazon Cognito set up when you added
-- the custom domain to your user pool.
--
-- 'httpStatus', 'updateUserPoolDomainResponse_httpStatus' - The response's http status code.
newUpdateUserPoolDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUserPoolDomainResponse
newUpdateUserPoolDomainResponse pHttpStatus_ =
  UpdateUserPoolDomainResponse'
    { cloudFrontDomain =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon CloudFront endpoint that Amazon Cognito set up when you added
-- the custom domain to your user pool.
updateUserPoolDomainResponse_cloudFrontDomain :: Lens.Lens' UpdateUserPoolDomainResponse (Prelude.Maybe Prelude.Text)
updateUserPoolDomainResponse_cloudFrontDomain = Lens.lens (\UpdateUserPoolDomainResponse' {cloudFrontDomain} -> cloudFrontDomain) (\s@UpdateUserPoolDomainResponse' {} a -> s {cloudFrontDomain = a} :: UpdateUserPoolDomainResponse)

-- | The response's http status code.
updateUserPoolDomainResponse_httpStatus :: Lens.Lens' UpdateUserPoolDomainResponse Prelude.Int
updateUserPoolDomainResponse_httpStatus = Lens.lens (\UpdateUserPoolDomainResponse' {httpStatus} -> httpStatus) (\s@UpdateUserPoolDomainResponse' {} a -> s {httpStatus = a} :: UpdateUserPoolDomainResponse)

instance Prelude.NFData UpdateUserPoolDomainResponse where
  rnf UpdateUserPoolDomainResponse' {..} =
    Prelude.rnf cloudFrontDomain `Prelude.seq`
      Prelude.rnf httpStatus
