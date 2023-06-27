{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.HealthLake.Types.IdentityProviderConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.IdentityProviderConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types.AuthorizationStrategy
import qualified Amazonka.Prelude as Prelude

-- | The identity provider configuration that you gave when the Data Store
-- was created.
--
-- /See:/ 'newIdentityProviderConfiguration' smart constructor.
data IdentityProviderConfiguration = IdentityProviderConfiguration'
  { -- | If you enabled fine-grained authorization when you created the Data
    -- Store.
    fineGrainedAuthorizationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Lambda function that you want to
    -- use to decode the access token created by the authorization server.
    idpLambdaArn :: Prelude.Maybe Prelude.Text,
    -- | The JSON metadata elements that you want to use in your identity
    -- provider configuration. Required elements are listed based on the launch
    -- specification of the SMART application. For more information on all
    -- possible elements, see
    -- <https://build.fhir.org/ig/HL7/smart-app-launch/conformance.html#metadata Metadata>
    -- in SMART\'s App Launch specification.
    --
    -- @authorization_endpoint@: The URL to the OAuth2 authorization endpoint.
    --
    -- @grant_types_supported@: An array of grant types that are supported at
    -- the token endpoint. You must provide at least one grant type option.
    -- Valid options are @authorization_code@ and @client_credentials@.
    --
    -- @token_endpoint@: The URL to the OAuth2 token endpoint.
    --
    -- @capabilities@: An array of strings of the SMART capabilities that the
    -- authorization server supports.
    --
    -- @code_challenge_methods_supported@: An array of strings of supported
    -- PKCE code challenge methods. You must include the @S256@ method in the
    -- array of PKCE code challenge methods.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | The authorization strategy that you selected when you created the Data
    -- Store.
    authorizationStrategy :: AuthorizationStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProviderConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fineGrainedAuthorizationEnabled', 'identityProviderConfiguration_fineGrainedAuthorizationEnabled' - If you enabled fine-grained authorization when you created the Data
-- Store.
--
-- 'idpLambdaArn', 'identityProviderConfiguration_idpLambdaArn' - The Amazon Resource Name (ARN) of the Lambda function that you want to
-- use to decode the access token created by the authorization server.
--
-- 'metadata', 'identityProviderConfiguration_metadata' - The JSON metadata elements that you want to use in your identity
-- provider configuration. Required elements are listed based on the launch
-- specification of the SMART application. For more information on all
-- possible elements, see
-- <https://build.fhir.org/ig/HL7/smart-app-launch/conformance.html#metadata Metadata>
-- in SMART\'s App Launch specification.
--
-- @authorization_endpoint@: The URL to the OAuth2 authorization endpoint.
--
-- @grant_types_supported@: An array of grant types that are supported at
-- the token endpoint. You must provide at least one grant type option.
-- Valid options are @authorization_code@ and @client_credentials@.
--
-- @token_endpoint@: The URL to the OAuth2 token endpoint.
--
-- @capabilities@: An array of strings of the SMART capabilities that the
-- authorization server supports.
--
-- @code_challenge_methods_supported@: An array of strings of supported
-- PKCE code challenge methods. You must include the @S256@ method in the
-- array of PKCE code challenge methods.
--
-- 'authorizationStrategy', 'identityProviderConfiguration_authorizationStrategy' - The authorization strategy that you selected when you created the Data
-- Store.
newIdentityProviderConfiguration ::
  -- | 'authorizationStrategy'
  AuthorizationStrategy ->
  IdentityProviderConfiguration
newIdentityProviderConfiguration
  pAuthorizationStrategy_ =
    IdentityProviderConfiguration'
      { fineGrainedAuthorizationEnabled =
          Prelude.Nothing,
        idpLambdaArn = Prelude.Nothing,
        metadata = Prelude.Nothing,
        authorizationStrategy =
          pAuthorizationStrategy_
      }

-- | If you enabled fine-grained authorization when you created the Data
-- Store.
identityProviderConfiguration_fineGrainedAuthorizationEnabled :: Lens.Lens' IdentityProviderConfiguration (Prelude.Maybe Prelude.Bool)
identityProviderConfiguration_fineGrainedAuthorizationEnabled = Lens.lens (\IdentityProviderConfiguration' {fineGrainedAuthorizationEnabled} -> fineGrainedAuthorizationEnabled) (\s@IdentityProviderConfiguration' {} a -> s {fineGrainedAuthorizationEnabled = a} :: IdentityProviderConfiguration)

-- | The Amazon Resource Name (ARN) of the Lambda function that you want to
-- use to decode the access token created by the authorization server.
identityProviderConfiguration_idpLambdaArn :: Lens.Lens' IdentityProviderConfiguration (Prelude.Maybe Prelude.Text)
identityProviderConfiguration_idpLambdaArn = Lens.lens (\IdentityProviderConfiguration' {idpLambdaArn} -> idpLambdaArn) (\s@IdentityProviderConfiguration' {} a -> s {idpLambdaArn = a} :: IdentityProviderConfiguration)

-- | The JSON metadata elements that you want to use in your identity
-- provider configuration. Required elements are listed based on the launch
-- specification of the SMART application. For more information on all
-- possible elements, see
-- <https://build.fhir.org/ig/HL7/smart-app-launch/conformance.html#metadata Metadata>
-- in SMART\'s App Launch specification.
--
-- @authorization_endpoint@: The URL to the OAuth2 authorization endpoint.
--
-- @grant_types_supported@: An array of grant types that are supported at
-- the token endpoint. You must provide at least one grant type option.
-- Valid options are @authorization_code@ and @client_credentials@.
--
-- @token_endpoint@: The URL to the OAuth2 token endpoint.
--
-- @capabilities@: An array of strings of the SMART capabilities that the
-- authorization server supports.
--
-- @code_challenge_methods_supported@: An array of strings of supported
-- PKCE code challenge methods. You must include the @S256@ method in the
-- array of PKCE code challenge methods.
identityProviderConfiguration_metadata :: Lens.Lens' IdentityProviderConfiguration (Prelude.Maybe Prelude.Text)
identityProviderConfiguration_metadata = Lens.lens (\IdentityProviderConfiguration' {metadata} -> metadata) (\s@IdentityProviderConfiguration' {} a -> s {metadata = a} :: IdentityProviderConfiguration)

-- | The authorization strategy that you selected when you created the Data
-- Store.
identityProviderConfiguration_authorizationStrategy :: Lens.Lens' IdentityProviderConfiguration AuthorizationStrategy
identityProviderConfiguration_authorizationStrategy = Lens.lens (\IdentityProviderConfiguration' {authorizationStrategy} -> authorizationStrategy) (\s@IdentityProviderConfiguration' {} a -> s {authorizationStrategy = a} :: IdentityProviderConfiguration)

instance Data.FromJSON IdentityProviderConfiguration where
  parseJSON =
    Data.withObject
      "IdentityProviderConfiguration"
      ( \x ->
          IdentityProviderConfiguration'
            Prelude.<$> (x Data..:? "FineGrainedAuthorizationEnabled")
            Prelude.<*> (x Data..:? "IdpLambdaArn")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..: "AuthorizationStrategy")
      )

instance
  Prelude.Hashable
    IdentityProviderConfiguration
  where
  hashWithSalt _salt IdentityProviderConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` fineGrainedAuthorizationEnabled
      `Prelude.hashWithSalt` idpLambdaArn
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` authorizationStrategy

instance Prelude.NFData IdentityProviderConfiguration where
  rnf IdentityProviderConfiguration' {..} =
    Prelude.rnf fineGrainedAuthorizationEnabled
      `Prelude.seq` Prelude.rnf idpLambdaArn
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf authorizationStrategy

instance Data.ToJSON IdentityProviderConfiguration where
  toJSON IdentityProviderConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FineGrainedAuthorizationEnabled" Data..=)
              Prelude.<$> fineGrainedAuthorizationEnabled,
            ("IdpLambdaArn" Data..=) Prelude.<$> idpLambdaArn,
            ("Metadata" Data..=) Prelude.<$> metadata,
            Prelude.Just
              ( "AuthorizationStrategy"
                  Data..= authorizationStrategy
              )
          ]
      )
