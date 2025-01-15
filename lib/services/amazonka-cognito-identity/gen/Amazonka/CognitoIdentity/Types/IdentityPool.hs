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
-- Module      : Amazonka.CognitoIdentity.Types.IdentityPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types.IdentityPool where

import Amazonka.CognitoIdentity.Types.CognitoIdentityProvider
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Amazon Cognito identity pool.
--
-- /See:/ 'newIdentityPool' smart constructor.
data IdentityPool = IdentityPool'
  { -- | Enables or disables the Basic (Classic) authentication flow. For more
    -- information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow>
    -- in the /Amazon Cognito Developer Guide/.
    allowClassicFlow :: Prelude.Maybe Prelude.Bool,
    -- | A list representing an Amazon Cognito user pool and its client ID.
    cognitoIdentityProviders :: Prelude.Maybe [CognitoIdentityProvider],
    -- | The \"domain\" by which Cognito will refer to your users.
    developerProviderName :: Prelude.Maybe Prelude.Text,
    -- | The tags that are assigned to the identity pool. A tag is a label that
    -- you can apply to identity pools to categorize and manage them in
    -- different ways, such as by purpose, owner, environment, or other
    -- criteria.
    identityPoolTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARNs of the OpenID Connect providers.
    openIdConnectProviderARNs :: Prelude.Maybe [Prelude.Text],
    -- | An array of Amazon Resource Names (ARNs) of the SAML provider for your
    -- identity pool.
    samlProviderARNs :: Prelude.Maybe [Prelude.Text],
    -- | Optional key:value pairs mapping provider names to provider app IDs.
    supportedLoginProviders :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text,
    -- | A string that you provide.
    identityPoolName :: Prelude.Text,
    -- | TRUE if the identity pool supports unauthenticated logins.
    allowUnauthenticatedIdentities :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowClassicFlow', 'identityPool_allowClassicFlow' - Enables or disables the Basic (Classic) authentication flow. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow>
-- in the /Amazon Cognito Developer Guide/.
--
-- 'cognitoIdentityProviders', 'identityPool_cognitoIdentityProviders' - A list representing an Amazon Cognito user pool and its client ID.
--
-- 'developerProviderName', 'identityPool_developerProviderName' - The \"domain\" by which Cognito will refer to your users.
--
-- 'identityPoolTags', 'identityPool_identityPoolTags' - The tags that are assigned to the identity pool. A tag is a label that
-- you can apply to identity pools to categorize and manage them in
-- different ways, such as by purpose, owner, environment, or other
-- criteria.
--
-- 'openIdConnectProviderARNs', 'identityPool_openIdConnectProviderARNs' - The ARNs of the OpenID Connect providers.
--
-- 'samlProviderARNs', 'identityPool_samlProviderARNs' - An array of Amazon Resource Names (ARNs) of the SAML provider for your
-- identity pool.
--
-- 'supportedLoginProviders', 'identityPool_supportedLoginProviders' - Optional key:value pairs mapping provider names to provider app IDs.
--
-- 'identityPoolId', 'identityPool_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'identityPoolName', 'identityPool_identityPoolName' - A string that you provide.
--
-- 'allowUnauthenticatedIdentities', 'identityPool_allowUnauthenticatedIdentities' - TRUE if the identity pool supports unauthenticated logins.
newIdentityPool ::
  -- | 'identityPoolId'
  Prelude.Text ->
  -- | 'identityPoolName'
  Prelude.Text ->
  -- | 'allowUnauthenticatedIdentities'
  Prelude.Bool ->
  IdentityPool
newIdentityPool
  pIdentityPoolId_
  pIdentityPoolName_
  pAllowUnauthenticatedIdentities_ =
    IdentityPool'
      { allowClassicFlow = Prelude.Nothing,
        cognitoIdentityProviders = Prelude.Nothing,
        developerProviderName = Prelude.Nothing,
        identityPoolTags = Prelude.Nothing,
        openIdConnectProviderARNs = Prelude.Nothing,
        samlProviderARNs = Prelude.Nothing,
        supportedLoginProviders = Prelude.Nothing,
        identityPoolId = pIdentityPoolId_,
        identityPoolName = pIdentityPoolName_,
        allowUnauthenticatedIdentities =
          pAllowUnauthenticatedIdentities_
      }

-- | Enables or disables the Basic (Classic) authentication flow. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow>
-- in the /Amazon Cognito Developer Guide/.
identityPool_allowClassicFlow :: Lens.Lens' IdentityPool (Prelude.Maybe Prelude.Bool)
identityPool_allowClassicFlow = Lens.lens (\IdentityPool' {allowClassicFlow} -> allowClassicFlow) (\s@IdentityPool' {} a -> s {allowClassicFlow = a} :: IdentityPool)

-- | A list representing an Amazon Cognito user pool and its client ID.
identityPool_cognitoIdentityProviders :: Lens.Lens' IdentityPool (Prelude.Maybe [CognitoIdentityProvider])
identityPool_cognitoIdentityProviders = Lens.lens (\IdentityPool' {cognitoIdentityProviders} -> cognitoIdentityProviders) (\s@IdentityPool' {} a -> s {cognitoIdentityProviders = a} :: IdentityPool) Prelude.. Lens.mapping Lens.coerced

-- | The \"domain\" by which Cognito will refer to your users.
identityPool_developerProviderName :: Lens.Lens' IdentityPool (Prelude.Maybe Prelude.Text)
identityPool_developerProviderName = Lens.lens (\IdentityPool' {developerProviderName} -> developerProviderName) (\s@IdentityPool' {} a -> s {developerProviderName = a} :: IdentityPool)

-- | The tags that are assigned to the identity pool. A tag is a label that
-- you can apply to identity pools to categorize and manage them in
-- different ways, such as by purpose, owner, environment, or other
-- criteria.
identityPool_identityPoolTags :: Lens.Lens' IdentityPool (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
identityPool_identityPoolTags = Lens.lens (\IdentityPool' {identityPoolTags} -> identityPoolTags) (\s@IdentityPool' {} a -> s {identityPoolTags = a} :: IdentityPool) Prelude.. Lens.mapping Lens.coerced

-- | The ARNs of the OpenID Connect providers.
identityPool_openIdConnectProviderARNs :: Lens.Lens' IdentityPool (Prelude.Maybe [Prelude.Text])
identityPool_openIdConnectProviderARNs = Lens.lens (\IdentityPool' {openIdConnectProviderARNs} -> openIdConnectProviderARNs) (\s@IdentityPool' {} a -> s {openIdConnectProviderARNs = a} :: IdentityPool) Prelude.. Lens.mapping Lens.coerced

-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your
-- identity pool.
identityPool_samlProviderARNs :: Lens.Lens' IdentityPool (Prelude.Maybe [Prelude.Text])
identityPool_samlProviderARNs = Lens.lens (\IdentityPool' {samlProviderARNs} -> samlProviderARNs) (\s@IdentityPool' {} a -> s {samlProviderARNs = a} :: IdentityPool) Prelude.. Lens.mapping Lens.coerced

-- | Optional key:value pairs mapping provider names to provider app IDs.
identityPool_supportedLoginProviders :: Lens.Lens' IdentityPool (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
identityPool_supportedLoginProviders = Lens.lens (\IdentityPool' {supportedLoginProviders} -> supportedLoginProviders) (\s@IdentityPool' {} a -> s {supportedLoginProviders = a} :: IdentityPool) Prelude.. Lens.mapping Lens.coerced

-- | An identity pool ID in the format REGION:GUID.
identityPool_identityPoolId :: Lens.Lens' IdentityPool Prelude.Text
identityPool_identityPoolId = Lens.lens (\IdentityPool' {identityPoolId} -> identityPoolId) (\s@IdentityPool' {} a -> s {identityPoolId = a} :: IdentityPool)

-- | A string that you provide.
identityPool_identityPoolName :: Lens.Lens' IdentityPool Prelude.Text
identityPool_identityPoolName = Lens.lens (\IdentityPool' {identityPoolName} -> identityPoolName) (\s@IdentityPool' {} a -> s {identityPoolName = a} :: IdentityPool)

-- | TRUE if the identity pool supports unauthenticated logins.
identityPool_allowUnauthenticatedIdentities :: Lens.Lens' IdentityPool Prelude.Bool
identityPool_allowUnauthenticatedIdentities = Lens.lens (\IdentityPool' {allowUnauthenticatedIdentities} -> allowUnauthenticatedIdentities) (\s@IdentityPool' {} a -> s {allowUnauthenticatedIdentities = a} :: IdentityPool)

instance Data.FromJSON IdentityPool where
  parseJSON =
    Data.withObject
      "IdentityPool"
      ( \x ->
          IdentityPool'
            Prelude.<$> (x Data..:? "AllowClassicFlow")
            Prelude.<*> ( x
                            Data..:? "CognitoIdentityProviders"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DeveloperProviderName")
            Prelude.<*> ( x
                            Data..:? "IdentityPoolTags"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "OpenIdConnectProviderARNs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SamlProviderARNs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SupportedLoginProviders"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "IdentityPoolId")
            Prelude.<*> (x Data..: "IdentityPoolName")
            Prelude.<*> (x Data..: "AllowUnauthenticatedIdentities")
      )

instance Prelude.Hashable IdentityPool where
  hashWithSalt _salt IdentityPool' {..} =
    _salt
      `Prelude.hashWithSalt` allowClassicFlow
      `Prelude.hashWithSalt` cognitoIdentityProviders
      `Prelude.hashWithSalt` developerProviderName
      `Prelude.hashWithSalt` identityPoolTags
      `Prelude.hashWithSalt` openIdConnectProviderARNs
      `Prelude.hashWithSalt` samlProviderARNs
      `Prelude.hashWithSalt` supportedLoginProviders
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` identityPoolName
      `Prelude.hashWithSalt` allowUnauthenticatedIdentities

instance Prelude.NFData IdentityPool where
  rnf IdentityPool' {..} =
    Prelude.rnf allowClassicFlow `Prelude.seq`
      Prelude.rnf cognitoIdentityProviders `Prelude.seq`
        Prelude.rnf developerProviderName `Prelude.seq`
          Prelude.rnf identityPoolTags `Prelude.seq`
            Prelude.rnf openIdConnectProviderARNs `Prelude.seq`
              Prelude.rnf samlProviderARNs `Prelude.seq`
                Prelude.rnf supportedLoginProviders `Prelude.seq`
                  Prelude.rnf identityPoolId `Prelude.seq`
                    Prelude.rnf identityPoolName `Prelude.seq`
                      Prelude.rnf allowUnauthenticatedIdentities

instance Data.ToJSON IdentityPool where
  toJSON IdentityPool' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowClassicFlow" Data..=)
              Prelude.<$> allowClassicFlow,
            ("CognitoIdentityProviders" Data..=)
              Prelude.<$> cognitoIdentityProviders,
            ("DeveloperProviderName" Data..=)
              Prelude.<$> developerProviderName,
            ("IdentityPoolTags" Data..=)
              Prelude.<$> identityPoolTags,
            ("OpenIdConnectProviderARNs" Data..=)
              Prelude.<$> openIdConnectProviderARNs,
            ("SamlProviderARNs" Data..=)
              Prelude.<$> samlProviderARNs,
            ("SupportedLoginProviders" Data..=)
              Prelude.<$> supportedLoginProviders,
            Prelude.Just
              ("IdentityPoolId" Data..= identityPoolId),
            Prelude.Just
              ("IdentityPoolName" Data..= identityPoolName),
            Prelude.Just
              ( "AllowUnauthenticatedIdentities"
                  Data..= allowUnauthenticatedIdentities
              )
          ]
      )
