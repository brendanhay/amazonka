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
-- Module      : Network.AWS.CognitoIdentity.Types.IdentityPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.IdentityPool where

import Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing an Amazon Cognito identity pool.
--
-- /See:/ 'newIdentityPool' smart constructor.
data IdentityPool = IdentityPool'
  { -- | Enables or disables the Basic (Classic) authentication flow. For more
    -- information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow>
    -- in the /Amazon Cognito Developer Guide/.
    allowClassicFlow :: Core.Maybe Core.Bool,
    -- | An array of Amazon Resource Names (ARNs) of the SAML provider for your
    -- identity pool.
    samlProviderARNs :: Core.Maybe [Core.Text],
    -- | The tags that are assigned to the identity pool. A tag is a label that
    -- you can apply to identity pools to categorize and manage them in
    -- different ways, such as by purpose, owner, environment, or other
    -- criteria.
    identityPoolTags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The ARNs of the OpenID Connect providers.
    openIdConnectProviderARNs :: Core.Maybe [Core.Text],
    -- | Optional key:value pairs mapping provider names to provider app IDs.
    supportedLoginProviders :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A list representing an Amazon Cognito user pool and its client ID.
    cognitoIdentityProviders :: Core.Maybe [CognitoIdentityProvider],
    -- | The \"domain\" by which Cognito will refer to your users.
    developerProviderName :: Core.Maybe Core.Text,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Core.Text,
    -- | A string that you provide.
    identityPoolName :: Core.Text,
    -- | TRUE if the identity pool supports unauthenticated logins.
    allowUnauthenticatedIdentities :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'samlProviderARNs', 'identityPool_samlProviderARNs' - An array of Amazon Resource Names (ARNs) of the SAML provider for your
-- identity pool.
--
-- 'identityPoolTags', 'identityPool_identityPoolTags' - The tags that are assigned to the identity pool. A tag is a label that
-- you can apply to identity pools to categorize and manage them in
-- different ways, such as by purpose, owner, environment, or other
-- criteria.
--
-- 'openIdConnectProviderARNs', 'identityPool_openIdConnectProviderARNs' - The ARNs of the OpenID Connect providers.
--
-- 'supportedLoginProviders', 'identityPool_supportedLoginProviders' - Optional key:value pairs mapping provider names to provider app IDs.
--
-- 'cognitoIdentityProviders', 'identityPool_cognitoIdentityProviders' - A list representing an Amazon Cognito user pool and its client ID.
--
-- 'developerProviderName', 'identityPool_developerProviderName' - The \"domain\" by which Cognito will refer to your users.
--
-- 'identityPoolId', 'identityPool_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'identityPoolName', 'identityPool_identityPoolName' - A string that you provide.
--
-- 'allowUnauthenticatedIdentities', 'identityPool_allowUnauthenticatedIdentities' - TRUE if the identity pool supports unauthenticated logins.
newIdentityPool ::
  -- | 'identityPoolId'
  Core.Text ->
  -- | 'identityPoolName'
  Core.Text ->
  -- | 'allowUnauthenticatedIdentities'
  Core.Bool ->
  IdentityPool
newIdentityPool
  pIdentityPoolId_
  pIdentityPoolName_
  pAllowUnauthenticatedIdentities_ =
    IdentityPool'
      { allowClassicFlow = Core.Nothing,
        samlProviderARNs = Core.Nothing,
        identityPoolTags = Core.Nothing,
        openIdConnectProviderARNs = Core.Nothing,
        supportedLoginProviders = Core.Nothing,
        cognitoIdentityProviders = Core.Nothing,
        developerProviderName = Core.Nothing,
        identityPoolId = pIdentityPoolId_,
        identityPoolName = pIdentityPoolName_,
        allowUnauthenticatedIdentities =
          pAllowUnauthenticatedIdentities_
      }

-- | Enables or disables the Basic (Classic) authentication flow. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow>
-- in the /Amazon Cognito Developer Guide/.
identityPool_allowClassicFlow :: Lens.Lens' IdentityPool (Core.Maybe Core.Bool)
identityPool_allowClassicFlow = Lens.lens (\IdentityPool' {allowClassicFlow} -> allowClassicFlow) (\s@IdentityPool' {} a -> s {allowClassicFlow = a} :: IdentityPool)

-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your
-- identity pool.
identityPool_samlProviderARNs :: Lens.Lens' IdentityPool (Core.Maybe [Core.Text])
identityPool_samlProviderARNs = Lens.lens (\IdentityPool' {samlProviderARNs} -> samlProviderARNs) (\s@IdentityPool' {} a -> s {samlProviderARNs = a} :: IdentityPool) Core.. Lens.mapping Lens._Coerce

-- | The tags that are assigned to the identity pool. A tag is a label that
-- you can apply to identity pools to categorize and manage them in
-- different ways, such as by purpose, owner, environment, or other
-- criteria.
identityPool_identityPoolTags :: Lens.Lens' IdentityPool (Core.Maybe (Core.HashMap Core.Text Core.Text))
identityPool_identityPoolTags = Lens.lens (\IdentityPool' {identityPoolTags} -> identityPoolTags) (\s@IdentityPool' {} a -> s {identityPoolTags = a} :: IdentityPool) Core.. Lens.mapping Lens._Coerce

-- | The ARNs of the OpenID Connect providers.
identityPool_openIdConnectProviderARNs :: Lens.Lens' IdentityPool (Core.Maybe [Core.Text])
identityPool_openIdConnectProviderARNs = Lens.lens (\IdentityPool' {openIdConnectProviderARNs} -> openIdConnectProviderARNs) (\s@IdentityPool' {} a -> s {openIdConnectProviderARNs = a} :: IdentityPool) Core.. Lens.mapping Lens._Coerce

-- | Optional key:value pairs mapping provider names to provider app IDs.
identityPool_supportedLoginProviders :: Lens.Lens' IdentityPool (Core.Maybe (Core.HashMap Core.Text Core.Text))
identityPool_supportedLoginProviders = Lens.lens (\IdentityPool' {supportedLoginProviders} -> supportedLoginProviders) (\s@IdentityPool' {} a -> s {supportedLoginProviders = a} :: IdentityPool) Core.. Lens.mapping Lens._Coerce

-- | A list representing an Amazon Cognito user pool and its client ID.
identityPool_cognitoIdentityProviders :: Lens.Lens' IdentityPool (Core.Maybe [CognitoIdentityProvider])
identityPool_cognitoIdentityProviders = Lens.lens (\IdentityPool' {cognitoIdentityProviders} -> cognitoIdentityProviders) (\s@IdentityPool' {} a -> s {cognitoIdentityProviders = a} :: IdentityPool) Core.. Lens.mapping Lens._Coerce

-- | The \"domain\" by which Cognito will refer to your users.
identityPool_developerProviderName :: Lens.Lens' IdentityPool (Core.Maybe Core.Text)
identityPool_developerProviderName = Lens.lens (\IdentityPool' {developerProviderName} -> developerProviderName) (\s@IdentityPool' {} a -> s {developerProviderName = a} :: IdentityPool)

-- | An identity pool ID in the format REGION:GUID.
identityPool_identityPoolId :: Lens.Lens' IdentityPool Core.Text
identityPool_identityPoolId = Lens.lens (\IdentityPool' {identityPoolId} -> identityPoolId) (\s@IdentityPool' {} a -> s {identityPoolId = a} :: IdentityPool)

-- | A string that you provide.
identityPool_identityPoolName :: Lens.Lens' IdentityPool Core.Text
identityPool_identityPoolName = Lens.lens (\IdentityPool' {identityPoolName} -> identityPoolName) (\s@IdentityPool' {} a -> s {identityPoolName = a} :: IdentityPool)

-- | TRUE if the identity pool supports unauthenticated logins.
identityPool_allowUnauthenticatedIdentities :: Lens.Lens' IdentityPool Core.Bool
identityPool_allowUnauthenticatedIdentities = Lens.lens (\IdentityPool' {allowUnauthenticatedIdentities} -> allowUnauthenticatedIdentities) (\s@IdentityPool' {} a -> s {allowUnauthenticatedIdentities = a} :: IdentityPool)

instance Core.FromJSON IdentityPool where
  parseJSON =
    Core.withObject
      "IdentityPool"
      ( \x ->
          IdentityPool'
            Core.<$> (x Core..:? "AllowClassicFlow")
            Core.<*> (x Core..:? "SamlProviderARNs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "IdentityPoolTags" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "OpenIdConnectProviderARNs"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "SupportedLoginProviders"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "CognitoIdentityProviders"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "DeveloperProviderName")
            Core.<*> (x Core..: "IdentityPoolId")
            Core.<*> (x Core..: "IdentityPoolName")
            Core.<*> (x Core..: "AllowUnauthenticatedIdentities")
      )

instance Core.Hashable IdentityPool

instance Core.NFData IdentityPool

instance Core.ToJSON IdentityPool where
  toJSON IdentityPool' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowClassicFlow" Core..=)
              Core.<$> allowClassicFlow,
            ("SamlProviderARNs" Core..=)
              Core.<$> samlProviderARNs,
            ("IdentityPoolTags" Core..=)
              Core.<$> identityPoolTags,
            ("OpenIdConnectProviderARNs" Core..=)
              Core.<$> openIdConnectProviderARNs,
            ("SupportedLoginProviders" Core..=)
              Core.<$> supportedLoginProviders,
            ("CognitoIdentityProviders" Core..=)
              Core.<$> cognitoIdentityProviders,
            ("DeveloperProviderName" Core..=)
              Core.<$> developerProviderName,
            Core.Just ("IdentityPoolId" Core..= identityPoolId),
            Core.Just
              ("IdentityPoolName" Core..= identityPoolName),
            Core.Just
              ( "AllowUnauthenticatedIdentities"
                  Core..= allowUnauthenticatedIdentities
              )
          ]
      )
