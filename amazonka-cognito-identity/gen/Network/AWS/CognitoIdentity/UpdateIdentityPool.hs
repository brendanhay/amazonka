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
-- Module      : Network.AWS.CognitoIdentity.UpdateIdentityPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an identity pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.UpdateIdentityPool
  ( -- * Creating a Request
    UpdateIdentityPool (..),
    newUpdateIdentityPool,

    -- * Request Lenses
    updateIdentityPool_allowClassicFlow,
    updateIdentityPool_samlProviderARNs,
    updateIdentityPool_identityPoolTags,
    updateIdentityPool_openIdConnectProviderARNs,
    updateIdentityPool_supportedLoginProviders,
    updateIdentityPool_cognitoIdentityProviders,
    updateIdentityPool_developerProviderName,
    updateIdentityPool_identityPoolId,
    updateIdentityPool_identityPoolName,
    updateIdentityPool_allowUnauthenticatedIdentities,

    -- * Destructuring the Response
    IdentityPool (..),
    newIdentityPool,

    -- * Response Lenses
    identityPool_allowClassicFlow,
    identityPool_samlProviderARNs,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | An object representing an Amazon Cognito identity pool.
--
-- /See:/ 'newUpdateIdentityPool' smart constructor.
data UpdateIdentityPool = UpdateIdentityPool'
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
-- Create a value of 'UpdateIdentityPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowClassicFlow', 'updateIdentityPool_allowClassicFlow' - Enables or disables the Basic (Classic) authentication flow. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow>
-- in the /Amazon Cognito Developer Guide/.
--
-- 'samlProviderARNs', 'updateIdentityPool_samlProviderARNs' - An array of Amazon Resource Names (ARNs) of the SAML provider for your
-- identity pool.
--
-- 'identityPoolTags', 'updateIdentityPool_identityPoolTags' - The tags that are assigned to the identity pool. A tag is a label that
-- you can apply to identity pools to categorize and manage them in
-- different ways, such as by purpose, owner, environment, or other
-- criteria.
--
-- 'openIdConnectProviderARNs', 'updateIdentityPool_openIdConnectProviderARNs' - The ARNs of the OpenID Connect providers.
--
-- 'supportedLoginProviders', 'updateIdentityPool_supportedLoginProviders' - Optional key:value pairs mapping provider names to provider app IDs.
--
-- 'cognitoIdentityProviders', 'updateIdentityPool_cognitoIdentityProviders' - A list representing an Amazon Cognito user pool and its client ID.
--
-- 'developerProviderName', 'updateIdentityPool_developerProviderName' - The \"domain\" by which Cognito will refer to your users.
--
-- 'identityPoolId', 'updateIdentityPool_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'identityPoolName', 'updateIdentityPool_identityPoolName' - A string that you provide.
--
-- 'allowUnauthenticatedIdentities', 'updateIdentityPool_allowUnauthenticatedIdentities' - TRUE if the identity pool supports unauthenticated logins.
newUpdateIdentityPool ::
  -- | 'identityPoolId'
  Core.Text ->
  -- | 'identityPoolName'
  Core.Text ->
  -- | 'allowUnauthenticatedIdentities'
  Core.Bool ->
  UpdateIdentityPool
newUpdateIdentityPool
  pIdentityPoolId_
  pIdentityPoolName_
  pAllowUnauthenticatedIdentities_ =
    UpdateIdentityPool'
      { allowClassicFlow =
          Core.Nothing,
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
updateIdentityPool_allowClassicFlow :: Lens.Lens' UpdateIdentityPool (Core.Maybe Core.Bool)
updateIdentityPool_allowClassicFlow = Lens.lens (\UpdateIdentityPool' {allowClassicFlow} -> allowClassicFlow) (\s@UpdateIdentityPool' {} a -> s {allowClassicFlow = a} :: UpdateIdentityPool)

-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your
-- identity pool.
updateIdentityPool_samlProviderARNs :: Lens.Lens' UpdateIdentityPool (Core.Maybe [Core.Text])
updateIdentityPool_samlProviderARNs = Lens.lens (\UpdateIdentityPool' {samlProviderARNs} -> samlProviderARNs) (\s@UpdateIdentityPool' {} a -> s {samlProviderARNs = a} :: UpdateIdentityPool) Core.. Lens.mapping Lens._Coerce

-- | The tags that are assigned to the identity pool. A tag is a label that
-- you can apply to identity pools to categorize and manage them in
-- different ways, such as by purpose, owner, environment, or other
-- criteria.
updateIdentityPool_identityPoolTags :: Lens.Lens' UpdateIdentityPool (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateIdentityPool_identityPoolTags = Lens.lens (\UpdateIdentityPool' {identityPoolTags} -> identityPoolTags) (\s@UpdateIdentityPool' {} a -> s {identityPoolTags = a} :: UpdateIdentityPool) Core.. Lens.mapping Lens._Coerce

-- | The ARNs of the OpenID Connect providers.
updateIdentityPool_openIdConnectProviderARNs :: Lens.Lens' UpdateIdentityPool (Core.Maybe [Core.Text])
updateIdentityPool_openIdConnectProviderARNs = Lens.lens (\UpdateIdentityPool' {openIdConnectProviderARNs} -> openIdConnectProviderARNs) (\s@UpdateIdentityPool' {} a -> s {openIdConnectProviderARNs = a} :: UpdateIdentityPool) Core.. Lens.mapping Lens._Coerce

-- | Optional key:value pairs mapping provider names to provider app IDs.
updateIdentityPool_supportedLoginProviders :: Lens.Lens' UpdateIdentityPool (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateIdentityPool_supportedLoginProviders = Lens.lens (\UpdateIdentityPool' {supportedLoginProviders} -> supportedLoginProviders) (\s@UpdateIdentityPool' {} a -> s {supportedLoginProviders = a} :: UpdateIdentityPool) Core.. Lens.mapping Lens._Coerce

-- | A list representing an Amazon Cognito user pool and its client ID.
updateIdentityPool_cognitoIdentityProviders :: Lens.Lens' UpdateIdentityPool (Core.Maybe [CognitoIdentityProvider])
updateIdentityPool_cognitoIdentityProviders = Lens.lens (\UpdateIdentityPool' {cognitoIdentityProviders} -> cognitoIdentityProviders) (\s@UpdateIdentityPool' {} a -> s {cognitoIdentityProviders = a} :: UpdateIdentityPool) Core.. Lens.mapping Lens._Coerce

-- | The \"domain\" by which Cognito will refer to your users.
updateIdentityPool_developerProviderName :: Lens.Lens' UpdateIdentityPool (Core.Maybe Core.Text)
updateIdentityPool_developerProviderName = Lens.lens (\UpdateIdentityPool' {developerProviderName} -> developerProviderName) (\s@UpdateIdentityPool' {} a -> s {developerProviderName = a} :: UpdateIdentityPool)

-- | An identity pool ID in the format REGION:GUID.
updateIdentityPool_identityPoolId :: Lens.Lens' UpdateIdentityPool Core.Text
updateIdentityPool_identityPoolId = Lens.lens (\UpdateIdentityPool' {identityPoolId} -> identityPoolId) (\s@UpdateIdentityPool' {} a -> s {identityPoolId = a} :: UpdateIdentityPool)

-- | A string that you provide.
updateIdentityPool_identityPoolName :: Lens.Lens' UpdateIdentityPool Core.Text
updateIdentityPool_identityPoolName = Lens.lens (\UpdateIdentityPool' {identityPoolName} -> identityPoolName) (\s@UpdateIdentityPool' {} a -> s {identityPoolName = a} :: UpdateIdentityPool)

-- | TRUE if the identity pool supports unauthenticated logins.
updateIdentityPool_allowUnauthenticatedIdentities :: Lens.Lens' UpdateIdentityPool Core.Bool
updateIdentityPool_allowUnauthenticatedIdentities = Lens.lens (\UpdateIdentityPool' {allowUnauthenticatedIdentities} -> allowUnauthenticatedIdentities) (\s@UpdateIdentityPool' {} a -> s {allowUnauthenticatedIdentities = a} :: UpdateIdentityPool)

instance Core.AWSRequest UpdateIdentityPool where
  type AWSResponse UpdateIdentityPool = IdentityPool
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable UpdateIdentityPool

instance Core.NFData UpdateIdentityPool

instance Core.ToHeaders UpdateIdentityPool where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.UpdateIdentityPool" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateIdentityPool where
  toJSON UpdateIdentityPool' {..} =
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

instance Core.ToPath UpdateIdentityPool where
  toPath = Core.const "/"

instance Core.ToQuery UpdateIdentityPool where
  toQuery = Core.const Core.mempty
