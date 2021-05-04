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
-- Module      : Network.AWS.CognitoIdentity.CreateIdentityPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new identity pool. The identity pool is a store of user
-- identity information that is specific to your AWS account. The keys for
-- @SupportedLoginProviders@ are as follows:
--
-- -   Facebook: @graph.facebook.com@
--
-- -   Google: @accounts.google.com@
--
-- -   Amazon: @www.amazon.com@
--
-- -   Twitter: @api.twitter.com@
--
-- -   Digits: @www.digits.com@
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.CreateIdentityPool
  ( -- * Creating a Request
    CreateIdentityPool (..),
    newCreateIdentityPool,

    -- * Request Lenses
    createIdentityPool_allowClassicFlow,
    createIdentityPool_samlProviderARNs,
    createIdentityPool_identityPoolTags,
    createIdentityPool_openIdConnectProviderARNs,
    createIdentityPool_supportedLoginProviders,
    createIdentityPool_cognitoIdentityProviders,
    createIdentityPool_developerProviderName,
    createIdentityPool_identityPoolName,
    createIdentityPool_allowUnauthenticatedIdentities,

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the CreateIdentityPool action.
--
-- /See:/ 'newCreateIdentityPool' smart constructor.
data CreateIdentityPool = CreateIdentityPool'
  { -- | Enables or disables the Basic (Classic) authentication flow. For more
    -- information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow>
    -- in the /Amazon Cognito Developer Guide/.
    allowClassicFlow :: Prelude.Maybe Prelude.Bool,
    -- | An array of Amazon Resource Names (ARNs) of the SAML provider for your
    -- identity pool.
    samlProviderARNs :: Prelude.Maybe [Prelude.Text],
    -- | Tags to assign to the identity pool. A tag is a label that you can apply
    -- to identity pools to categorize and manage them in different ways, such
    -- as by purpose, owner, environment, or other criteria.
    identityPoolTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Names (ARN) of the OpenID Connect providers.
    openIdConnectProviderARNs :: Prelude.Maybe [Prelude.Text],
    -- | Optional key:value pairs mapping provider names to provider app IDs.
    supportedLoginProviders :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An array of Amazon Cognito user pools and their client IDs.
    cognitoIdentityProviders :: Prelude.Maybe [CognitoIdentityProvider],
    -- | The \"domain\" by which Cognito will refer to your users. This name acts
    -- as a placeholder that allows your backend and the Cognito service to
    -- communicate about the developer provider. For the
    -- @DeveloperProviderName@, you can use letters as well as period (@.@),
    -- underscore (@_@), and dash (@-@).
    --
    -- Once you have set a developer provider name, you cannot change it.
    -- Please take care in setting this parameter.
    developerProviderName :: Prelude.Maybe Prelude.Text,
    -- | A string that you provide.
    identityPoolName :: Prelude.Text,
    -- | TRUE if the identity pool supports unauthenticated logins.
    allowUnauthenticatedIdentities :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateIdentityPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowClassicFlow', 'createIdentityPool_allowClassicFlow' - Enables or disables the Basic (Classic) authentication flow. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow>
-- in the /Amazon Cognito Developer Guide/.
--
-- 'samlProviderARNs', 'createIdentityPool_samlProviderARNs' - An array of Amazon Resource Names (ARNs) of the SAML provider for your
-- identity pool.
--
-- 'identityPoolTags', 'createIdentityPool_identityPoolTags' - Tags to assign to the identity pool. A tag is a label that you can apply
-- to identity pools to categorize and manage them in different ways, such
-- as by purpose, owner, environment, or other criteria.
--
-- 'openIdConnectProviderARNs', 'createIdentityPool_openIdConnectProviderARNs' - The Amazon Resource Names (ARN) of the OpenID Connect providers.
--
-- 'supportedLoginProviders', 'createIdentityPool_supportedLoginProviders' - Optional key:value pairs mapping provider names to provider app IDs.
--
-- 'cognitoIdentityProviders', 'createIdentityPool_cognitoIdentityProviders' - An array of Amazon Cognito user pools and their client IDs.
--
-- 'developerProviderName', 'createIdentityPool_developerProviderName' - The \"domain\" by which Cognito will refer to your users. This name acts
-- as a placeholder that allows your backend and the Cognito service to
-- communicate about the developer provider. For the
-- @DeveloperProviderName@, you can use letters as well as period (@.@),
-- underscore (@_@), and dash (@-@).
--
-- Once you have set a developer provider name, you cannot change it.
-- Please take care in setting this parameter.
--
-- 'identityPoolName', 'createIdentityPool_identityPoolName' - A string that you provide.
--
-- 'allowUnauthenticatedIdentities', 'createIdentityPool_allowUnauthenticatedIdentities' - TRUE if the identity pool supports unauthenticated logins.
newCreateIdentityPool ::
  -- | 'identityPoolName'
  Prelude.Text ->
  -- | 'allowUnauthenticatedIdentities'
  Prelude.Bool ->
  CreateIdentityPool
newCreateIdentityPool
  pIdentityPoolName_
  pAllowUnauthenticatedIdentities_ =
    CreateIdentityPool'
      { allowClassicFlow =
          Prelude.Nothing,
        samlProviderARNs = Prelude.Nothing,
        identityPoolTags = Prelude.Nothing,
        openIdConnectProviderARNs = Prelude.Nothing,
        supportedLoginProviders = Prelude.Nothing,
        cognitoIdentityProviders = Prelude.Nothing,
        developerProviderName = Prelude.Nothing,
        identityPoolName = pIdentityPoolName_,
        allowUnauthenticatedIdentities =
          pAllowUnauthenticatedIdentities_
      }

-- | Enables or disables the Basic (Classic) authentication flow. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow>
-- in the /Amazon Cognito Developer Guide/.
createIdentityPool_allowClassicFlow :: Lens.Lens' CreateIdentityPool (Prelude.Maybe Prelude.Bool)
createIdentityPool_allowClassicFlow = Lens.lens (\CreateIdentityPool' {allowClassicFlow} -> allowClassicFlow) (\s@CreateIdentityPool' {} a -> s {allowClassicFlow = a} :: CreateIdentityPool)

-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your
-- identity pool.
createIdentityPool_samlProviderARNs :: Lens.Lens' CreateIdentityPool (Prelude.Maybe [Prelude.Text])
createIdentityPool_samlProviderARNs = Lens.lens (\CreateIdentityPool' {samlProviderARNs} -> samlProviderARNs) (\s@CreateIdentityPool' {} a -> s {samlProviderARNs = a} :: CreateIdentityPool) Prelude.. Lens.mapping Prelude._Coerce

-- | Tags to assign to the identity pool. A tag is a label that you can apply
-- to identity pools to categorize and manage them in different ways, such
-- as by purpose, owner, environment, or other criteria.
createIdentityPool_identityPoolTags :: Lens.Lens' CreateIdentityPool (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIdentityPool_identityPoolTags = Lens.lens (\CreateIdentityPool' {identityPoolTags} -> identityPoolTags) (\s@CreateIdentityPool' {} a -> s {identityPoolTags = a} :: CreateIdentityPool) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Names (ARN) of the OpenID Connect providers.
createIdentityPool_openIdConnectProviderARNs :: Lens.Lens' CreateIdentityPool (Prelude.Maybe [Prelude.Text])
createIdentityPool_openIdConnectProviderARNs = Lens.lens (\CreateIdentityPool' {openIdConnectProviderARNs} -> openIdConnectProviderARNs) (\s@CreateIdentityPool' {} a -> s {openIdConnectProviderARNs = a} :: CreateIdentityPool) Prelude.. Lens.mapping Prelude._Coerce

-- | Optional key:value pairs mapping provider names to provider app IDs.
createIdentityPool_supportedLoginProviders :: Lens.Lens' CreateIdentityPool (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIdentityPool_supportedLoginProviders = Lens.lens (\CreateIdentityPool' {supportedLoginProviders} -> supportedLoginProviders) (\s@CreateIdentityPool' {} a -> s {supportedLoginProviders = a} :: CreateIdentityPool) Prelude.. Lens.mapping Prelude._Coerce

-- | An array of Amazon Cognito user pools and their client IDs.
createIdentityPool_cognitoIdentityProviders :: Lens.Lens' CreateIdentityPool (Prelude.Maybe [CognitoIdentityProvider])
createIdentityPool_cognitoIdentityProviders = Lens.lens (\CreateIdentityPool' {cognitoIdentityProviders} -> cognitoIdentityProviders) (\s@CreateIdentityPool' {} a -> s {cognitoIdentityProviders = a} :: CreateIdentityPool) Prelude.. Lens.mapping Prelude._Coerce

-- | The \"domain\" by which Cognito will refer to your users. This name acts
-- as a placeholder that allows your backend and the Cognito service to
-- communicate about the developer provider. For the
-- @DeveloperProviderName@, you can use letters as well as period (@.@),
-- underscore (@_@), and dash (@-@).
--
-- Once you have set a developer provider name, you cannot change it.
-- Please take care in setting this parameter.
createIdentityPool_developerProviderName :: Lens.Lens' CreateIdentityPool (Prelude.Maybe Prelude.Text)
createIdentityPool_developerProviderName = Lens.lens (\CreateIdentityPool' {developerProviderName} -> developerProviderName) (\s@CreateIdentityPool' {} a -> s {developerProviderName = a} :: CreateIdentityPool)

-- | A string that you provide.
createIdentityPool_identityPoolName :: Lens.Lens' CreateIdentityPool Prelude.Text
createIdentityPool_identityPoolName = Lens.lens (\CreateIdentityPool' {identityPoolName} -> identityPoolName) (\s@CreateIdentityPool' {} a -> s {identityPoolName = a} :: CreateIdentityPool)

-- | TRUE if the identity pool supports unauthenticated logins.
createIdentityPool_allowUnauthenticatedIdentities :: Lens.Lens' CreateIdentityPool Prelude.Bool
createIdentityPool_allowUnauthenticatedIdentities = Lens.lens (\CreateIdentityPool' {allowUnauthenticatedIdentities} -> allowUnauthenticatedIdentities) (\s@CreateIdentityPool' {} a -> s {allowUnauthenticatedIdentities = a} :: CreateIdentityPool)

instance Prelude.AWSRequest CreateIdentityPool where
  type Rs CreateIdentityPool = IdentityPool
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable CreateIdentityPool

instance Prelude.NFData CreateIdentityPool

instance Prelude.ToHeaders CreateIdentityPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityService.CreateIdentityPool" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateIdentityPool where
  toJSON CreateIdentityPool' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AllowClassicFlow" Prelude..=)
              Prelude.<$> allowClassicFlow,
            ("SamlProviderARNs" Prelude..=)
              Prelude.<$> samlProviderARNs,
            ("IdentityPoolTags" Prelude..=)
              Prelude.<$> identityPoolTags,
            ("OpenIdConnectProviderARNs" Prelude..=)
              Prelude.<$> openIdConnectProviderARNs,
            ("SupportedLoginProviders" Prelude..=)
              Prelude.<$> supportedLoginProviders,
            ("CognitoIdentityProviders" Prelude..=)
              Prelude.<$> cognitoIdentityProviders,
            ("DeveloperProviderName" Prelude..=)
              Prelude.<$> developerProviderName,
            Prelude.Just
              ("IdentityPoolName" Prelude..= identityPoolName),
            Prelude.Just
              ( "AllowUnauthenticatedIdentities"
                  Prelude..= allowUnauthenticatedIdentities
              )
          ]
      )

instance Prelude.ToPath CreateIdentityPool where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateIdentityPool where
  toQuery = Prelude.const Prelude.mempty
