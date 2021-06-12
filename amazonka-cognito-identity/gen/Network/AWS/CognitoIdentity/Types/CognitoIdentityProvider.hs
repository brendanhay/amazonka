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
-- Module      : Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A provider representing an Amazon Cognito user pool and its client ID.
--
-- /See:/ 'newCognitoIdentityProvider' smart constructor.
data CognitoIdentityProvider = CognitoIdentityProvider'
  { -- | The client ID for the Amazon Cognito user pool.
    clientId :: Core.Maybe Core.Text,
    -- | The provider name for an Amazon Cognito user pool. For example,
    -- @cognito-idp.us-east-1.amazonaws.com\/us-east-1_123456789@.
    providerName :: Core.Maybe Core.Text,
    -- | TRUE if server-side token validation is enabled for the identity
    -- provider’s token.
    --
    -- Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that
    -- identity pool will check with the integrated user pools to make sure
    -- that the user has not been globally signed out or deleted before the
    -- identity pool provides an OIDC token or AWS credentials for the user.
    --
    -- If the user is signed out or deleted, the identity pool will return a
    -- 400 Not Authorized error.
    serverSideTokenCheck :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CognitoIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'cognitoIdentityProvider_clientId' - The client ID for the Amazon Cognito user pool.
--
-- 'providerName', 'cognitoIdentityProvider_providerName' - The provider name for an Amazon Cognito user pool. For example,
-- @cognito-idp.us-east-1.amazonaws.com\/us-east-1_123456789@.
--
-- 'serverSideTokenCheck', 'cognitoIdentityProvider_serverSideTokenCheck' - TRUE if server-side token validation is enabled for the identity
-- provider’s token.
--
-- Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that
-- identity pool will check with the integrated user pools to make sure
-- that the user has not been globally signed out or deleted before the
-- identity pool provides an OIDC token or AWS credentials for the user.
--
-- If the user is signed out or deleted, the identity pool will return a
-- 400 Not Authorized error.
newCognitoIdentityProvider ::
  CognitoIdentityProvider
newCognitoIdentityProvider =
  CognitoIdentityProvider'
    { clientId = Core.Nothing,
      providerName = Core.Nothing,
      serverSideTokenCheck = Core.Nothing
    }

-- | The client ID for the Amazon Cognito user pool.
cognitoIdentityProvider_clientId :: Lens.Lens' CognitoIdentityProvider (Core.Maybe Core.Text)
cognitoIdentityProvider_clientId = Lens.lens (\CognitoIdentityProvider' {clientId} -> clientId) (\s@CognitoIdentityProvider' {} a -> s {clientId = a} :: CognitoIdentityProvider)

-- | The provider name for an Amazon Cognito user pool. For example,
-- @cognito-idp.us-east-1.amazonaws.com\/us-east-1_123456789@.
cognitoIdentityProvider_providerName :: Lens.Lens' CognitoIdentityProvider (Core.Maybe Core.Text)
cognitoIdentityProvider_providerName = Lens.lens (\CognitoIdentityProvider' {providerName} -> providerName) (\s@CognitoIdentityProvider' {} a -> s {providerName = a} :: CognitoIdentityProvider)

-- | TRUE if server-side token validation is enabled for the identity
-- provider’s token.
--
-- Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that
-- identity pool will check with the integrated user pools to make sure
-- that the user has not been globally signed out or deleted before the
-- identity pool provides an OIDC token or AWS credentials for the user.
--
-- If the user is signed out or deleted, the identity pool will return a
-- 400 Not Authorized error.
cognitoIdentityProvider_serverSideTokenCheck :: Lens.Lens' CognitoIdentityProvider (Core.Maybe Core.Bool)
cognitoIdentityProvider_serverSideTokenCheck = Lens.lens (\CognitoIdentityProvider' {serverSideTokenCheck} -> serverSideTokenCheck) (\s@CognitoIdentityProvider' {} a -> s {serverSideTokenCheck = a} :: CognitoIdentityProvider)

instance Core.FromJSON CognitoIdentityProvider where
  parseJSON =
    Core.withObject
      "CognitoIdentityProvider"
      ( \x ->
          CognitoIdentityProvider'
            Core.<$> (x Core..:? "ClientId")
            Core.<*> (x Core..:? "ProviderName")
            Core.<*> (x Core..:? "ServerSideTokenCheck")
      )

instance Core.Hashable CognitoIdentityProvider

instance Core.NFData CognitoIdentityProvider

instance Core.ToJSON CognitoIdentityProvider where
  toJSON CognitoIdentityProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientId" Core..=) Core.<$> clientId,
            ("ProviderName" Core..=) Core.<$> providerName,
            ("ServerSideTokenCheck" Core..=)
              Core.<$> serverSideTokenCheck
          ]
      )
