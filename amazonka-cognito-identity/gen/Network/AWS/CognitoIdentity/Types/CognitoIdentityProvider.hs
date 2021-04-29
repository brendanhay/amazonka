{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A provider representing an Amazon Cognito user pool and its client ID.
--
-- /See:/ 'newCognitoIdentityProvider' smart constructor.
data CognitoIdentityProvider = CognitoIdentityProvider'
  { -- | The client ID for the Amazon Cognito user pool.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The provider name for an Amazon Cognito user pool. For example,
    -- @cognito-idp.us-east-1.amazonaws.com\/us-east-1_123456789@.
    providerName :: Prelude.Maybe Prelude.Text,
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
    serverSideTokenCheck :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { clientId =
        Prelude.Nothing,
      providerName = Prelude.Nothing,
      serverSideTokenCheck = Prelude.Nothing
    }

-- | The client ID for the Amazon Cognito user pool.
cognitoIdentityProvider_clientId :: Lens.Lens' CognitoIdentityProvider (Prelude.Maybe Prelude.Text)
cognitoIdentityProvider_clientId = Lens.lens (\CognitoIdentityProvider' {clientId} -> clientId) (\s@CognitoIdentityProvider' {} a -> s {clientId = a} :: CognitoIdentityProvider)

-- | The provider name for an Amazon Cognito user pool. For example,
-- @cognito-idp.us-east-1.amazonaws.com\/us-east-1_123456789@.
cognitoIdentityProvider_providerName :: Lens.Lens' CognitoIdentityProvider (Prelude.Maybe Prelude.Text)
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
cognitoIdentityProvider_serverSideTokenCheck :: Lens.Lens' CognitoIdentityProvider (Prelude.Maybe Prelude.Bool)
cognitoIdentityProvider_serverSideTokenCheck = Lens.lens (\CognitoIdentityProvider' {serverSideTokenCheck} -> serverSideTokenCheck) (\s@CognitoIdentityProvider' {} a -> s {serverSideTokenCheck = a} :: CognitoIdentityProvider)

instance Prelude.FromJSON CognitoIdentityProvider where
  parseJSON =
    Prelude.withObject
      "CognitoIdentityProvider"
      ( \x ->
          CognitoIdentityProvider'
            Prelude.<$> (x Prelude..:? "ClientId")
            Prelude.<*> (x Prelude..:? "ProviderName")
            Prelude.<*> (x Prelude..:? "ServerSideTokenCheck")
      )

instance Prelude.Hashable CognitoIdentityProvider

instance Prelude.NFData CognitoIdentityProvider

instance Prelude.ToJSON CognitoIdentityProvider where
  toJSON CognitoIdentityProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ClientId" Prelude..=) Prelude.<$> clientId,
            ("ProviderName" Prelude..=) Prelude.<$> providerName,
            ("ServerSideTokenCheck" Prelude..=)
              Prelude.<$> serverSideTokenCheck
          ]
      )
