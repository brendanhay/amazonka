{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
  ( CognitoIdentityProvider (..),

    -- * Smart constructor
    mkCognitoIdentityProvider,

    -- * Lenses
    cipClientId,
    cipServerSideTokenCheck,
    cipProviderName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A provider representing an Amazon Cognito user pool and its client ID.
--
-- /See:/ 'mkCognitoIdentityProvider' smart constructor.
data CognitoIdentityProvider = CognitoIdentityProvider'
  { -- | The client ID for the Amazon Cognito user pool.
    clientId :: Lude.Maybe Lude.Text,
    -- | TRUE if server-side token validation is enabled for the identity provider’s token.
    --
    -- Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that identity pool will check with the integrated user pools to make sure that the user has not been globally signed out or deleted before the identity pool provides an OIDC token or AWS credentials for the user.
    -- If the user is signed out or deleted, the identity pool will return a 400 Not Authorized error.
    serverSideTokenCheck :: Lude.Maybe Lude.Bool,
    -- | The provider name for an Amazon Cognito user pool. For example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
    providerName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CognitoIdentityProvider' with the minimum fields required to make a request.
--
-- * 'clientId' - The client ID for the Amazon Cognito user pool.
-- * 'serverSideTokenCheck' - TRUE if server-side token validation is enabled for the identity provider’s token.
--
-- Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that identity pool will check with the integrated user pools to make sure that the user has not been globally signed out or deleted before the identity pool provides an OIDC token or AWS credentials for the user.
-- If the user is signed out or deleted, the identity pool will return a 400 Not Authorized error.
-- * 'providerName' - The provider name for an Amazon Cognito user pool. For example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
mkCognitoIdentityProvider ::
  CognitoIdentityProvider
mkCognitoIdentityProvider =
  CognitoIdentityProvider'
    { clientId = Lude.Nothing,
      serverSideTokenCheck = Lude.Nothing,
      providerName = Lude.Nothing
    }

-- | The client ID for the Amazon Cognito user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipClientId :: Lens.Lens' CognitoIdentityProvider (Lude.Maybe Lude.Text)
cipClientId = Lens.lens (clientId :: CognitoIdentityProvider -> Lude.Maybe Lude.Text) (\s a -> s {clientId = a} :: CognitoIdentityProvider)
{-# DEPRECATED cipClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | TRUE if server-side token validation is enabled for the identity provider’s token.
--
-- Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that identity pool will check with the integrated user pools to make sure that the user has not been globally signed out or deleted before the identity pool provides an OIDC token or AWS credentials for the user.
-- If the user is signed out or deleted, the identity pool will return a 400 Not Authorized error.
--
-- /Note:/ Consider using 'serverSideTokenCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipServerSideTokenCheck :: Lens.Lens' CognitoIdentityProvider (Lude.Maybe Lude.Bool)
cipServerSideTokenCheck = Lens.lens (serverSideTokenCheck :: CognitoIdentityProvider -> Lude.Maybe Lude.Bool) (\s a -> s {serverSideTokenCheck = a} :: CognitoIdentityProvider)
{-# DEPRECATED cipServerSideTokenCheck "Use generic-lens or generic-optics with 'serverSideTokenCheck' instead." #-}

-- | The provider name for an Amazon Cognito user pool. For example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipProviderName :: Lens.Lens' CognitoIdentityProvider (Lude.Maybe Lude.Text)
cipProviderName = Lens.lens (providerName :: CognitoIdentityProvider -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: CognitoIdentityProvider)
{-# DEPRECATED cipProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Lude.FromJSON CognitoIdentityProvider where
  parseJSON =
    Lude.withObject
      "CognitoIdentityProvider"
      ( \x ->
          CognitoIdentityProvider'
            Lude.<$> (x Lude..:? "ClientId")
            Lude.<*> (x Lude..:? "ServerSideTokenCheck")
            Lude.<*> (x Lude..:? "ProviderName")
      )

instance Lude.ToJSON CognitoIdentityProvider where
  toJSON CognitoIdentityProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientId" Lude..=) Lude.<$> clientId,
            ("ServerSideTokenCheck" Lude..=) Lude.<$> serverSideTokenCheck,
            ("ProviderName" Lude..=) Lude.<$> providerName
          ]
      )
