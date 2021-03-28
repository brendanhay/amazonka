{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
  ( CognitoIdentityProvider (..)
  -- * Smart constructor
  , mkCognitoIdentityProvider
  -- * Lenses
  , cipClientId
  , cipProviderName
  , cipServerSideTokenCheck
  ) where

import qualified Network.AWS.CognitoIdentity.Types.CognitoIdentityProviderClientId as Types
import qualified Network.AWS.CognitoIdentity.Types.ProviderName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A provider representing an Amazon Cognito user pool and its client ID.
--
-- /See:/ 'mkCognitoIdentityProvider' smart constructor.
data CognitoIdentityProvider = CognitoIdentityProvider'
  { clientId :: Core.Maybe Types.CognitoIdentityProviderClientId
    -- ^ The client ID for the Amazon Cognito user pool.
  , providerName :: Core.Maybe Types.ProviderName
    -- ^ The provider name for an Amazon Cognito user pool. For example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
  , serverSideTokenCheck :: Core.Maybe Core.Bool
    -- ^ TRUE if server-side token validation is enabled for the identity provider’s token.
--
-- Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that identity pool will check with the integrated user pools to make sure that the user has not been globally signed out or deleted before the identity pool provides an OIDC token or AWS credentials for the user.
-- If the user is signed out or deleted, the identity pool will return a 400 Not Authorized error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CognitoIdentityProvider' value with any optional fields omitted.
mkCognitoIdentityProvider
    :: CognitoIdentityProvider
mkCognitoIdentityProvider
  = CognitoIdentityProvider'{clientId = Core.Nothing,
                             providerName = Core.Nothing, serverSideTokenCheck = Core.Nothing}

-- | The client ID for the Amazon Cognito user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipClientId :: Lens.Lens' CognitoIdentityProvider (Core.Maybe Types.CognitoIdentityProviderClientId)
cipClientId = Lens.field @"clientId"
{-# INLINEABLE cipClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The provider name for an Amazon Cognito user pool. For example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipProviderName :: Lens.Lens' CognitoIdentityProvider (Core.Maybe Types.ProviderName)
cipProviderName = Lens.field @"providerName"
{-# INLINEABLE cipProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

-- | TRUE if server-side token validation is enabled for the identity provider’s token.
--
-- Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that identity pool will check with the integrated user pools to make sure that the user has not been globally signed out or deleted before the identity pool provides an OIDC token or AWS credentials for the user.
-- If the user is signed out or deleted, the identity pool will return a 400 Not Authorized error.
--
-- /Note:/ Consider using 'serverSideTokenCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipServerSideTokenCheck :: Lens.Lens' CognitoIdentityProvider (Core.Maybe Core.Bool)
cipServerSideTokenCheck = Lens.field @"serverSideTokenCheck"
{-# INLINEABLE cipServerSideTokenCheck #-}
{-# DEPRECATED serverSideTokenCheck "Use generic-lens or generic-optics with 'serverSideTokenCheck' instead"  #-}

instance Core.FromJSON CognitoIdentityProvider where
        toJSON CognitoIdentityProvider{..}
          = Core.object
              (Core.catMaybes
                 [("ClientId" Core..=) Core.<$> clientId,
                  ("ProviderName" Core..=) Core.<$> providerName,
                  ("ServerSideTokenCheck" Core..=) Core.<$> serverSideTokenCheck])

instance Core.FromJSON CognitoIdentityProvider where
        parseJSON
          = Core.withObject "CognitoIdentityProvider" Core.$
              \ x ->
                CognitoIdentityProvider' Core.<$>
                  (x Core..:? "ClientId") Core.<*> x Core..:? "ProviderName" Core.<*>
                    x Core..:? "ServerSideTokenCheck"
