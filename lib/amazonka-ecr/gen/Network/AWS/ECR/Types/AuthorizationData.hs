{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.AuthorizationData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.AuthorizationData
  ( AuthorizationData (..)
  -- * Smart constructor
  , mkAuthorizationData
  -- * Lenses
  , adAuthorizationToken
  , adExpiresAt
  , adProxyEndpoint
  ) where

import qualified Network.AWS.ECR.Types.AuthorizationToken as Types
import qualified Network.AWS.ECR.Types.ProxyEndpoint as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing authorization data for an Amazon ECR registry.
--
-- /See:/ 'mkAuthorizationData' smart constructor.
data AuthorizationData = AuthorizationData'
  { authorizationToken :: Core.Maybe Types.AuthorizationToken
    -- ^ A base64-encoded string that contains authorization data for the specified Amazon ECR registry. When the string is decoded, it is presented in the format @user:password@ for private registry authentication using @docker login@ .
  , expiresAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The Unix time in seconds and milliseconds when the authorization token expires. Authorization tokens are valid for 12 hours.
  , proxyEndpoint :: Core.Maybe Types.ProxyEndpoint
    -- ^ The registry URL to use for this authorization token in a @docker login@ command. The Amazon ECR registry URL format is @https://aws_account_id.dkr.ecr.region.amazonaws.com@ . For example, @https://012345678910.dkr.ecr.us-east-1.amazonaws.com@ .. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AuthorizationData' value with any optional fields omitted.
mkAuthorizationData
    :: AuthorizationData
mkAuthorizationData
  = AuthorizationData'{authorizationToken = Core.Nothing,
                       expiresAt = Core.Nothing, proxyEndpoint = Core.Nothing}

-- | A base64-encoded string that contains authorization data for the specified Amazon ECR registry. When the string is decoded, it is presented in the format @user:password@ for private registry authentication using @docker login@ .
--
-- /Note:/ Consider using 'authorizationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAuthorizationToken :: Lens.Lens' AuthorizationData (Core.Maybe Types.AuthorizationToken)
adAuthorizationToken = Lens.field @"authorizationToken"
{-# INLINEABLE adAuthorizationToken #-}
{-# DEPRECATED authorizationToken "Use generic-lens or generic-optics with 'authorizationToken' instead"  #-}

-- | The Unix time in seconds and milliseconds when the authorization token expires. Authorization tokens are valid for 12 hours.
--
-- /Note:/ Consider using 'expiresAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adExpiresAt :: Lens.Lens' AuthorizationData (Core.Maybe Core.NominalDiffTime)
adExpiresAt = Lens.field @"expiresAt"
{-# INLINEABLE adExpiresAt #-}
{-# DEPRECATED expiresAt "Use generic-lens or generic-optics with 'expiresAt' instead"  #-}

-- | The registry URL to use for this authorization token in a @docker login@ command. The Amazon ECR registry URL format is @https://aws_account_id.dkr.ecr.region.amazonaws.com@ . For example, @https://012345678910.dkr.ecr.us-east-1.amazonaws.com@ .. 
--
-- /Note:/ Consider using 'proxyEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adProxyEndpoint :: Lens.Lens' AuthorizationData (Core.Maybe Types.ProxyEndpoint)
adProxyEndpoint = Lens.field @"proxyEndpoint"
{-# INLINEABLE adProxyEndpoint #-}
{-# DEPRECATED proxyEndpoint "Use generic-lens or generic-optics with 'proxyEndpoint' instead"  #-}

instance Core.FromJSON AuthorizationData where
        parseJSON
          = Core.withObject "AuthorizationData" Core.$
              \ x ->
                AuthorizationData' Core.<$>
                  (x Core..:? "authorizationToken") Core.<*> x Core..:? "expiresAt"
                    Core.<*> x Core..:? "proxyEndpoint"
