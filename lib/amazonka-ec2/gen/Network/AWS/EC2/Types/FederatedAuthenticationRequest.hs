{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FederatedAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FederatedAuthenticationRequest
  ( FederatedAuthenticationRequest (..)
  -- * Smart constructor
  , mkFederatedAuthenticationRequest
  -- * Lenses
  , farSAMLProviderArn
  , farSelfServiceSAMLProviderArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The IAM SAML identity provider used for federated authentication.
--
-- /See:/ 'mkFederatedAuthenticationRequest' smart constructor.
data FederatedAuthenticationRequest = FederatedAuthenticationRequest'
  { sAMLProviderArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM SAML identity provider.
  , selfServiceSAMLProviderArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FederatedAuthenticationRequest' value with any optional fields omitted.
mkFederatedAuthenticationRequest
    :: FederatedAuthenticationRequest
mkFederatedAuthenticationRequest
  = FederatedAuthenticationRequest'{sAMLProviderArn = Core.Nothing,
                                    selfServiceSAMLProviderArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
--
-- /Note:/ Consider using 'sAMLProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farSAMLProviderArn :: Lens.Lens' FederatedAuthenticationRequest (Core.Maybe Core.Text)
farSAMLProviderArn = Lens.field @"sAMLProviderArn"
{-# INLINEABLE farSAMLProviderArn #-}
{-# DEPRECATED sAMLProviderArn "Use generic-lens or generic-optics with 'sAMLProviderArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
--
-- /Note:/ Consider using 'selfServiceSAMLProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farSelfServiceSAMLProviderArn :: Lens.Lens' FederatedAuthenticationRequest (Core.Maybe Core.Text)
farSelfServiceSAMLProviderArn = Lens.field @"selfServiceSAMLProviderArn"
{-# INLINEABLE farSelfServiceSAMLProviderArn #-}
{-# DEPRECATED selfServiceSAMLProviderArn "Use generic-lens or generic-optics with 'selfServiceSAMLProviderArn' instead"  #-}

instance Core.ToQuery FederatedAuthenticationRequest where
        toQuery FederatedAuthenticationRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "SAMLProviderArn")
              sAMLProviderArn
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SelfServiceSAMLProviderArn")
                selfServiceSAMLProviderArn
