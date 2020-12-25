{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FederatedAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FederatedAuthenticationRequest
  ( FederatedAuthenticationRequest (..),

    -- * Smart constructor
    mkFederatedAuthenticationRequest,

    -- * Lenses
    farSAMLProviderArn,
    farSelfServiceSAMLProviderArn,
  )
where

import qualified Network.AWS.EC2.Types.SAMLProviderArn as Types
import qualified Network.AWS.EC2.Types.SelfServiceSAMLProviderArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The IAM SAML identity provider used for federated authentication.
--
-- /See:/ 'mkFederatedAuthenticationRequest' smart constructor.
data FederatedAuthenticationRequest = FederatedAuthenticationRequest'
  { -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
    sAMLProviderArn :: Core.Maybe Types.SAMLProviderArn,
    -- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
    selfServiceSAMLProviderArn :: Core.Maybe Types.SelfServiceSAMLProviderArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FederatedAuthenticationRequest' value with any optional fields omitted.
mkFederatedAuthenticationRequest ::
  FederatedAuthenticationRequest
mkFederatedAuthenticationRequest =
  FederatedAuthenticationRequest'
    { sAMLProviderArn = Core.Nothing,
      selfServiceSAMLProviderArn = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
--
-- /Note:/ Consider using 'sAMLProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farSAMLProviderArn :: Lens.Lens' FederatedAuthenticationRequest (Core.Maybe Types.SAMLProviderArn)
farSAMLProviderArn = Lens.field @"sAMLProviderArn"
{-# DEPRECATED farSAMLProviderArn "Use generic-lens or generic-optics with 'sAMLProviderArn' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
--
-- /Note:/ Consider using 'selfServiceSAMLProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farSelfServiceSAMLProviderArn :: Lens.Lens' FederatedAuthenticationRequest (Core.Maybe Types.SelfServiceSAMLProviderArn)
farSelfServiceSAMLProviderArn = Lens.field @"selfServiceSAMLProviderArn"
{-# DEPRECATED farSelfServiceSAMLProviderArn "Use generic-lens or generic-optics with 'selfServiceSAMLProviderArn' instead." #-}
