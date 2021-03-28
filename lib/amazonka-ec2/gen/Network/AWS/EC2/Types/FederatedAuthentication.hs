{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FederatedAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FederatedAuthentication
  ( FederatedAuthentication (..)
  -- * Smart constructor
  , mkFederatedAuthentication
  -- * Lenses
  , faSamlProviderArn
  , faSelfServiceSamlProviderArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the IAM SAML identity providers used for federated authentication.
--
-- /See:/ 'mkFederatedAuthentication' smart constructor.
data FederatedAuthentication = FederatedAuthentication'
  { samlProviderArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM SAML identity provider.
  , selfServiceSamlProviderArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FederatedAuthentication' value with any optional fields omitted.
mkFederatedAuthentication
    :: FederatedAuthentication
mkFederatedAuthentication
  = FederatedAuthentication'{samlProviderArn = Core.Nothing,
                             selfServiceSamlProviderArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider.
--
-- /Note:/ Consider using 'samlProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faSamlProviderArn :: Lens.Lens' FederatedAuthentication (Core.Maybe Core.Text)
faSamlProviderArn = Lens.field @"samlProviderArn"
{-# INLINEABLE faSamlProviderArn #-}
{-# DEPRECATED samlProviderArn "Use generic-lens or generic-optics with 'samlProviderArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM SAML identity provider for the self-service portal.
--
-- /Note:/ Consider using 'selfServiceSamlProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faSelfServiceSamlProviderArn :: Lens.Lens' FederatedAuthentication (Core.Maybe Core.Text)
faSelfServiceSamlProviderArn = Lens.field @"selfServiceSamlProviderArn"
{-# INLINEABLE faSelfServiceSamlProviderArn #-}
{-# DEPRECATED selfServiceSamlProviderArn "Use generic-lens or generic-optics with 'selfServiceSamlProviderArn' instead"  #-}

instance Core.FromXML FederatedAuthentication where
        parseXML x
          = FederatedAuthentication' Core.<$>
              (x Core..@? "samlProviderArn") Core.<*>
                x Core..@? "selfServiceSamlProviderArn"
