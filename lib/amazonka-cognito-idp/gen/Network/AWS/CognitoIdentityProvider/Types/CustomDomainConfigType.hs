{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
  ( CustomDomainConfigType (..)
  -- * Smart constructor
  , mkCustomDomainConfigType
  -- * Lenses
  , cdctCertificateArn
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.ArnType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
--
-- /See:/ 'mkCustomDomainConfigType' smart constructor.
newtype CustomDomainConfigType = CustomDomainConfigType'
  { certificateArn :: Types.ArnType
    -- ^ The Amazon Resource Name (ARN) of an AWS Certificate Manager SSL certificate. You use this certificate for the subdomain of your custom domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CustomDomainConfigType' value with any optional fields omitted.
mkCustomDomainConfigType
    :: Types.ArnType -- ^ 'certificateArn'
    -> CustomDomainConfigType
mkCustomDomainConfigType certificateArn
  = CustomDomainConfigType'{certificateArn}

-- | The Amazon Resource Name (ARN) of an AWS Certificate Manager SSL certificate. You use this certificate for the subdomain of your custom domain.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdctCertificateArn :: Lens.Lens' CustomDomainConfigType Types.ArnType
cdctCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE cdctCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

instance Core.FromJSON CustomDomainConfigType where
        toJSON CustomDomainConfigType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateArn" Core..= certificateArn)])

instance Core.FromJSON CustomDomainConfigType where
        parseJSON
          = Core.withObject "CustomDomainConfigType" Core.$
              \ x ->
                CustomDomainConfigType' Core.<$> (x Core..: "CertificateArn")
