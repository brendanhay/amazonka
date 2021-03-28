{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
  ( ImportImageLicenseConfigurationResponse (..)
  -- * Smart constructor
  , mkImportImageLicenseConfigurationResponse
  -- * Lenses
  , iLicenseConfigurationArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The response information for license configurations.
--
-- /See:/ 'mkImportImageLicenseConfigurationResponse' smart constructor.
newtype ImportImageLicenseConfigurationResponse = ImportImageLicenseConfigurationResponse'
  { licenseConfigurationArn :: Core.Maybe Core.Text
    -- ^ The ARN of a license configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportImageLicenseConfigurationResponse' value with any optional fields omitted.
mkImportImageLicenseConfigurationResponse
    :: ImportImageLicenseConfigurationResponse
mkImportImageLicenseConfigurationResponse
  = ImportImageLicenseConfigurationResponse'{licenseConfigurationArn
                                               = Core.Nothing}

-- | The ARN of a license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLicenseConfigurationArn :: Lens.Lens' ImportImageLicenseConfigurationResponse (Core.Maybe Core.Text)
iLicenseConfigurationArn = Lens.field @"licenseConfigurationArn"
{-# INLINEABLE iLicenseConfigurationArn #-}
{-# DEPRECATED licenseConfigurationArn "Use generic-lens or generic-optics with 'licenseConfigurationArn' instead"  #-}

instance Core.FromXML ImportImageLicenseConfigurationResponse where
        parseXML x
          = ImportImageLicenseConfigurationResponse' Core.<$>
              (x Core..@? "licenseConfigurationArn")
