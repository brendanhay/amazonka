{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
  ( LaunchTemplateLicenseConfigurationRequest (..)
  -- * Smart constructor
  , mkLaunchTemplateLicenseConfigurationRequest
  -- * Lenses
  , ltlcrLicenseConfigurationArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a license configuration.
--
-- /See:/ 'mkLaunchTemplateLicenseConfigurationRequest' smart constructor.
newtype LaunchTemplateLicenseConfigurationRequest = LaunchTemplateLicenseConfigurationRequest'
  { licenseConfigurationArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the license configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateLicenseConfigurationRequest' value with any optional fields omitted.
mkLaunchTemplateLicenseConfigurationRequest
    :: LaunchTemplateLicenseConfigurationRequest
mkLaunchTemplateLicenseConfigurationRequest
  = LaunchTemplateLicenseConfigurationRequest'{licenseConfigurationArn
                                                 = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlcrLicenseConfigurationArn :: Lens.Lens' LaunchTemplateLicenseConfigurationRequest (Core.Maybe Core.Text)
ltlcrLicenseConfigurationArn = Lens.field @"licenseConfigurationArn"
{-# INLINEABLE ltlcrLicenseConfigurationArn #-}
{-# DEPRECATED licenseConfigurationArn "Use generic-lens or generic-optics with 'licenseConfigurationArn' instead"  #-}

instance Core.ToQuery LaunchTemplateLicenseConfigurationRequest
         where
        toQuery LaunchTemplateLicenseConfigurationRequest{..}
          = Core.maybe Core.mempty
              (Core.toQueryPair "LicenseConfigurationArn")
              licenseConfigurationArn
