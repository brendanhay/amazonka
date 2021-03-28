{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
  ( LaunchTemplateLicenseConfiguration (..)
  -- * Smart constructor
  , mkLaunchTemplateLicenseConfiguration
  -- * Lenses
  , ltlcLicenseConfigurationArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a license configuration.
--
-- /See:/ 'mkLaunchTemplateLicenseConfiguration' smart constructor.
newtype LaunchTemplateLicenseConfiguration = LaunchTemplateLicenseConfiguration'
  { licenseConfigurationArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the license configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateLicenseConfiguration' value with any optional fields omitted.
mkLaunchTemplateLicenseConfiguration
    :: LaunchTemplateLicenseConfiguration
mkLaunchTemplateLicenseConfiguration
  = LaunchTemplateLicenseConfiguration'{licenseConfigurationArn =
                                          Core.Nothing}

-- | The Amazon Resource Name (ARN) of the license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlcLicenseConfigurationArn :: Lens.Lens' LaunchTemplateLicenseConfiguration (Core.Maybe Core.Text)
ltlcLicenseConfigurationArn = Lens.field @"licenseConfigurationArn"
{-# INLINEABLE ltlcLicenseConfigurationArn #-}
{-# DEPRECATED licenseConfigurationArn "Use generic-lens or generic-optics with 'licenseConfigurationArn' instead"  #-}

instance Core.FromXML LaunchTemplateLicenseConfiguration where
        parseXML x
          = LaunchTemplateLicenseConfiguration' Core.<$>
              (x Core..@? "licenseConfigurationArn")
