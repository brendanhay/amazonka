{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LicenseConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LicenseConfiguration
  ( LicenseConfiguration (..),

    -- * Smart constructor
    mkLicenseConfiguration,

    -- * Lenses
    lcLicenseConfigurationArn,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a license configuration.
--
-- /See:/ 'mkLicenseConfiguration' smart constructor.
newtype LicenseConfiguration = LicenseConfiguration'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LicenseConfiguration' value with any optional fields omitted.
mkLicenseConfiguration ::
  LicenseConfiguration
mkLicenseConfiguration =
  LicenseConfiguration' {licenseConfigurationArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLicenseConfigurationArn :: Lens.Lens' LicenseConfiguration (Core.Maybe Types.String)
lcLicenseConfigurationArn = Lens.field @"licenseConfigurationArn"
{-# DEPRECATED lcLicenseConfigurationArn "Use generic-lens or generic-optics with 'licenseConfigurationArn' instead." #-}

instance Core.FromXML LicenseConfiguration where
  parseXML x =
    LicenseConfiguration'
      Core.<$> (x Core..@? "licenseConfigurationArn")
