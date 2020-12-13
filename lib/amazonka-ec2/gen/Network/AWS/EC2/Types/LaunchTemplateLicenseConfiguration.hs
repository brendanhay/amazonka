{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
  ( LaunchTemplateLicenseConfiguration (..),

    -- * Smart constructor
    mkLaunchTemplateLicenseConfiguration,

    -- * Lenses
    ltlcLicenseConfigurationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a license configuration.
--
-- /See:/ 'mkLaunchTemplateLicenseConfiguration' smart constructor.
newtype LaunchTemplateLicenseConfiguration = LaunchTemplateLicenseConfiguration'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateLicenseConfiguration' with the minimum fields required to make a request.
--
-- * 'licenseConfigurationARN' - The Amazon Resource Name (ARN) of the license configuration.
mkLaunchTemplateLicenseConfiguration ::
  LaunchTemplateLicenseConfiguration
mkLaunchTemplateLicenseConfiguration =
  LaunchTemplateLicenseConfiguration'
    { licenseConfigurationARN =
        Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlcLicenseConfigurationARN :: Lens.Lens' LaunchTemplateLicenseConfiguration (Lude.Maybe Lude.Text)
ltlcLicenseConfigurationARN = Lens.lens (licenseConfigurationARN :: LaunchTemplateLicenseConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {licenseConfigurationARN = a} :: LaunchTemplateLicenseConfiguration)
{-# DEPRECATED ltlcLicenseConfigurationARN "Use generic-lens or generic-optics with 'licenseConfigurationARN' instead." #-}

instance Lude.FromXML LaunchTemplateLicenseConfiguration where
  parseXML x =
    LaunchTemplateLicenseConfiguration'
      Lude.<$> (x Lude..@? "licenseConfigurationArn")
