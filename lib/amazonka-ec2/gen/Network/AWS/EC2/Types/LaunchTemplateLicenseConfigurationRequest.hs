-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
  ( LaunchTemplateLicenseConfigurationRequest (..),

    -- * Smart constructor
    mkLaunchTemplateLicenseConfigurationRequest,

    -- * Lenses
    ltlcrLicenseConfigurationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a license configuration.
--
-- /See:/ 'mkLaunchTemplateLicenseConfigurationRequest' smart constructor.
newtype LaunchTemplateLicenseConfigurationRequest = LaunchTemplateLicenseConfigurationRequest'
  { licenseConfigurationARN ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateLicenseConfigurationRequest' with the minimum fields required to make a request.
--
-- * 'licenseConfigurationARN' - The Amazon Resource Name (ARN) of the license configuration.
mkLaunchTemplateLicenseConfigurationRequest ::
  LaunchTemplateLicenseConfigurationRequest
mkLaunchTemplateLicenseConfigurationRequest =
  LaunchTemplateLicenseConfigurationRequest'
    { licenseConfigurationARN =
        Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlcrLicenseConfigurationARN :: Lens.Lens' LaunchTemplateLicenseConfigurationRequest (Lude.Maybe Lude.Text)
ltlcrLicenseConfigurationARN = Lens.lens (licenseConfigurationARN :: LaunchTemplateLicenseConfigurationRequest -> Lude.Maybe Lude.Text) (\s a -> s {licenseConfigurationARN = a} :: LaunchTemplateLicenseConfigurationRequest)
{-# DEPRECATED ltlcrLicenseConfigurationARN "Use generic-lens or generic-optics with 'licenseConfigurationARN' instead." #-}

instance Lude.ToQuery LaunchTemplateLicenseConfigurationRequest where
  toQuery LaunchTemplateLicenseConfigurationRequest' {..} =
    Lude.mconcat
      ["LicenseConfigurationArn" Lude.=: licenseConfigurationARN]
