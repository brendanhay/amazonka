{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LicenseConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LicenseConfigurationRequest
  ( LicenseConfigurationRequest (..),

    -- * Smart constructor
    mkLicenseConfigurationRequest,

    -- * Lenses
    lcrLicenseConfigurationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a license configuration.
--
-- /See:/ 'mkLicenseConfigurationRequest' smart constructor.
newtype LicenseConfigurationRequest = LicenseConfigurationRequest'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LicenseConfigurationRequest' with the minimum fields required to make a request.
--
-- * 'licenseConfigurationARN' - The Amazon Resource Name (ARN) of the license configuration.
mkLicenseConfigurationRequest ::
  LicenseConfigurationRequest
mkLicenseConfigurationRequest =
  LicenseConfigurationRequest'
    { licenseConfigurationARN =
        Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrLicenseConfigurationARN :: Lens.Lens' LicenseConfigurationRequest (Lude.Maybe Lude.Text)
lcrLicenseConfigurationARN = Lens.lens (licenseConfigurationARN :: LicenseConfigurationRequest -> Lude.Maybe Lude.Text) (\s a -> s {licenseConfigurationARN = a} :: LicenseConfigurationRequest)
{-# DEPRECATED lcrLicenseConfigurationARN "Use generic-lens or generic-optics with 'licenseConfigurationARN' instead." #-}

instance Lude.ToQuery LicenseConfigurationRequest where
  toQuery LicenseConfigurationRequest' {..} =
    Lude.mconcat
      ["LicenseConfigurationArn" Lude.=: licenseConfigurationARN]
