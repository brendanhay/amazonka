{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportImageLicenseConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportImageLicenseConfigurationRequest
  ( ImportImageLicenseConfigurationRequest (..),

    -- * Smart constructor
    mkImportImageLicenseConfigurationRequest,

    -- * Lenses
    iilcrLicenseConfigurationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The request information of license configurations.
--
-- /See:/ 'mkImportImageLicenseConfigurationRequest' smart constructor.
newtype ImportImageLicenseConfigurationRequest = ImportImageLicenseConfigurationRequest'
  { -- | The ARN of a license configuration.
    licenseConfigurationARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportImageLicenseConfigurationRequest' with the minimum fields required to make a request.
--
-- * 'licenseConfigurationARN' - The ARN of a license configuration.
mkImportImageLicenseConfigurationRequest ::
  ImportImageLicenseConfigurationRequest
mkImportImageLicenseConfigurationRequest =
  ImportImageLicenseConfigurationRequest'
    { licenseConfigurationARN =
        Lude.Nothing
    }

-- | The ARN of a license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilcrLicenseConfigurationARN :: Lens.Lens' ImportImageLicenseConfigurationRequest (Lude.Maybe Lude.Text)
iilcrLicenseConfigurationARN = Lens.lens (licenseConfigurationARN :: ImportImageLicenseConfigurationRequest -> Lude.Maybe Lude.Text) (\s a -> s {licenseConfigurationARN = a} :: ImportImageLicenseConfigurationRequest)
{-# DEPRECATED iilcrLicenseConfigurationARN "Use generic-lens or generic-optics with 'licenseConfigurationARN' instead." #-}

instance Lude.ToQuery ImportImageLicenseConfigurationRequest where
  toQuery ImportImageLicenseConfigurationRequest' {..} =
    Lude.mconcat
      ["LicenseConfigurationArn" Lude.=: licenseConfigurationARN]
