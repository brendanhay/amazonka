{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
  ( ImportImageLicenseConfigurationResponse (..),

    -- * Smart constructor
    mkImportImageLicenseConfigurationResponse,

    -- * Lenses
    iilcLicenseConfigurationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The response information for license configurations.
--
-- /See:/ 'mkImportImageLicenseConfigurationResponse' smart constructor.
newtype ImportImageLicenseConfigurationResponse = ImportImageLicenseConfigurationResponse'
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

-- | Creates a value of 'ImportImageLicenseConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'licenseConfigurationARN' - The ARN of a license configuration.
mkImportImageLicenseConfigurationResponse ::
  ImportImageLicenseConfigurationResponse
mkImportImageLicenseConfigurationResponse =
  ImportImageLicenseConfigurationResponse'
    { licenseConfigurationARN =
        Lude.Nothing
    }

-- | The ARN of a license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilcLicenseConfigurationARN :: Lens.Lens' ImportImageLicenseConfigurationResponse (Lude.Maybe Lude.Text)
iilcLicenseConfigurationARN = Lens.lens (licenseConfigurationARN :: ImportImageLicenseConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {licenseConfigurationARN = a} :: ImportImageLicenseConfigurationResponse)
{-# DEPRECATED iilcLicenseConfigurationARN "Use generic-lens or generic-optics with 'licenseConfigurationARN' instead." #-}

instance Lude.FromXML ImportImageLicenseConfigurationResponse where
  parseXML x =
    ImportImageLicenseConfigurationResponse'
      Lude.<$> (x Lude..@? "licenseConfigurationArn")
