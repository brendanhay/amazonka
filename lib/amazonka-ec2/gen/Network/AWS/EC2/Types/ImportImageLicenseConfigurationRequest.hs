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
    iilcrLicenseConfigurationArn,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The request information of license configurations.
--
-- /See:/ 'mkImportImageLicenseConfigurationRequest' smart constructor.
newtype ImportImageLicenseConfigurationRequest = ImportImageLicenseConfigurationRequest'
  { -- | The ARN of a license configuration.
    licenseConfigurationArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportImageLicenseConfigurationRequest' value with any optional fields omitted.
mkImportImageLicenseConfigurationRequest ::
  ImportImageLicenseConfigurationRequest
mkImportImageLicenseConfigurationRequest =
  ImportImageLicenseConfigurationRequest'
    { licenseConfigurationArn =
        Core.Nothing
    }

-- | The ARN of a license configuration.
--
-- /Note:/ Consider using 'licenseConfigurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilcrLicenseConfigurationArn :: Lens.Lens' ImportImageLicenseConfigurationRequest (Core.Maybe Types.String)
iilcrLicenseConfigurationArn = Lens.field @"licenseConfigurationArn"
{-# DEPRECATED iilcrLicenseConfigurationArn "Use generic-lens or generic-optics with 'licenseConfigurationArn' instead." #-}
