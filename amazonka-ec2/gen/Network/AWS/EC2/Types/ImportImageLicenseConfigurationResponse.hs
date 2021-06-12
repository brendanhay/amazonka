{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The response information for license configurations.
--
-- /See:/ 'newImportImageLicenseConfigurationResponse' smart constructor.
data ImportImageLicenseConfigurationResponse = ImportImageLicenseConfigurationResponse'
  { -- | The ARN of a license configuration.
    licenseConfigurationArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportImageLicenseConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'importImageLicenseConfigurationResponse_licenseConfigurationArn' - The ARN of a license configuration.
newImportImageLicenseConfigurationResponse ::
  ImportImageLicenseConfigurationResponse
newImportImageLicenseConfigurationResponse =
  ImportImageLicenseConfigurationResponse'
    { licenseConfigurationArn =
        Core.Nothing
    }

-- | The ARN of a license configuration.
importImageLicenseConfigurationResponse_licenseConfigurationArn :: Lens.Lens' ImportImageLicenseConfigurationResponse (Core.Maybe Core.Text)
importImageLicenseConfigurationResponse_licenseConfigurationArn = Lens.lens (\ImportImageLicenseConfigurationResponse' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@ImportImageLicenseConfigurationResponse' {} a -> s {licenseConfigurationArn = a} :: ImportImageLicenseConfigurationResponse)

instance
  Core.FromXML
    ImportImageLicenseConfigurationResponse
  where
  parseXML x =
    ImportImageLicenseConfigurationResponse'
      Core.<$> (x Core..@? "licenseConfigurationArn")

instance
  Core.Hashable
    ImportImageLicenseConfigurationResponse

instance
  Core.NFData
    ImportImageLicenseConfigurationResponse
