{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The response information for license configurations.
--
-- /See:/ 'newImportImageLicenseConfigurationResponse' smart constructor.
data ImportImageLicenseConfigurationResponse = ImportImageLicenseConfigurationResponse'
  { -- | The ARN of a license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The ARN of a license configuration.
importImageLicenseConfigurationResponse_licenseConfigurationArn :: Lens.Lens' ImportImageLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
importImageLicenseConfigurationResponse_licenseConfigurationArn = Lens.lens (\ImportImageLicenseConfigurationResponse' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@ImportImageLicenseConfigurationResponse' {} a -> s {licenseConfigurationArn = a} :: ImportImageLicenseConfigurationResponse)

instance
  Prelude.FromXML
    ImportImageLicenseConfigurationResponse
  where
  parseXML x =
    ImportImageLicenseConfigurationResponse'
      Prelude.<$> (x Prelude..@? "licenseConfigurationArn")

instance
  Prelude.Hashable
    ImportImageLicenseConfigurationResponse

instance
  Prelude.NFData
    ImportImageLicenseConfigurationResponse
