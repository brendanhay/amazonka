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
-- Module      : Network.AWS.EC2.Types.ImportImageLicenseConfigurationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportImageLicenseConfigurationRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The request information of license configurations.
--
-- /See:/ 'newImportImageLicenseConfigurationRequest' smart constructor.
data ImportImageLicenseConfigurationRequest = ImportImageLicenseConfigurationRequest'
  { -- | The ARN of a license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportImageLicenseConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'importImageLicenseConfigurationRequest_licenseConfigurationArn' - The ARN of a license configuration.
newImportImageLicenseConfigurationRequest ::
  ImportImageLicenseConfigurationRequest
newImportImageLicenseConfigurationRequest =
  ImportImageLicenseConfigurationRequest'
    { licenseConfigurationArn =
        Prelude.Nothing
    }

-- | The ARN of a license configuration.
importImageLicenseConfigurationRequest_licenseConfigurationArn :: Lens.Lens' ImportImageLicenseConfigurationRequest (Prelude.Maybe Prelude.Text)
importImageLicenseConfigurationRequest_licenseConfigurationArn = Lens.lens (\ImportImageLicenseConfigurationRequest' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@ImportImageLicenseConfigurationRequest' {} a -> s {licenseConfigurationArn = a} :: ImportImageLicenseConfigurationRequest)

instance
  Prelude.Hashable
    ImportImageLicenseConfigurationRequest

instance
  Prelude.NFData
    ImportImageLicenseConfigurationRequest

instance
  Prelude.ToQuery
    ImportImageLicenseConfigurationRequest
  where
  toQuery ImportImageLicenseConfigurationRequest' {..} =
    Prelude.mconcat
      [ "LicenseConfigurationArn"
          Prelude.=: licenseConfigurationArn
      ]
