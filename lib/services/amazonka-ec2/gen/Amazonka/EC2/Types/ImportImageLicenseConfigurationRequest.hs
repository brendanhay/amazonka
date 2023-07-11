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
-- Module      : Amazonka.EC2.Types.ImportImageLicenseConfigurationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ImportImageLicenseConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The request information of license configurations.
--
-- /See:/ 'newImportImageLicenseConfigurationRequest' smart constructor.
data ImportImageLicenseConfigurationRequest = ImportImageLicenseConfigurationRequest'
  { -- | The ARN of a license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  hashWithSalt
    _salt
    ImportImageLicenseConfigurationRequest' {..} =
      _salt
        `Prelude.hashWithSalt` licenseConfigurationArn

instance
  Prelude.NFData
    ImportImageLicenseConfigurationRequest
  where
  rnf ImportImageLicenseConfigurationRequest' {..} =
    Prelude.rnf licenseConfigurationArn

instance
  Data.ToQuery
    ImportImageLicenseConfigurationRequest
  where
  toQuery ImportImageLicenseConfigurationRequest' {..} =
    Prelude.mconcat
      [ "LicenseConfigurationArn"
          Data.=: licenseConfigurationArn
      ]
