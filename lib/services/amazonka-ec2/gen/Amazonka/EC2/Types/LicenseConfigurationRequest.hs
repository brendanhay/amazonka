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
-- Module      : Amazonka.EC2.Types.LicenseConfigurationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LicenseConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a license configuration.
--
-- /See:/ 'newLicenseConfigurationRequest' smart constructor.
data LicenseConfigurationRequest = LicenseConfigurationRequest'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'licenseConfigurationRequest_licenseConfigurationArn' - The Amazon Resource Name (ARN) of the license configuration.
newLicenseConfigurationRequest ::
  LicenseConfigurationRequest
newLicenseConfigurationRequest =
  LicenseConfigurationRequest'
    { licenseConfigurationArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
licenseConfigurationRequest_licenseConfigurationArn :: Lens.Lens' LicenseConfigurationRequest (Prelude.Maybe Prelude.Text)
licenseConfigurationRequest_licenseConfigurationArn = Lens.lens (\LicenseConfigurationRequest' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@LicenseConfigurationRequest' {} a -> s {licenseConfigurationArn = a} :: LicenseConfigurationRequest)

instance Prelude.Hashable LicenseConfigurationRequest where
  hashWithSalt _salt LicenseConfigurationRequest' {..} =
    _salt
      `Prelude.hashWithSalt` licenseConfigurationArn

instance Prelude.NFData LicenseConfigurationRequest where
  rnf LicenseConfigurationRequest' {..} =
    Prelude.rnf licenseConfigurationArn

instance Data.ToQuery LicenseConfigurationRequest where
  toQuery LicenseConfigurationRequest' {..} =
    Prelude.mconcat
      [ "LicenseConfigurationArn"
          Data.=: licenseConfigurationArn
      ]
