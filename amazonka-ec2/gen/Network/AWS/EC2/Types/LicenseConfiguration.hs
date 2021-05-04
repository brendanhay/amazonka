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
-- Module      : Network.AWS.EC2.Types.LicenseConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LicenseConfiguration where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a license configuration.
--
-- /See:/ 'newLicenseConfiguration' smart constructor.
data LicenseConfiguration = LicenseConfiguration'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LicenseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'licenseConfiguration_licenseConfigurationArn' - The Amazon Resource Name (ARN) of the license configuration.
newLicenseConfiguration ::
  LicenseConfiguration
newLicenseConfiguration =
  LicenseConfiguration'
    { licenseConfigurationArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
licenseConfiguration_licenseConfigurationArn :: Lens.Lens' LicenseConfiguration (Prelude.Maybe Prelude.Text)
licenseConfiguration_licenseConfigurationArn = Lens.lens (\LicenseConfiguration' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@LicenseConfiguration' {} a -> s {licenseConfigurationArn = a} :: LicenseConfiguration)

instance Prelude.FromXML LicenseConfiguration where
  parseXML x =
    LicenseConfiguration'
      Prelude.<$> (x Prelude..@? "licenseConfigurationArn")

instance Prelude.Hashable LicenseConfiguration

instance Prelude.NFData LicenseConfiguration
