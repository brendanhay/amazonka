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
-- Module      : Network.AWS.EC2.Types.LicenseConfigurationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LicenseConfigurationRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a license configuration.
--
-- /See:/ 'newLicenseConfigurationRequest' smart constructor.
data LicenseConfigurationRequest = LicenseConfigurationRequest'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
licenseConfigurationRequest_licenseConfigurationArn :: Lens.Lens' LicenseConfigurationRequest (Core.Maybe Core.Text)
licenseConfigurationRequest_licenseConfigurationArn = Lens.lens (\LicenseConfigurationRequest' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@LicenseConfigurationRequest' {} a -> s {licenseConfigurationArn = a} :: LicenseConfigurationRequest)

instance Core.Hashable LicenseConfigurationRequest

instance Core.NFData LicenseConfigurationRequest

instance Core.ToQuery LicenseConfigurationRequest where
  toQuery LicenseConfigurationRequest' {..} =
    Core.mconcat
      [ "LicenseConfigurationArn"
          Core.=: licenseConfigurationArn
      ]
