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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a license configuration.
--
-- /See:/ 'newLaunchTemplateLicenseConfiguration' smart constructor.
data LaunchTemplateLicenseConfiguration = LaunchTemplateLicenseConfiguration'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateLicenseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'launchTemplateLicenseConfiguration_licenseConfigurationArn' - The Amazon Resource Name (ARN) of the license configuration.
newLaunchTemplateLicenseConfiguration ::
  LaunchTemplateLicenseConfiguration
newLaunchTemplateLicenseConfiguration =
  LaunchTemplateLicenseConfiguration'
    { licenseConfigurationArn =
        Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
launchTemplateLicenseConfiguration_licenseConfigurationArn :: Lens.Lens' LaunchTemplateLicenseConfiguration (Core.Maybe Core.Text)
launchTemplateLicenseConfiguration_licenseConfigurationArn = Lens.lens (\LaunchTemplateLicenseConfiguration' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@LaunchTemplateLicenseConfiguration' {} a -> s {licenseConfigurationArn = a} :: LaunchTemplateLicenseConfiguration)

instance
  Core.FromXML
    LaunchTemplateLicenseConfiguration
  where
  parseXML x =
    LaunchTemplateLicenseConfiguration'
      Core.<$> (x Core..@? "licenseConfigurationArn")

instance
  Core.Hashable
    LaunchTemplateLicenseConfiguration

instance
  Core.NFData
    LaunchTemplateLicenseConfiguration
