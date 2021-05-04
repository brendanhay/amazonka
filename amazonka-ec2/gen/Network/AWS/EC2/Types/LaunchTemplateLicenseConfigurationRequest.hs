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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a license configuration.
--
-- /See:/ 'newLaunchTemplateLicenseConfigurationRequest' smart constructor.
data LaunchTemplateLicenseConfigurationRequest = LaunchTemplateLicenseConfigurationRequest'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateLicenseConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'launchTemplateLicenseConfigurationRequest_licenseConfigurationArn' - The Amazon Resource Name (ARN) of the license configuration.
newLaunchTemplateLicenseConfigurationRequest ::
  LaunchTemplateLicenseConfigurationRequest
newLaunchTemplateLicenseConfigurationRequest =
  LaunchTemplateLicenseConfigurationRequest'
    { licenseConfigurationArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
launchTemplateLicenseConfigurationRequest_licenseConfigurationArn :: Lens.Lens' LaunchTemplateLicenseConfigurationRequest (Prelude.Maybe Prelude.Text)
launchTemplateLicenseConfigurationRequest_licenseConfigurationArn = Lens.lens (\LaunchTemplateLicenseConfigurationRequest' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@LaunchTemplateLicenseConfigurationRequest' {} a -> s {licenseConfigurationArn = a} :: LaunchTemplateLicenseConfigurationRequest)

instance
  Prelude.Hashable
    LaunchTemplateLicenseConfigurationRequest

instance
  Prelude.NFData
    LaunchTemplateLicenseConfigurationRequest

instance
  Prelude.ToQuery
    LaunchTemplateLicenseConfigurationRequest
  where
  toQuery
    LaunchTemplateLicenseConfigurationRequest' {..} =
      Prelude.mconcat
        [ "LicenseConfigurationArn"
            Prelude.=: licenseConfigurationArn
        ]
