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
-- Module      : Amazonka.EC2.Types.LaunchTemplateLicenseConfigurationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateLicenseConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a license configuration.
--
-- /See:/ 'newLaunchTemplateLicenseConfigurationRequest' smart constructor.
data LaunchTemplateLicenseConfigurationRequest = LaunchTemplateLicenseConfigurationRequest'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  hashWithSalt
    _salt
    LaunchTemplateLicenseConfigurationRequest' {..} =
      _salt
        `Prelude.hashWithSalt` licenseConfigurationArn

instance
  Prelude.NFData
    LaunchTemplateLicenseConfigurationRequest
  where
  rnf LaunchTemplateLicenseConfigurationRequest' {..} =
    Prelude.rnf licenseConfigurationArn

instance
  Core.ToQuery
    LaunchTemplateLicenseConfigurationRequest
  where
  toQuery
    LaunchTemplateLicenseConfigurationRequest' {..} =
      Prelude.mconcat
        [ "LicenseConfigurationArn"
            Core.=: licenseConfigurationArn
        ]
