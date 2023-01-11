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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataLicenseSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataLicenseSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the license configuration for an Amazon EC2
-- instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataLicenseSetDetails' smart constructor.
data AwsEc2LaunchTemplateDataLicenseSetDetails = AwsEc2LaunchTemplateDataLicenseSetDetails'
  { -- | The Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataLicenseSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'awsEc2LaunchTemplateDataLicenseSetDetails_licenseConfigurationArn' - The Amazon Resource Name (ARN) of the license configuration.
newAwsEc2LaunchTemplateDataLicenseSetDetails ::
  AwsEc2LaunchTemplateDataLicenseSetDetails
newAwsEc2LaunchTemplateDataLicenseSetDetails =
  AwsEc2LaunchTemplateDataLicenseSetDetails'
    { licenseConfigurationArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the license configuration.
awsEc2LaunchTemplateDataLicenseSetDetails_licenseConfigurationArn :: Lens.Lens' AwsEc2LaunchTemplateDataLicenseSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataLicenseSetDetails_licenseConfigurationArn = Lens.lens (\AwsEc2LaunchTemplateDataLicenseSetDetails' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@AwsEc2LaunchTemplateDataLicenseSetDetails' {} a -> s {licenseConfigurationArn = a} :: AwsEc2LaunchTemplateDataLicenseSetDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataLicenseSetDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataLicenseSetDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataLicenseSetDetails'
            Prelude.<$> (x Data..:? "LicenseConfigurationArn")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataLicenseSetDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataLicenseSetDetails' {..} =
      _salt
        `Prelude.hashWithSalt` licenseConfigurationArn

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataLicenseSetDetails
  where
  rnf AwsEc2LaunchTemplateDataLicenseSetDetails' {..} =
    Prelude.rnf licenseConfigurationArn

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataLicenseSetDetails
  where
  toJSON AwsEc2LaunchTemplateDataLicenseSetDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LicenseConfigurationArn" Data..=)
              Prelude.<$> licenseConfigurationArn
          ]
      )
