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
-- Module      : Amazonka.ImageBuilder.Types.Distribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Distribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.AmiDistributionConfiguration
import Amazonka.ImageBuilder.Types.ContainerDistributionConfiguration
import Amazonka.ImageBuilder.Types.FastLaunchConfiguration
import Amazonka.ImageBuilder.Types.LaunchTemplateConfiguration
import Amazonka.ImageBuilder.Types.S3ExportConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Defines the settings for a specific Region.
--
-- /See:/ 'newDistribution' smart constructor.
data Distribution = Distribution'
  { -- | The specific AMI settings; for example, launch permissions or AMI tags.
    amiDistributionConfiguration :: Prelude.Maybe AmiDistributionConfiguration,
    -- | Container distribution settings for encryption, licensing, and sharing
    -- in a specific Region.
    containerDistributionConfiguration :: Prelude.Maybe ContainerDistributionConfiguration,
    -- | The Windows faster-launching configurations to use for AMI distribution.
    fastLaunchConfigurations :: Prelude.Maybe (Prelude.NonEmpty FastLaunchConfiguration),
    -- | A group of launchTemplateConfiguration settings that apply to image
    -- distribution for specified accounts.
    launchTemplateConfigurations :: Prelude.Maybe (Prelude.NonEmpty LaunchTemplateConfiguration),
    -- | The License Manager Configuration to associate with the AMI in the
    -- specified Region.
    licenseConfigurationArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Configure export settings to deliver disk images created from your image
    -- build, using a file format that is compatible with your VMs in that
    -- Region.
    s3ExportConfiguration :: Prelude.Maybe S3ExportConfiguration,
    -- | The target Region.
    region :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Distribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amiDistributionConfiguration', 'distribution_amiDistributionConfiguration' - The specific AMI settings; for example, launch permissions or AMI tags.
--
-- 'containerDistributionConfiguration', 'distribution_containerDistributionConfiguration' - Container distribution settings for encryption, licensing, and sharing
-- in a specific Region.
--
-- 'fastLaunchConfigurations', 'distribution_fastLaunchConfigurations' - The Windows faster-launching configurations to use for AMI distribution.
--
-- 'launchTemplateConfigurations', 'distribution_launchTemplateConfigurations' - A group of launchTemplateConfiguration settings that apply to image
-- distribution for specified accounts.
--
-- 'licenseConfigurationArns', 'distribution_licenseConfigurationArns' - The License Manager Configuration to associate with the AMI in the
-- specified Region.
--
-- 's3ExportConfiguration', 'distribution_s3ExportConfiguration' - Configure export settings to deliver disk images created from your image
-- build, using a file format that is compatible with your VMs in that
-- Region.
--
-- 'region', 'distribution_region' - The target Region.
newDistribution ::
  -- | 'region'
  Prelude.Text ->
  Distribution
newDistribution pRegion_ =
  Distribution'
    { amiDistributionConfiguration =
        Prelude.Nothing,
      containerDistributionConfiguration = Prelude.Nothing,
      fastLaunchConfigurations = Prelude.Nothing,
      launchTemplateConfigurations = Prelude.Nothing,
      licenseConfigurationArns = Prelude.Nothing,
      s3ExportConfiguration = Prelude.Nothing,
      region = pRegion_
    }

-- | The specific AMI settings; for example, launch permissions or AMI tags.
distribution_amiDistributionConfiguration :: Lens.Lens' Distribution (Prelude.Maybe AmiDistributionConfiguration)
distribution_amiDistributionConfiguration = Lens.lens (\Distribution' {amiDistributionConfiguration} -> amiDistributionConfiguration) (\s@Distribution' {} a -> s {amiDistributionConfiguration = a} :: Distribution)

-- | Container distribution settings for encryption, licensing, and sharing
-- in a specific Region.
distribution_containerDistributionConfiguration :: Lens.Lens' Distribution (Prelude.Maybe ContainerDistributionConfiguration)
distribution_containerDistributionConfiguration = Lens.lens (\Distribution' {containerDistributionConfiguration} -> containerDistributionConfiguration) (\s@Distribution' {} a -> s {containerDistributionConfiguration = a} :: Distribution)

-- | The Windows faster-launching configurations to use for AMI distribution.
distribution_fastLaunchConfigurations :: Lens.Lens' Distribution (Prelude.Maybe (Prelude.NonEmpty FastLaunchConfiguration))
distribution_fastLaunchConfigurations = Lens.lens (\Distribution' {fastLaunchConfigurations} -> fastLaunchConfigurations) (\s@Distribution' {} a -> s {fastLaunchConfigurations = a} :: Distribution) Prelude.. Lens.mapping Lens.coerced

-- | A group of launchTemplateConfiguration settings that apply to image
-- distribution for specified accounts.
distribution_launchTemplateConfigurations :: Lens.Lens' Distribution (Prelude.Maybe (Prelude.NonEmpty LaunchTemplateConfiguration))
distribution_launchTemplateConfigurations = Lens.lens (\Distribution' {launchTemplateConfigurations} -> launchTemplateConfigurations) (\s@Distribution' {} a -> s {launchTemplateConfigurations = a} :: Distribution) Prelude.. Lens.mapping Lens.coerced

-- | The License Manager Configuration to associate with the AMI in the
-- specified Region.
distribution_licenseConfigurationArns :: Lens.Lens' Distribution (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
distribution_licenseConfigurationArns = Lens.lens (\Distribution' {licenseConfigurationArns} -> licenseConfigurationArns) (\s@Distribution' {} a -> s {licenseConfigurationArns = a} :: Distribution) Prelude.. Lens.mapping Lens.coerced

-- | Configure export settings to deliver disk images created from your image
-- build, using a file format that is compatible with your VMs in that
-- Region.
distribution_s3ExportConfiguration :: Lens.Lens' Distribution (Prelude.Maybe S3ExportConfiguration)
distribution_s3ExportConfiguration = Lens.lens (\Distribution' {s3ExportConfiguration} -> s3ExportConfiguration) (\s@Distribution' {} a -> s {s3ExportConfiguration = a} :: Distribution)

-- | The target Region.
distribution_region :: Lens.Lens' Distribution Prelude.Text
distribution_region = Lens.lens (\Distribution' {region} -> region) (\s@Distribution' {} a -> s {region = a} :: Distribution)

instance Data.FromJSON Distribution where
  parseJSON =
    Data.withObject
      "Distribution"
      ( \x ->
          Distribution'
            Prelude.<$> (x Data..:? "amiDistributionConfiguration")
            Prelude.<*> (x Data..:? "containerDistributionConfiguration")
            Prelude.<*> (x Data..:? "fastLaunchConfigurations")
            Prelude.<*> (x Data..:? "launchTemplateConfigurations")
            Prelude.<*> (x Data..:? "licenseConfigurationArns")
            Prelude.<*> (x Data..:? "s3ExportConfiguration")
            Prelude.<*> (x Data..: "region")
      )

instance Prelude.Hashable Distribution where
  hashWithSalt _salt Distribution' {..} =
    _salt
      `Prelude.hashWithSalt` amiDistributionConfiguration
      `Prelude.hashWithSalt` containerDistributionConfiguration
      `Prelude.hashWithSalt` fastLaunchConfigurations
      `Prelude.hashWithSalt` launchTemplateConfigurations
      `Prelude.hashWithSalt` licenseConfigurationArns
      `Prelude.hashWithSalt` s3ExportConfiguration
      `Prelude.hashWithSalt` region

instance Prelude.NFData Distribution where
  rnf Distribution' {..} =
    Prelude.rnf amiDistributionConfiguration
      `Prelude.seq` Prelude.rnf containerDistributionConfiguration
      `Prelude.seq` Prelude.rnf fastLaunchConfigurations
      `Prelude.seq` Prelude.rnf launchTemplateConfigurations
      `Prelude.seq` Prelude.rnf licenseConfigurationArns
      `Prelude.seq` Prelude.rnf s3ExportConfiguration
      `Prelude.seq` Prelude.rnf region

instance Data.ToJSON Distribution where
  toJSON Distribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("amiDistributionConfiguration" Data..=)
              Prelude.<$> amiDistributionConfiguration,
            ("containerDistributionConfiguration" Data..=)
              Prelude.<$> containerDistributionConfiguration,
            ("fastLaunchConfigurations" Data..=)
              Prelude.<$> fastLaunchConfigurations,
            ("launchTemplateConfigurations" Data..=)
              Prelude.<$> launchTemplateConfigurations,
            ("licenseConfigurationArns" Data..=)
              Prelude.<$> licenseConfigurationArns,
            ("s3ExportConfiguration" Data..=)
              Prelude.<$> s3ExportConfiguration,
            Prelude.Just ("region" Data..= region)
          ]
      )
