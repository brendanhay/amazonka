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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentEnvironmentLink
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentOptionSetting
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentTier

-- | Contains details about an Elastic Beanstalk environment.
--
-- /See:/ 'newAwsElasticBeanstalkEnvironmentDetails' smart constructor.
data AwsElasticBeanstalkEnvironmentDetails = AwsElasticBeanstalkEnvironmentDetails'
  { -- | The current operational status of the environment.
    status :: Prelude.Maybe Prelude.Text,
    -- | The URL to the CNAME for this environment.
    cname :: Prelude.Maybe Prelude.Text,
    -- | For load-balanced, autoscaling environments, the URL to the load
    -- balancer. For single-instance environments, the IP address of the
    -- instance.
    endpointUrl :: Prelude.Maybe Prelude.Text,
    -- | The configuration setting for the environment.
    optionSettings :: Prelude.Maybe [AwsElasticBeanstalkEnvironmentOptionSetting],
    -- | The date when this environment was last modified.
    dateUpdated :: Prelude.Maybe Prelude.Text,
    -- | The creation date for this environment.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The application version of the environment.
    versionLabel :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the platform version for the environment.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The tier of the environment.
    tier :: Prelude.Maybe AwsElasticBeanstalkEnvironmentTier,
    -- | The name of the environment.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the application that is associated with the environment.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the environment.
    environmentArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the solution stack that is deployed with the environment.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Links to other environments in the same group.
    environmentLinks :: Prelude.Maybe [AwsElasticBeanstalkEnvironmentEnvironmentLink],
    -- | A description of the environment.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticBeanstalkEnvironmentDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsElasticBeanstalkEnvironmentDetails_status' - The current operational status of the environment.
--
-- 'cname', 'awsElasticBeanstalkEnvironmentDetails_cname' - The URL to the CNAME for this environment.
--
-- 'endpointUrl', 'awsElasticBeanstalkEnvironmentDetails_endpointUrl' - For load-balanced, autoscaling environments, the URL to the load
-- balancer. For single-instance environments, the IP address of the
-- instance.
--
-- 'optionSettings', 'awsElasticBeanstalkEnvironmentDetails_optionSettings' - The configuration setting for the environment.
--
-- 'dateUpdated', 'awsElasticBeanstalkEnvironmentDetails_dateUpdated' - The date when this environment was last modified.
--
-- 'dateCreated', 'awsElasticBeanstalkEnvironmentDetails_dateCreated' - The creation date for this environment.
--
-- 'versionLabel', 'awsElasticBeanstalkEnvironmentDetails_versionLabel' - The application version of the environment.
--
-- 'platformArn', 'awsElasticBeanstalkEnvironmentDetails_platformArn' - The ARN of the platform version for the environment.
--
-- 'tier', 'awsElasticBeanstalkEnvironmentDetails_tier' - The tier of the environment.
--
-- 'environmentName', 'awsElasticBeanstalkEnvironmentDetails_environmentName' - The name of the environment.
--
-- 'applicationName', 'awsElasticBeanstalkEnvironmentDetails_applicationName' - The name of the application that is associated with the environment.
--
-- 'environmentArn', 'awsElasticBeanstalkEnvironmentDetails_environmentArn' - The ARN of the environment.
--
-- 'solutionStackName', 'awsElasticBeanstalkEnvironmentDetails_solutionStackName' - The name of the solution stack that is deployed with the environment.
--
-- 'environmentId', 'awsElasticBeanstalkEnvironmentDetails_environmentId' - The identifier of the environment.
--
-- 'environmentLinks', 'awsElasticBeanstalkEnvironmentDetails_environmentLinks' - Links to other environments in the same group.
--
-- 'description', 'awsElasticBeanstalkEnvironmentDetails_description' - A description of the environment.
newAwsElasticBeanstalkEnvironmentDetails ::
  AwsElasticBeanstalkEnvironmentDetails
newAwsElasticBeanstalkEnvironmentDetails =
  AwsElasticBeanstalkEnvironmentDetails'
    { status =
        Prelude.Nothing,
      cname = Prelude.Nothing,
      endpointUrl = Prelude.Nothing,
      optionSettings = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      versionLabel = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      tier = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      environmentArn = Prelude.Nothing,
      solutionStackName = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      environmentLinks = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The current operational status of the environment.
awsElasticBeanstalkEnvironmentDetails_status :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_status = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {status} -> status) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {status = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The URL to the CNAME for this environment.
awsElasticBeanstalkEnvironmentDetails_cname :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_cname = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {cname} -> cname) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {cname = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | For load-balanced, autoscaling environments, the URL to the load
-- balancer. For single-instance environments, the IP address of the
-- instance.
awsElasticBeanstalkEnvironmentDetails_endpointUrl :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_endpointUrl = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {endpointUrl} -> endpointUrl) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {endpointUrl = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The configuration setting for the environment.
awsElasticBeanstalkEnvironmentDetails_optionSettings :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe [AwsElasticBeanstalkEnvironmentOptionSetting])
awsElasticBeanstalkEnvironmentDetails_optionSettings = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {optionSettings} -> optionSettings) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {optionSettings = a} :: AwsElasticBeanstalkEnvironmentDetails) Prelude.. Lens.mapping Lens.coerced

-- | The date when this environment was last modified.
awsElasticBeanstalkEnvironmentDetails_dateUpdated :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_dateUpdated = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {dateUpdated} -> dateUpdated) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {dateUpdated = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The creation date for this environment.
awsElasticBeanstalkEnvironmentDetails_dateCreated :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_dateCreated = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {dateCreated} -> dateCreated) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {dateCreated = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The application version of the environment.
awsElasticBeanstalkEnvironmentDetails_versionLabel :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_versionLabel = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {versionLabel} -> versionLabel) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {versionLabel = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The ARN of the platform version for the environment.
awsElasticBeanstalkEnvironmentDetails_platformArn :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_platformArn = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {platformArn} -> platformArn) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {platformArn = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The tier of the environment.
awsElasticBeanstalkEnvironmentDetails_tier :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe AwsElasticBeanstalkEnvironmentTier)
awsElasticBeanstalkEnvironmentDetails_tier = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {tier} -> tier) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {tier = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The name of the environment.
awsElasticBeanstalkEnvironmentDetails_environmentName :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_environmentName = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {environmentName} -> environmentName) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {environmentName = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The name of the application that is associated with the environment.
awsElasticBeanstalkEnvironmentDetails_applicationName :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_applicationName = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {applicationName} -> applicationName) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {applicationName = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The ARN of the environment.
awsElasticBeanstalkEnvironmentDetails_environmentArn :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_environmentArn = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {environmentArn} -> environmentArn) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {environmentArn = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The name of the solution stack that is deployed with the environment.
awsElasticBeanstalkEnvironmentDetails_solutionStackName :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_solutionStackName = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {solutionStackName} -> solutionStackName) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {solutionStackName = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The identifier of the environment.
awsElasticBeanstalkEnvironmentDetails_environmentId :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_environmentId = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {environmentId} -> environmentId) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {environmentId = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | Links to other environments in the same group.
awsElasticBeanstalkEnvironmentDetails_environmentLinks :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe [AwsElasticBeanstalkEnvironmentEnvironmentLink])
awsElasticBeanstalkEnvironmentDetails_environmentLinks = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {environmentLinks} -> environmentLinks) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {environmentLinks = a} :: AwsElasticBeanstalkEnvironmentDetails) Prelude.. Lens.mapping Lens.coerced

-- | A description of the environment.
awsElasticBeanstalkEnvironmentDetails_description :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_description = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {description} -> description) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {description = a} :: AwsElasticBeanstalkEnvironmentDetails)

instance
  Core.FromJSON
    AwsElasticBeanstalkEnvironmentDetails
  where
  parseJSON =
    Core.withObject
      "AwsElasticBeanstalkEnvironmentDetails"
      ( \x ->
          AwsElasticBeanstalkEnvironmentDetails'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Cname")
            Prelude.<*> (x Core..:? "EndpointUrl")
            Prelude.<*> (x Core..:? "OptionSettings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DateUpdated")
            Prelude.<*> (x Core..:? "DateCreated")
            Prelude.<*> (x Core..:? "VersionLabel")
            Prelude.<*> (x Core..:? "PlatformArn")
            Prelude.<*> (x Core..:? "Tier")
            Prelude.<*> (x Core..:? "EnvironmentName")
            Prelude.<*> (x Core..:? "ApplicationName")
            Prelude.<*> (x Core..:? "EnvironmentArn")
            Prelude.<*> (x Core..:? "SolutionStackName")
            Prelude.<*> (x Core..:? "EnvironmentId")
            Prelude.<*> ( x Core..:? "EnvironmentLinks"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Description")
      )

instance
  Prelude.Hashable
    AwsElasticBeanstalkEnvironmentDetails

instance
  Prelude.NFData
    AwsElasticBeanstalkEnvironmentDetails

instance
  Core.ToJSON
    AwsElasticBeanstalkEnvironmentDetails
  where
  toJSON AwsElasticBeanstalkEnvironmentDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("Cname" Core..=) Prelude.<$> cname,
            ("EndpointUrl" Core..=) Prelude.<$> endpointUrl,
            ("OptionSettings" Core..=)
              Prelude.<$> optionSettings,
            ("DateUpdated" Core..=) Prelude.<$> dateUpdated,
            ("DateCreated" Core..=) Prelude.<$> dateCreated,
            ("VersionLabel" Core..=) Prelude.<$> versionLabel,
            ("PlatformArn" Core..=) Prelude.<$> platformArn,
            ("Tier" Core..=) Prelude.<$> tier,
            ("EnvironmentName" Core..=)
              Prelude.<$> environmentName,
            ("ApplicationName" Core..=)
              Prelude.<$> applicationName,
            ("EnvironmentArn" Core..=)
              Prelude.<$> environmentArn,
            ("SolutionStackName" Core..=)
              Prelude.<$> solutionStackName,
            ("EnvironmentId" Core..=) Prelude.<$> environmentId,
            ("EnvironmentLinks" Core..=)
              Prelude.<$> environmentLinks,
            ("Description" Core..=) Prelude.<$> description
          ]
      )
