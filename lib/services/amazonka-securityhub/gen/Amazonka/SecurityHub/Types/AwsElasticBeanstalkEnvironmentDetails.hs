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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentEnvironmentLink
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentOptionSetting
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentTier

-- | Contains details about an Elastic Beanstalk environment.
--
-- /See:/ 'newAwsElasticBeanstalkEnvironmentDetails' smart constructor.
data AwsElasticBeanstalkEnvironmentDetails = AwsElasticBeanstalkEnvironmentDetails'
  { -- | The name of the application that is associated with the environment.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The URL to the CNAME for this environment.
    cname :: Prelude.Maybe Prelude.Text,
    -- | The creation date for this environment.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The date when this environment was last modified.
    dateUpdated :: Prelude.Maybe Prelude.Text,
    -- | A description of the environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | For load-balanced, autoscaling environments, the URL to the load
    -- balancer. For single-instance environments, the IP address of the
    -- instance.
    endpointUrl :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the environment.
    environmentArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Links to other environments in the same group.
    environmentLinks :: Prelude.Maybe [AwsElasticBeanstalkEnvironmentEnvironmentLink],
    -- | The name of the environment.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The configuration setting for the environment.
    optionSettings :: Prelude.Maybe [AwsElasticBeanstalkEnvironmentOptionSetting],
    -- | The ARN of the platform version for the environment.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the solution stack that is deployed with the environment.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | The current operational status of the environment. Valid values are as
    -- follows:
    --
    -- -   @Aborting@
    --
    -- -   @Launching@
    --
    -- -   @LinkingFrom@
    --
    -- -   @LinkingTo@
    --
    -- -   @Ready@
    --
    -- -   @Terminated@
    --
    -- -   @Terminating@
    --
    -- -   @Updating@
    status :: Prelude.Maybe Prelude.Text,
    -- | The tier of the environment.
    tier :: Prelude.Maybe AwsElasticBeanstalkEnvironmentTier,
    -- | The application version of the environment.
    versionLabel :: Prelude.Maybe Prelude.Text
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
-- 'applicationName', 'awsElasticBeanstalkEnvironmentDetails_applicationName' - The name of the application that is associated with the environment.
--
-- 'cname', 'awsElasticBeanstalkEnvironmentDetails_cname' - The URL to the CNAME for this environment.
--
-- 'dateCreated', 'awsElasticBeanstalkEnvironmentDetails_dateCreated' - The creation date for this environment.
--
-- 'dateUpdated', 'awsElasticBeanstalkEnvironmentDetails_dateUpdated' - The date when this environment was last modified.
--
-- 'description', 'awsElasticBeanstalkEnvironmentDetails_description' - A description of the environment.
--
-- 'endpointUrl', 'awsElasticBeanstalkEnvironmentDetails_endpointUrl' - For load-balanced, autoscaling environments, the URL to the load
-- balancer. For single-instance environments, the IP address of the
-- instance.
--
-- 'environmentArn', 'awsElasticBeanstalkEnvironmentDetails_environmentArn' - The ARN of the environment.
--
-- 'environmentId', 'awsElasticBeanstalkEnvironmentDetails_environmentId' - The identifier of the environment.
--
-- 'environmentLinks', 'awsElasticBeanstalkEnvironmentDetails_environmentLinks' - Links to other environments in the same group.
--
-- 'environmentName', 'awsElasticBeanstalkEnvironmentDetails_environmentName' - The name of the environment.
--
-- 'optionSettings', 'awsElasticBeanstalkEnvironmentDetails_optionSettings' - The configuration setting for the environment.
--
-- 'platformArn', 'awsElasticBeanstalkEnvironmentDetails_platformArn' - The ARN of the platform version for the environment.
--
-- 'solutionStackName', 'awsElasticBeanstalkEnvironmentDetails_solutionStackName' - The name of the solution stack that is deployed with the environment.
--
-- 'status', 'awsElasticBeanstalkEnvironmentDetails_status' - The current operational status of the environment. Valid values are as
-- follows:
--
-- -   @Aborting@
--
-- -   @Launching@
--
-- -   @LinkingFrom@
--
-- -   @LinkingTo@
--
-- -   @Ready@
--
-- -   @Terminated@
--
-- -   @Terminating@
--
-- -   @Updating@
--
-- 'tier', 'awsElasticBeanstalkEnvironmentDetails_tier' - The tier of the environment.
--
-- 'versionLabel', 'awsElasticBeanstalkEnvironmentDetails_versionLabel' - The application version of the environment.
newAwsElasticBeanstalkEnvironmentDetails ::
  AwsElasticBeanstalkEnvironmentDetails
newAwsElasticBeanstalkEnvironmentDetails =
  AwsElasticBeanstalkEnvironmentDetails'
    { applicationName =
        Prelude.Nothing,
      cname = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      description = Prelude.Nothing,
      endpointUrl = Prelude.Nothing,
      environmentArn = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      environmentLinks = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      optionSettings = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      solutionStackName = Prelude.Nothing,
      status = Prelude.Nothing,
      tier = Prelude.Nothing,
      versionLabel = Prelude.Nothing
    }

-- | The name of the application that is associated with the environment.
awsElasticBeanstalkEnvironmentDetails_applicationName :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_applicationName = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {applicationName} -> applicationName) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {applicationName = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The URL to the CNAME for this environment.
awsElasticBeanstalkEnvironmentDetails_cname :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_cname = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {cname} -> cname) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {cname = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The creation date for this environment.
awsElasticBeanstalkEnvironmentDetails_dateCreated :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_dateCreated = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {dateCreated} -> dateCreated) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {dateCreated = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The date when this environment was last modified.
awsElasticBeanstalkEnvironmentDetails_dateUpdated :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_dateUpdated = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {dateUpdated} -> dateUpdated) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {dateUpdated = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | A description of the environment.
awsElasticBeanstalkEnvironmentDetails_description :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_description = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {description} -> description) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {description = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | For load-balanced, autoscaling environments, the URL to the load
-- balancer. For single-instance environments, the IP address of the
-- instance.
awsElasticBeanstalkEnvironmentDetails_endpointUrl :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_endpointUrl = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {endpointUrl} -> endpointUrl) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {endpointUrl = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The ARN of the environment.
awsElasticBeanstalkEnvironmentDetails_environmentArn :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_environmentArn = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {environmentArn} -> environmentArn) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {environmentArn = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The identifier of the environment.
awsElasticBeanstalkEnvironmentDetails_environmentId :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_environmentId = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {environmentId} -> environmentId) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {environmentId = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | Links to other environments in the same group.
awsElasticBeanstalkEnvironmentDetails_environmentLinks :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe [AwsElasticBeanstalkEnvironmentEnvironmentLink])
awsElasticBeanstalkEnvironmentDetails_environmentLinks = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {environmentLinks} -> environmentLinks) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {environmentLinks = a} :: AwsElasticBeanstalkEnvironmentDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the environment.
awsElasticBeanstalkEnvironmentDetails_environmentName :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_environmentName = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {environmentName} -> environmentName) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {environmentName = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The configuration setting for the environment.
awsElasticBeanstalkEnvironmentDetails_optionSettings :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe [AwsElasticBeanstalkEnvironmentOptionSetting])
awsElasticBeanstalkEnvironmentDetails_optionSettings = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {optionSettings} -> optionSettings) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {optionSettings = a} :: AwsElasticBeanstalkEnvironmentDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the platform version for the environment.
awsElasticBeanstalkEnvironmentDetails_platformArn :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_platformArn = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {platformArn} -> platformArn) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {platformArn = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The name of the solution stack that is deployed with the environment.
awsElasticBeanstalkEnvironmentDetails_solutionStackName :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_solutionStackName = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {solutionStackName} -> solutionStackName) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {solutionStackName = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The current operational status of the environment. Valid values are as
-- follows:
--
-- -   @Aborting@
--
-- -   @Launching@
--
-- -   @LinkingFrom@
--
-- -   @LinkingTo@
--
-- -   @Ready@
--
-- -   @Terminated@
--
-- -   @Terminating@
--
-- -   @Updating@
awsElasticBeanstalkEnvironmentDetails_status :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_status = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {status} -> status) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {status = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The tier of the environment.
awsElasticBeanstalkEnvironmentDetails_tier :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe AwsElasticBeanstalkEnvironmentTier)
awsElasticBeanstalkEnvironmentDetails_tier = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {tier} -> tier) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {tier = a} :: AwsElasticBeanstalkEnvironmentDetails)

-- | The application version of the environment.
awsElasticBeanstalkEnvironmentDetails_versionLabel :: Lens.Lens' AwsElasticBeanstalkEnvironmentDetails (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentDetails_versionLabel = Lens.lens (\AwsElasticBeanstalkEnvironmentDetails' {versionLabel} -> versionLabel) (\s@AwsElasticBeanstalkEnvironmentDetails' {} a -> s {versionLabel = a} :: AwsElasticBeanstalkEnvironmentDetails)

instance
  Data.FromJSON
    AwsElasticBeanstalkEnvironmentDetails
  where
  parseJSON =
    Data.withObject
      "AwsElasticBeanstalkEnvironmentDetails"
      ( \x ->
          AwsElasticBeanstalkEnvironmentDetails'
            Prelude.<$> (x Data..:? "ApplicationName")
            Prelude.<*> (x Data..:? "Cname")
            Prelude.<*> (x Data..:? "DateCreated")
            Prelude.<*> (x Data..:? "DateUpdated")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EndpointUrl")
            Prelude.<*> (x Data..:? "EnvironmentArn")
            Prelude.<*> (x Data..:? "EnvironmentId")
            Prelude.<*> ( x
                            Data..:? "EnvironmentLinks"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EnvironmentName")
            Prelude.<*> (x Data..:? "OptionSettings" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PlatformArn")
            Prelude.<*> (x Data..:? "SolutionStackName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Tier")
            Prelude.<*> (x Data..:? "VersionLabel")
      )

instance
  Prelude.Hashable
    AwsElasticBeanstalkEnvironmentDetails
  where
  hashWithSalt
    _salt
    AwsElasticBeanstalkEnvironmentDetails' {..} =
      _salt
        `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` cname
        `Prelude.hashWithSalt` dateCreated
        `Prelude.hashWithSalt` dateUpdated
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` endpointUrl
        `Prelude.hashWithSalt` environmentArn
        `Prelude.hashWithSalt` environmentId
        `Prelude.hashWithSalt` environmentLinks
        `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` optionSettings
        `Prelude.hashWithSalt` platformArn
        `Prelude.hashWithSalt` solutionStackName
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` tier
        `Prelude.hashWithSalt` versionLabel

instance
  Prelude.NFData
    AwsElasticBeanstalkEnvironmentDetails
  where
  rnf AwsElasticBeanstalkEnvironmentDetails' {..} =
    Prelude.rnf applicationName `Prelude.seq`
      Prelude.rnf cname `Prelude.seq`
        Prelude.rnf dateCreated `Prelude.seq`
          Prelude.rnf dateUpdated `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf endpointUrl `Prelude.seq`
                Prelude.rnf environmentArn `Prelude.seq`
                  Prelude.rnf environmentId `Prelude.seq`
                    Prelude.rnf environmentLinks `Prelude.seq`
                      Prelude.rnf environmentName `Prelude.seq`
                        Prelude.rnf optionSettings `Prelude.seq`
                          Prelude.rnf platformArn `Prelude.seq`
                            Prelude.rnf solutionStackName `Prelude.seq`
                              Prelude.rnf status `Prelude.seq`
                                Prelude.rnf tier `Prelude.seq`
                                  Prelude.rnf versionLabel

instance
  Data.ToJSON
    AwsElasticBeanstalkEnvironmentDetails
  where
  toJSON AwsElasticBeanstalkEnvironmentDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationName" Data..=)
              Prelude.<$> applicationName,
            ("Cname" Data..=) Prelude.<$> cname,
            ("DateCreated" Data..=) Prelude.<$> dateCreated,
            ("DateUpdated" Data..=) Prelude.<$> dateUpdated,
            ("Description" Data..=) Prelude.<$> description,
            ("EndpointUrl" Data..=) Prelude.<$> endpointUrl,
            ("EnvironmentArn" Data..=)
              Prelude.<$> environmentArn,
            ("EnvironmentId" Data..=) Prelude.<$> environmentId,
            ("EnvironmentLinks" Data..=)
              Prelude.<$> environmentLinks,
            ("EnvironmentName" Data..=)
              Prelude.<$> environmentName,
            ("OptionSettings" Data..=)
              Prelude.<$> optionSettings,
            ("PlatformArn" Data..=) Prelude.<$> platformArn,
            ("SolutionStackName" Data..=)
              Prelude.<$> solutionStackName,
            ("Status" Data..=) Prelude.<$> status,
            ("Tier" Data..=) Prelude.<$> tier,
            ("VersionLabel" Data..=) Prelude.<$> versionLabel
          ]
      )
