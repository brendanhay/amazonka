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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription where

import Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the settings for a configuration set.
--
-- /See:/ 'newConfigurationSettingsDescription' smart constructor.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription'
  { -- | If not @null@, the name of the configuration template for this
    -- configuration set.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The date (in UTC time) when this configuration set was created.
    dateCreated :: Prelude.Maybe Prelude.ISO8601,
    -- | The name of the solution stack this configuration set uses.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | If this configuration set is associated with an environment, the
    -- @DeploymentStatus@ parameter indicates the deployment status of this
    -- configuration set:
    --
    -- -   @null@: This configuration is not associated with a running
    --     environment.
    --
    -- -   @pending@: This is a draft configuration that is not deployed to the
    --     associated environment but is in the process of deploying.
    --
    -- -   @deployed@: This is the configuration that is currently deployed to
    --     the associated running environment.
    --
    -- -   @failed@: This is a draft configuration that failed to successfully
    --     deploy.
    deploymentStatus :: Prelude.Maybe ConfigurationDeploymentStatus,
    -- | If not @null@, the name of the environment for this configuration set.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the platform version.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The date (in UTC time) when this configuration set was last modified.
    dateUpdated :: Prelude.Maybe Prelude.ISO8601,
    -- | A list of the configuration options and their values in this
    -- configuration set.
    optionSettings :: Prelude.Maybe [ConfigurationOptionSetting],
    -- | Describes this configuration set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the application associated with this configuration set.
    applicationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'configurationSettingsDescription_templateName' - If not @null@, the name of the configuration template for this
-- configuration set.
--
-- 'dateCreated', 'configurationSettingsDescription_dateCreated' - The date (in UTC time) when this configuration set was created.
--
-- 'solutionStackName', 'configurationSettingsDescription_solutionStackName' - The name of the solution stack this configuration set uses.
--
-- 'deploymentStatus', 'configurationSettingsDescription_deploymentStatus' - If this configuration set is associated with an environment, the
-- @DeploymentStatus@ parameter indicates the deployment status of this
-- configuration set:
--
-- -   @null@: This configuration is not associated with a running
--     environment.
--
-- -   @pending@: This is a draft configuration that is not deployed to the
--     associated environment but is in the process of deploying.
--
-- -   @deployed@: This is the configuration that is currently deployed to
--     the associated running environment.
--
-- -   @failed@: This is a draft configuration that failed to successfully
--     deploy.
--
-- 'environmentName', 'configurationSettingsDescription_environmentName' - If not @null@, the name of the environment for this configuration set.
--
-- 'platformArn', 'configurationSettingsDescription_platformArn' - The ARN of the platform version.
--
-- 'dateUpdated', 'configurationSettingsDescription_dateUpdated' - The date (in UTC time) when this configuration set was last modified.
--
-- 'optionSettings', 'configurationSettingsDescription_optionSettings' - A list of the configuration options and their values in this
-- configuration set.
--
-- 'description', 'configurationSettingsDescription_description' - Describes this configuration set.
--
-- 'applicationName', 'configurationSettingsDescription_applicationName' - The name of the application associated with this configuration set.
newConfigurationSettingsDescription ::
  ConfigurationSettingsDescription
newConfigurationSettingsDescription =
  ConfigurationSettingsDescription'
    { templateName =
        Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      solutionStackName = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      optionSettings = Prelude.Nothing,
      description = Prelude.Nothing,
      applicationName = Prelude.Nothing
    }

-- | If not @null@, the name of the configuration template for this
-- configuration set.
configurationSettingsDescription_templateName :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_templateName = Lens.lens (\ConfigurationSettingsDescription' {templateName} -> templateName) (\s@ConfigurationSettingsDescription' {} a -> s {templateName = a} :: ConfigurationSettingsDescription)

-- | The date (in UTC time) when this configuration set was created.
configurationSettingsDescription_dateCreated :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.UTCTime)
configurationSettingsDescription_dateCreated = Lens.lens (\ConfigurationSettingsDescription' {dateCreated} -> dateCreated) (\s@ConfigurationSettingsDescription' {} a -> s {dateCreated = a} :: ConfigurationSettingsDescription) Prelude.. Lens.mapping Prelude._Time

-- | The name of the solution stack this configuration set uses.
configurationSettingsDescription_solutionStackName :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_solutionStackName = Lens.lens (\ConfigurationSettingsDescription' {solutionStackName} -> solutionStackName) (\s@ConfigurationSettingsDescription' {} a -> s {solutionStackName = a} :: ConfigurationSettingsDescription)

-- | If this configuration set is associated with an environment, the
-- @DeploymentStatus@ parameter indicates the deployment status of this
-- configuration set:
--
-- -   @null@: This configuration is not associated with a running
--     environment.
--
-- -   @pending@: This is a draft configuration that is not deployed to the
--     associated environment but is in the process of deploying.
--
-- -   @deployed@: This is the configuration that is currently deployed to
--     the associated running environment.
--
-- -   @failed@: This is a draft configuration that failed to successfully
--     deploy.
configurationSettingsDescription_deploymentStatus :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe ConfigurationDeploymentStatus)
configurationSettingsDescription_deploymentStatus = Lens.lens (\ConfigurationSettingsDescription' {deploymentStatus} -> deploymentStatus) (\s@ConfigurationSettingsDescription' {} a -> s {deploymentStatus = a} :: ConfigurationSettingsDescription)

-- | If not @null@, the name of the environment for this configuration set.
configurationSettingsDescription_environmentName :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_environmentName = Lens.lens (\ConfigurationSettingsDescription' {environmentName} -> environmentName) (\s@ConfigurationSettingsDescription' {} a -> s {environmentName = a} :: ConfigurationSettingsDescription)

-- | The ARN of the platform version.
configurationSettingsDescription_platformArn :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_platformArn = Lens.lens (\ConfigurationSettingsDescription' {platformArn} -> platformArn) (\s@ConfigurationSettingsDescription' {} a -> s {platformArn = a} :: ConfigurationSettingsDescription)

-- | The date (in UTC time) when this configuration set was last modified.
configurationSettingsDescription_dateUpdated :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.UTCTime)
configurationSettingsDescription_dateUpdated = Lens.lens (\ConfigurationSettingsDescription' {dateUpdated} -> dateUpdated) (\s@ConfigurationSettingsDescription' {} a -> s {dateUpdated = a} :: ConfigurationSettingsDescription) Prelude.. Lens.mapping Prelude._Time

-- | A list of the configuration options and their values in this
-- configuration set.
configurationSettingsDescription_optionSettings :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe [ConfigurationOptionSetting])
configurationSettingsDescription_optionSettings = Lens.lens (\ConfigurationSettingsDescription' {optionSettings} -> optionSettings) (\s@ConfigurationSettingsDescription' {} a -> s {optionSettings = a} :: ConfigurationSettingsDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | Describes this configuration set.
configurationSettingsDescription_description :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_description = Lens.lens (\ConfigurationSettingsDescription' {description} -> description) (\s@ConfigurationSettingsDescription' {} a -> s {description = a} :: ConfigurationSettingsDescription)

-- | The name of the application associated with this configuration set.
configurationSettingsDescription_applicationName :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_applicationName = Lens.lens (\ConfigurationSettingsDescription' {applicationName} -> applicationName) (\s@ConfigurationSettingsDescription' {} a -> s {applicationName = a} :: ConfigurationSettingsDescription)

instance
  Prelude.FromXML
    ConfigurationSettingsDescription
  where
  parseXML x =
    ConfigurationSettingsDescription'
      Prelude.<$> (x Prelude..@? "TemplateName")
      Prelude.<*> (x Prelude..@? "DateCreated")
      Prelude.<*> (x Prelude..@? "SolutionStackName")
      Prelude.<*> (x Prelude..@? "DeploymentStatus")
      Prelude.<*> (x Prelude..@? "EnvironmentName")
      Prelude.<*> (x Prelude..@? "PlatformArn")
      Prelude.<*> (x Prelude..@? "DateUpdated")
      Prelude.<*> ( x Prelude..@? "OptionSettings"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "ApplicationName")

instance
  Prelude.Hashable
    ConfigurationSettingsDescription

instance
  Prelude.NFData
    ConfigurationSettingsDescription
