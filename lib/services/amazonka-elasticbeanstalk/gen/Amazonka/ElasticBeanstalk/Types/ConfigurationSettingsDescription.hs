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
-- Module      : Amazonka.ElasticBeanstalk.Types.ConfigurationSettingsDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ConfigurationSettingsDescription where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
import Amazonka.ElasticBeanstalk.Types.ConfigurationOptionSetting
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the settings for a configuration set.
--
-- /See:/ 'newConfigurationSettingsDescription' smart constructor.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription'
  { -- | If not @null@, the name of the configuration template for this
    -- configuration set.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | A list of the configuration options and their values in this
    -- configuration set.
    optionSettings :: Prelude.Maybe [ConfigurationOptionSetting],
    -- | The date (in UTC time) when this configuration set was last modified.
    dateUpdated :: Prelude.Maybe Core.ISO8601,
    -- | The date (in UTC time) when this configuration set was created.
    dateCreated :: Prelude.Maybe Core.ISO8601,
    -- | The ARN of the platform version.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | If not @null@, the name of the environment for this configuration set.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the application associated with this configuration set.
    applicationName :: Prelude.Maybe Prelude.Text,
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
    -- | The name of the solution stack this configuration set uses.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | Describes this configuration set.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'optionSettings', 'configurationSettingsDescription_optionSettings' - A list of the configuration options and their values in this
-- configuration set.
--
-- 'dateUpdated', 'configurationSettingsDescription_dateUpdated' - The date (in UTC time) when this configuration set was last modified.
--
-- 'dateCreated', 'configurationSettingsDescription_dateCreated' - The date (in UTC time) when this configuration set was created.
--
-- 'platformArn', 'configurationSettingsDescription_platformArn' - The ARN of the platform version.
--
-- 'environmentName', 'configurationSettingsDescription_environmentName' - If not @null@, the name of the environment for this configuration set.
--
-- 'applicationName', 'configurationSettingsDescription_applicationName' - The name of the application associated with this configuration set.
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
-- 'solutionStackName', 'configurationSettingsDescription_solutionStackName' - The name of the solution stack this configuration set uses.
--
-- 'description', 'configurationSettingsDescription_description' - Describes this configuration set.
newConfigurationSettingsDescription ::
  ConfigurationSettingsDescription
newConfigurationSettingsDescription =
  ConfigurationSettingsDescription'
    { templateName =
        Prelude.Nothing,
      optionSettings = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      solutionStackName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | If not @null@, the name of the configuration template for this
-- configuration set.
configurationSettingsDescription_templateName :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_templateName = Lens.lens (\ConfigurationSettingsDescription' {templateName} -> templateName) (\s@ConfigurationSettingsDescription' {} a -> s {templateName = a} :: ConfigurationSettingsDescription)

-- | A list of the configuration options and their values in this
-- configuration set.
configurationSettingsDescription_optionSettings :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe [ConfigurationOptionSetting])
configurationSettingsDescription_optionSettings = Lens.lens (\ConfigurationSettingsDescription' {optionSettings} -> optionSettings) (\s@ConfigurationSettingsDescription' {} a -> s {optionSettings = a} :: ConfigurationSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | The date (in UTC time) when this configuration set was last modified.
configurationSettingsDescription_dateUpdated :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.UTCTime)
configurationSettingsDescription_dateUpdated = Lens.lens (\ConfigurationSettingsDescription' {dateUpdated} -> dateUpdated) (\s@ConfigurationSettingsDescription' {} a -> s {dateUpdated = a} :: ConfigurationSettingsDescription) Prelude.. Lens.mapping Core._Time

-- | The date (in UTC time) when this configuration set was created.
configurationSettingsDescription_dateCreated :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.UTCTime)
configurationSettingsDescription_dateCreated = Lens.lens (\ConfigurationSettingsDescription' {dateCreated} -> dateCreated) (\s@ConfigurationSettingsDescription' {} a -> s {dateCreated = a} :: ConfigurationSettingsDescription) Prelude.. Lens.mapping Core._Time

-- | The ARN of the platform version.
configurationSettingsDescription_platformArn :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_platformArn = Lens.lens (\ConfigurationSettingsDescription' {platformArn} -> platformArn) (\s@ConfigurationSettingsDescription' {} a -> s {platformArn = a} :: ConfigurationSettingsDescription)

-- | If not @null@, the name of the environment for this configuration set.
configurationSettingsDescription_environmentName :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_environmentName = Lens.lens (\ConfigurationSettingsDescription' {environmentName} -> environmentName) (\s@ConfigurationSettingsDescription' {} a -> s {environmentName = a} :: ConfigurationSettingsDescription)

-- | The name of the application associated with this configuration set.
configurationSettingsDescription_applicationName :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_applicationName = Lens.lens (\ConfigurationSettingsDescription' {applicationName} -> applicationName) (\s@ConfigurationSettingsDescription' {} a -> s {applicationName = a} :: ConfigurationSettingsDescription)

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

-- | The name of the solution stack this configuration set uses.
configurationSettingsDescription_solutionStackName :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_solutionStackName = Lens.lens (\ConfigurationSettingsDescription' {solutionStackName} -> solutionStackName) (\s@ConfigurationSettingsDescription' {} a -> s {solutionStackName = a} :: ConfigurationSettingsDescription)

-- | Describes this configuration set.
configurationSettingsDescription_description :: Lens.Lens' ConfigurationSettingsDescription (Prelude.Maybe Prelude.Text)
configurationSettingsDescription_description = Lens.lens (\ConfigurationSettingsDescription' {description} -> description) (\s@ConfigurationSettingsDescription' {} a -> s {description = a} :: ConfigurationSettingsDescription)

instance
  Core.FromXML
    ConfigurationSettingsDescription
  where
  parseXML x =
    ConfigurationSettingsDescription'
      Prelude.<$> (x Core..@? "TemplateName")
      Prelude.<*> ( x Core..@? "OptionSettings" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "DateUpdated")
      Prelude.<*> (x Core..@? "DateCreated")
      Prelude.<*> (x Core..@? "PlatformArn")
      Prelude.<*> (x Core..@? "EnvironmentName")
      Prelude.<*> (x Core..@? "ApplicationName")
      Prelude.<*> (x Core..@? "DeploymentStatus")
      Prelude.<*> (x Core..@? "SolutionStackName")
      Prelude.<*> (x Core..@? "Description")

instance
  Prelude.Hashable
    ConfigurationSettingsDescription
  where
  hashWithSalt
    salt'
    ConfigurationSettingsDescription' {..} =
      salt' `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` solutionStackName
        `Prelude.hashWithSalt` deploymentStatus
        `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` platformArn
        `Prelude.hashWithSalt` dateCreated
        `Prelude.hashWithSalt` dateUpdated
        `Prelude.hashWithSalt` optionSettings
        `Prelude.hashWithSalt` templateName

instance
  Prelude.NFData
    ConfigurationSettingsDescription
  where
  rnf ConfigurationSettingsDescription' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf solutionStackName
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf platformArn
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf optionSettings
