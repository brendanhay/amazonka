{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticBeanstalk.CreateEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an AWS Elastic Beanstalk environment for the specified
-- application using the specified configuration.
module Amazonka.ElasticBeanstalk.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_cNAMEPrefix,
    createEnvironment_description,
    createEnvironment_environmentName,
    createEnvironment_groupName,
    createEnvironment_operationsRole,
    createEnvironment_optionSettings,
    createEnvironment_optionsToRemove,
    createEnvironment_platformArn,
    createEnvironment_solutionStackName,
    createEnvironment_tags,
    createEnvironment_templateName,
    createEnvironment_tier,
    createEnvironment_versionLabel,
    createEnvironment_applicationName,

    -- * Destructuring the Response
    EnvironmentDescription (..),
    newEnvironmentDescription,

    -- * Response Lenses
    environmentDescription_abortableOperationInProgress,
    environmentDescription_applicationName,
    environmentDescription_cname,
    environmentDescription_dateCreated,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_endpointURL,
    environmentDescription_environmentArn,
    environmentDescription_environmentId,
    environmentDescription_environmentLinks,
    environmentDescription_environmentName,
    environmentDescription_health,
    environmentDescription_healthStatus,
    environmentDescription_operationsRole,
    environmentDescription_platformArn,
    environmentDescription_resources,
    environmentDescription_solutionStackName,
    environmentDescription_status,
    environmentDescription_templateName,
    environmentDescription_tier,
    environmentDescription_versionLabel,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | If specified, the environment attempts to use this value as the prefix
    -- for the CNAME in your Elastic Beanstalk environment URL. If not
    -- specified, the CNAME is generated automatically by appending a random
    -- alphanumeric string to the environment name.
    cNAMEPrefix :: Prelude.Maybe Prelude.Text,
    -- | Your description for this environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique name for the environment.
    --
    -- Constraint: Must be from 4 to 40 characters in length. The name can
    -- contain only letters, numbers, and hyphens. It can\'t start or end with
    -- a hyphen. This name must be unique within a region in your account. If
    -- the specified name already exists in the region, Elastic Beanstalk
    -- returns an @InvalidParameterValue@ error.
    --
    -- If you don\'t specify the @CNAMEPrefix@ parameter, the environment name
    -- becomes part of the CNAME, and therefore part of the visible URL for
    -- your application.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the group to which the target environment belongs. Specify a
    -- group name only if the environment\'s name is specified in an
    -- environment manifest and not with the environment name parameter. See
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
    -- for details.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the
    -- environment\'s operations role. If specified, Elastic Beanstalk uses the
    -- operations role for permissions to downstream services during this call
    -- and during subsequent calls acting on this environment. To specify an
    -- operations role, you must have the @iam:PassRole@ permission for the
    -- role. For more information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles>
    -- in the /AWS Elastic Beanstalk Developer Guide/.
    operationsRole :: Prelude.Maybe Prelude.Text,
    -- | If specified, AWS Elastic Beanstalk sets the specified configuration
    -- options to the requested value in the configuration set for the new
    -- environment. These override the values obtained from the solution stack
    -- or the configuration template.
    optionSettings :: Prelude.Maybe [ConfigurationOptionSetting],
    -- | A list of custom user-defined configuration options to remove from the
    -- configuration set for this new environment.
    optionsToRemove :: Prelude.Maybe [OptionSpecification],
    -- | The Amazon Resource Name (ARN) of the custom platform to use with the
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms>
    -- in the /AWS Elastic Beanstalk Developer Guide/.
    --
    -- If you specify @PlatformArn@, don\'t specify @SolutionStackName@.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The name of an Elastic Beanstalk solution stack (platform version) to
    -- use with the environment. If specified, Elastic Beanstalk sets the
    -- configuration values to the default values associated with the specified
    -- solution stack. For a list of current solution stacks, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms>
    -- in the /AWS Elastic Beanstalk Platforms/ guide.
    --
    -- If you specify @SolutionStackName@, don\'t specify @PlatformArn@ or
    -- @TemplateName@.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the tags applied to resources in the environment.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the Elastic Beanstalk configuration template to use with the
    -- environment.
    --
    -- If you specify @TemplateName@, then don\'t specify @SolutionStackName@.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the tier to use in creating this environment. The environment
    -- tier that you choose determines whether Elastic Beanstalk provisions
    -- resources to support a web application that handles HTTP(S) requests or
    -- a web application that handles background-processing tasks.
    tier :: Prelude.Maybe EnvironmentTier,
    -- | The name of the application version to deploy.
    --
    -- Default: If not specified, Elastic Beanstalk attempts to deploy the
    -- sample application.
    versionLabel :: Prelude.Maybe Prelude.Text,
    -- | The name of the application that is associated with this environment.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cNAMEPrefix', 'createEnvironment_cNAMEPrefix' - If specified, the environment attempts to use this value as the prefix
-- for the CNAME in your Elastic Beanstalk environment URL. If not
-- specified, the CNAME is generated automatically by appending a random
-- alphanumeric string to the environment name.
--
-- 'description', 'createEnvironment_description' - Your description for this environment.
--
-- 'environmentName', 'createEnvironment_environmentName' - A unique name for the environment.
--
-- Constraint: Must be from 4 to 40 characters in length. The name can
-- contain only letters, numbers, and hyphens. It can\'t start or end with
-- a hyphen. This name must be unique within a region in your account. If
-- the specified name already exists in the region, Elastic Beanstalk
-- returns an @InvalidParameterValue@ error.
--
-- If you don\'t specify the @CNAMEPrefix@ parameter, the environment name
-- becomes part of the CNAME, and therefore part of the visible URL for
-- your application.
--
-- 'groupName', 'createEnvironment_groupName' - The name of the group to which the target environment belongs. Specify a
-- group name only if the environment\'s name is specified in an
-- environment manifest and not with the environment name parameter. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
--
-- 'operationsRole', 'createEnvironment_operationsRole' - The Amazon Resource Name (ARN) of an existing IAM role to be used as the
-- environment\'s operations role. If specified, Elastic Beanstalk uses the
-- operations role for permissions to downstream services during this call
-- and during subsequent calls acting on this environment. To specify an
-- operations role, you must have the @iam:PassRole@ permission for the
-- role. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- 'optionSettings', 'createEnvironment_optionSettings' - If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack
-- or the configuration template.
--
-- 'optionsToRemove', 'createEnvironment_optionsToRemove' - A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
--
-- 'platformArn', 'createEnvironment_platformArn' - The Amazon Resource Name (ARN) of the custom platform to use with the
-- environment. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- If you specify @PlatformArn@, don\'t specify @SolutionStackName@.
--
-- 'solutionStackName', 'createEnvironment_solutionStackName' - The name of an Elastic Beanstalk solution stack (platform version) to
-- use with the environment. If specified, Elastic Beanstalk sets the
-- configuration values to the default values associated with the specified
-- solution stack. For a list of current solution stacks, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms>
-- in the /AWS Elastic Beanstalk Platforms/ guide.
--
-- If you specify @SolutionStackName@, don\'t specify @PlatformArn@ or
-- @TemplateName@.
--
-- 'tags', 'createEnvironment_tags' - Specifies the tags applied to resources in the environment.
--
-- 'templateName', 'createEnvironment_templateName' - The name of the Elastic Beanstalk configuration template to use with the
-- environment.
--
-- If you specify @TemplateName@, then don\'t specify @SolutionStackName@.
--
-- 'tier', 'createEnvironment_tier' - Specifies the tier to use in creating this environment. The environment
-- tier that you choose determines whether Elastic Beanstalk provisions
-- resources to support a web application that handles HTTP(S) requests or
-- a web application that handles background-processing tasks.
--
-- 'versionLabel', 'createEnvironment_versionLabel' - The name of the application version to deploy.
--
-- Default: If not specified, Elastic Beanstalk attempts to deploy the
-- sample application.
--
-- 'applicationName', 'createEnvironment_applicationName' - The name of the application that is associated with this environment.
newCreateEnvironment ::
  -- | 'applicationName'
  Prelude.Text ->
  CreateEnvironment
newCreateEnvironment pApplicationName_ =
  CreateEnvironment'
    { cNAMEPrefix = Prelude.Nothing,
      description = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      groupName = Prelude.Nothing,
      operationsRole = Prelude.Nothing,
      optionSettings = Prelude.Nothing,
      optionsToRemove = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      solutionStackName = Prelude.Nothing,
      tags = Prelude.Nothing,
      templateName = Prelude.Nothing,
      tier = Prelude.Nothing,
      versionLabel = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | If specified, the environment attempts to use this value as the prefix
-- for the CNAME in your Elastic Beanstalk environment URL. If not
-- specified, the CNAME is generated automatically by appending a random
-- alphanumeric string to the environment name.
createEnvironment_cNAMEPrefix :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_cNAMEPrefix = Lens.lens (\CreateEnvironment' {cNAMEPrefix} -> cNAMEPrefix) (\s@CreateEnvironment' {} a -> s {cNAMEPrefix = a} :: CreateEnvironment)

-- | Your description for this environment.
createEnvironment_description :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_description = Lens.lens (\CreateEnvironment' {description} -> description) (\s@CreateEnvironment' {} a -> s {description = a} :: CreateEnvironment)

-- | A unique name for the environment.
--
-- Constraint: Must be from 4 to 40 characters in length. The name can
-- contain only letters, numbers, and hyphens. It can\'t start or end with
-- a hyphen. This name must be unique within a region in your account. If
-- the specified name already exists in the region, Elastic Beanstalk
-- returns an @InvalidParameterValue@ error.
--
-- If you don\'t specify the @CNAMEPrefix@ parameter, the environment name
-- becomes part of the CNAME, and therefore part of the visible URL for
-- your application.
createEnvironment_environmentName :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_environmentName = Lens.lens (\CreateEnvironment' {environmentName} -> environmentName) (\s@CreateEnvironment' {} a -> s {environmentName = a} :: CreateEnvironment)

-- | The name of the group to which the target environment belongs. Specify a
-- group name only if the environment\'s name is specified in an
-- environment manifest and not with the environment name parameter. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
createEnvironment_groupName :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_groupName = Lens.lens (\CreateEnvironment' {groupName} -> groupName) (\s@CreateEnvironment' {} a -> s {groupName = a} :: CreateEnvironment)

-- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the
-- environment\'s operations role. If specified, Elastic Beanstalk uses the
-- operations role for permissions to downstream services during this call
-- and during subsequent calls acting on this environment. To specify an
-- operations role, you must have the @iam:PassRole@ permission for the
-- role. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles>
-- in the /AWS Elastic Beanstalk Developer Guide/.
createEnvironment_operationsRole :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_operationsRole = Lens.lens (\CreateEnvironment' {operationsRole} -> operationsRole) (\s@CreateEnvironment' {} a -> s {operationsRole = a} :: CreateEnvironment)

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack
-- or the configuration template.
createEnvironment_optionSettings :: Lens.Lens' CreateEnvironment (Prelude.Maybe [ConfigurationOptionSetting])
createEnvironment_optionSettings = Lens.lens (\CreateEnvironment' {optionSettings} -> optionSettings) (\s@CreateEnvironment' {} a -> s {optionSettings = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
createEnvironment_optionsToRemove :: Lens.Lens' CreateEnvironment (Prelude.Maybe [OptionSpecification])
createEnvironment_optionsToRemove = Lens.lens (\CreateEnvironment' {optionsToRemove} -> optionsToRemove) (\s@CreateEnvironment' {} a -> s {optionsToRemove = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the custom platform to use with the
-- environment. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- If you specify @PlatformArn@, don\'t specify @SolutionStackName@.
createEnvironment_platformArn :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_platformArn = Lens.lens (\CreateEnvironment' {platformArn} -> platformArn) (\s@CreateEnvironment' {} a -> s {platformArn = a} :: CreateEnvironment)

-- | The name of an Elastic Beanstalk solution stack (platform version) to
-- use with the environment. If specified, Elastic Beanstalk sets the
-- configuration values to the default values associated with the specified
-- solution stack. For a list of current solution stacks, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms>
-- in the /AWS Elastic Beanstalk Platforms/ guide.
--
-- If you specify @SolutionStackName@, don\'t specify @PlatformArn@ or
-- @TemplateName@.
createEnvironment_solutionStackName :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_solutionStackName = Lens.lens (\CreateEnvironment' {solutionStackName} -> solutionStackName) (\s@CreateEnvironment' {} a -> s {solutionStackName = a} :: CreateEnvironment)

-- | Specifies the tags applied to resources in the environment.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Prelude.Maybe [Tag])
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Elastic Beanstalk configuration template to use with the
-- environment.
--
-- If you specify @TemplateName@, then don\'t specify @SolutionStackName@.
createEnvironment_templateName :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_templateName = Lens.lens (\CreateEnvironment' {templateName} -> templateName) (\s@CreateEnvironment' {} a -> s {templateName = a} :: CreateEnvironment)

-- | Specifies the tier to use in creating this environment. The environment
-- tier that you choose determines whether Elastic Beanstalk provisions
-- resources to support a web application that handles HTTP(S) requests or
-- a web application that handles background-processing tasks.
createEnvironment_tier :: Lens.Lens' CreateEnvironment (Prelude.Maybe EnvironmentTier)
createEnvironment_tier = Lens.lens (\CreateEnvironment' {tier} -> tier) (\s@CreateEnvironment' {} a -> s {tier = a} :: CreateEnvironment)

-- | The name of the application version to deploy.
--
-- Default: If not specified, Elastic Beanstalk attempts to deploy the
-- sample application.
createEnvironment_versionLabel :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_versionLabel = Lens.lens (\CreateEnvironment' {versionLabel} -> versionLabel) (\s@CreateEnvironment' {} a -> s {versionLabel = a} :: CreateEnvironment)

-- | The name of the application that is associated with this environment.
createEnvironment_applicationName :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_applicationName = Lens.lens (\CreateEnvironment' {applicationName} -> applicationName) (\s@CreateEnvironment' {} a -> s {applicationName = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type
    AWSResponse CreateEnvironment =
      EnvironmentDescription
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateEnvironmentResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable CreateEnvironment where
  hashWithSalt _salt CreateEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` cNAMEPrefix
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` operationsRole
      `Prelude.hashWithSalt` optionSettings
      `Prelude.hashWithSalt` optionsToRemove
      `Prelude.hashWithSalt` platformArn
      `Prelude.hashWithSalt` solutionStackName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` tier
      `Prelude.hashWithSalt` versionLabel
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData CreateEnvironment where
  rnf CreateEnvironment' {..} =
    Prelude.rnf cNAMEPrefix `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf environmentName `Prelude.seq`
          Prelude.rnf groupName `Prelude.seq`
            Prelude.rnf operationsRole `Prelude.seq`
              Prelude.rnf optionSettings `Prelude.seq`
                Prelude.rnf optionsToRemove `Prelude.seq`
                  Prelude.rnf platformArn `Prelude.seq`
                    Prelude.rnf solutionStackName `Prelude.seq`
                      Prelude.rnf tags `Prelude.seq`
                        Prelude.rnf templateName `Prelude.seq`
                          Prelude.rnf tier `Prelude.seq`
                            Prelude.rnf versionLabel `Prelude.seq`
                              Prelude.rnf applicationName

instance Data.ToHeaders CreateEnvironment where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateEnvironment where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEnvironment where
  toQuery CreateEnvironment' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateEnvironment" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "CNAMEPrefix" Data.=: cNAMEPrefix,
        "Description" Data.=: description,
        "EnvironmentName" Data.=: environmentName,
        "GroupName" Data.=: groupName,
        "OperationsRole" Data.=: operationsRole,
        "OptionSettings"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> optionSettings
            ),
        "OptionsToRemove"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> optionsToRemove
            ),
        "PlatformArn" Data.=: platformArn,
        "SolutionStackName" Data.=: solutionStackName,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "TemplateName" Data.=: templateName,
        "Tier" Data.=: tier,
        "VersionLabel" Data.=: versionLabel,
        "ApplicationName" Data.=: applicationName
      ]
