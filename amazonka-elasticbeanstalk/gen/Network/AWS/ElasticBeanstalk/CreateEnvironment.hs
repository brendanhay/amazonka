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
-- Module      : Network.AWS.ElasticBeanstalk.CreateEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an AWS Elastic Beanstalk environment for the specified
-- application using the specified configuration.
module Network.AWS.ElasticBeanstalk.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_templateName,
    createEnvironment_groupName,
    createEnvironment_solutionStackName,
    createEnvironment_optionsToRemove,
    createEnvironment_environmentName,
    createEnvironment_platformArn,
    createEnvironment_versionLabel,
    createEnvironment_cNAMEPrefix,
    createEnvironment_tags,
    createEnvironment_optionSettings,
    createEnvironment_description,
    createEnvironment_tier,
    createEnvironment_operationsRole,
    createEnvironment_applicationName,

    -- * Destructuring the Response
    EnvironmentDescription (..),
    newEnvironmentDescription,

    -- * Response Lenses
    environmentDescription_abortableOperationInProgress,
    environmentDescription_templateName,
    environmentDescription_status,
    environmentDescription_dateCreated,
    environmentDescription_environmentLinks,
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_environmentName,
    environmentDescription_platformArn,
    environmentDescription_versionLabel,
    environmentDescription_health,
    environmentDescription_cname,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_healthStatus,
    environmentDescription_environmentArn,
    environmentDescription_endpointURL,
    environmentDescription_applicationName,
    environmentDescription_tier,
    environmentDescription_operationsRole,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | The name of the Elastic Beanstalk configuration template to use with the
    -- environment.
    --
    -- If you specify @TemplateName@, then don\'t specify @SolutionStackName@.
    templateName :: Core.Maybe Core.Text,
    -- | The name of the group to which the target environment belongs. Specify a
    -- group name only if the environment\'s name is specified in an
    -- environment manifest and not with the environment name parameter. See
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
    -- for details.
    groupName :: Core.Maybe Core.Text,
    -- | The name of an Elastic Beanstalk solution stack (platform version) to
    -- use with the environment. If specified, Elastic Beanstalk sets the
    -- configuration values to the default values associated with the specified
    -- solution stack. For a list of current solution stacks, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms>
    -- in the /AWS Elastic Beanstalk Platforms/ guide.
    --
    -- If you specify @SolutionStackName@, don\'t specify @PlatformArn@ or
    -- @TemplateName@.
    solutionStackName :: Core.Maybe Core.Text,
    -- | A list of custom user-defined configuration options to remove from the
    -- configuration set for this new environment.
    optionsToRemove :: Core.Maybe [OptionSpecification],
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
    environmentName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the custom platform to use with the
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms>
    -- in the /AWS Elastic Beanstalk Developer Guide/.
    --
    -- If you specify @PlatformArn@, don\'t specify @SolutionStackName@.
    platformArn :: Core.Maybe Core.Text,
    -- | The name of the application version to deploy.
    --
    -- Default: If not specified, Elastic Beanstalk attempts to deploy the
    -- sample application.
    versionLabel :: Core.Maybe Core.Text,
    -- | If specified, the environment attempts to use this value as the prefix
    -- for the CNAME in your Elastic Beanstalk environment URL. If not
    -- specified, the CNAME is generated automatically by appending a random
    -- alphanumeric string to the environment name.
    cNAMEPrefix :: Core.Maybe Core.Text,
    -- | Specifies the tags applied to resources in the environment.
    tags :: Core.Maybe [Tag],
    -- | If specified, AWS Elastic Beanstalk sets the specified configuration
    -- options to the requested value in the configuration set for the new
    -- environment. These override the values obtained from the solution stack
    -- or the configuration template.
    optionSettings :: Core.Maybe [ConfigurationOptionSetting],
    -- | Your description for this environment.
    description :: Core.Maybe Core.Text,
    -- | Specifies the tier to use in creating this environment. The environment
    -- tier that you choose determines whether Elastic Beanstalk provisions
    -- resources to support a web application that handles HTTP(S) requests or
    -- a web application that handles background-processing tasks.
    tier :: Core.Maybe EnvironmentTier,
    -- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the
    -- environment\'s operations role. If specified, Elastic Beanstalk uses the
    -- operations role for permissions to downstream services during this call
    -- and during subsequent calls acting on this environment. To specify an
    -- operations role, you must have the @iam:PassRole@ permission for the
    -- role. For more information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles>
    -- in the /AWS Elastic Beanstalk Developer Guide/.
    operationsRole :: Core.Maybe Core.Text,
    -- | The name of the application that is associated with this environment.
    applicationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createEnvironment_templateName' - The name of the Elastic Beanstalk configuration template to use with the
-- environment.
--
-- If you specify @TemplateName@, then don\'t specify @SolutionStackName@.
--
-- 'groupName', 'createEnvironment_groupName' - The name of the group to which the target environment belongs. Specify a
-- group name only if the environment\'s name is specified in an
-- environment manifest and not with the environment name parameter. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
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
-- 'optionsToRemove', 'createEnvironment_optionsToRemove' - A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
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
-- 'platformArn', 'createEnvironment_platformArn' - The Amazon Resource Name (ARN) of the custom platform to use with the
-- environment. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- If you specify @PlatformArn@, don\'t specify @SolutionStackName@.
--
-- 'versionLabel', 'createEnvironment_versionLabel' - The name of the application version to deploy.
--
-- Default: If not specified, Elastic Beanstalk attempts to deploy the
-- sample application.
--
-- 'cNAMEPrefix', 'createEnvironment_cNAMEPrefix' - If specified, the environment attempts to use this value as the prefix
-- for the CNAME in your Elastic Beanstalk environment URL. If not
-- specified, the CNAME is generated automatically by appending a random
-- alphanumeric string to the environment name.
--
-- 'tags', 'createEnvironment_tags' - Specifies the tags applied to resources in the environment.
--
-- 'optionSettings', 'createEnvironment_optionSettings' - If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack
-- or the configuration template.
--
-- 'description', 'createEnvironment_description' - Your description for this environment.
--
-- 'tier', 'createEnvironment_tier' - Specifies the tier to use in creating this environment. The environment
-- tier that you choose determines whether Elastic Beanstalk provisions
-- resources to support a web application that handles HTTP(S) requests or
-- a web application that handles background-processing tasks.
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
-- 'applicationName', 'createEnvironment_applicationName' - The name of the application that is associated with this environment.
newCreateEnvironment ::
  -- | 'applicationName'
  Core.Text ->
  CreateEnvironment
newCreateEnvironment pApplicationName_ =
  CreateEnvironment'
    { templateName = Core.Nothing,
      groupName = Core.Nothing,
      solutionStackName = Core.Nothing,
      optionsToRemove = Core.Nothing,
      environmentName = Core.Nothing,
      platformArn = Core.Nothing,
      versionLabel = Core.Nothing,
      cNAMEPrefix = Core.Nothing,
      tags = Core.Nothing,
      optionSettings = Core.Nothing,
      description = Core.Nothing,
      tier = Core.Nothing,
      operationsRole = Core.Nothing,
      applicationName = pApplicationName_
    }

-- | The name of the Elastic Beanstalk configuration template to use with the
-- environment.
--
-- If you specify @TemplateName@, then don\'t specify @SolutionStackName@.
createEnvironment_templateName :: Lens.Lens' CreateEnvironment (Core.Maybe Core.Text)
createEnvironment_templateName = Lens.lens (\CreateEnvironment' {templateName} -> templateName) (\s@CreateEnvironment' {} a -> s {templateName = a} :: CreateEnvironment)

-- | The name of the group to which the target environment belongs. Specify a
-- group name only if the environment\'s name is specified in an
-- environment manifest and not with the environment name parameter. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
createEnvironment_groupName :: Lens.Lens' CreateEnvironment (Core.Maybe Core.Text)
createEnvironment_groupName = Lens.lens (\CreateEnvironment' {groupName} -> groupName) (\s@CreateEnvironment' {} a -> s {groupName = a} :: CreateEnvironment)

-- | The name of an Elastic Beanstalk solution stack (platform version) to
-- use with the environment. If specified, Elastic Beanstalk sets the
-- configuration values to the default values associated with the specified
-- solution stack. For a list of current solution stacks, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms>
-- in the /AWS Elastic Beanstalk Platforms/ guide.
--
-- If you specify @SolutionStackName@, don\'t specify @PlatformArn@ or
-- @TemplateName@.
createEnvironment_solutionStackName :: Lens.Lens' CreateEnvironment (Core.Maybe Core.Text)
createEnvironment_solutionStackName = Lens.lens (\CreateEnvironment' {solutionStackName} -> solutionStackName) (\s@CreateEnvironment' {} a -> s {solutionStackName = a} :: CreateEnvironment)

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
createEnvironment_optionsToRemove :: Lens.Lens' CreateEnvironment (Core.Maybe [OptionSpecification])
createEnvironment_optionsToRemove = Lens.lens (\CreateEnvironment' {optionsToRemove} -> optionsToRemove) (\s@CreateEnvironment' {} a -> s {optionsToRemove = a} :: CreateEnvironment) Core.. Lens.mapping Lens._Coerce

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
createEnvironment_environmentName :: Lens.Lens' CreateEnvironment (Core.Maybe Core.Text)
createEnvironment_environmentName = Lens.lens (\CreateEnvironment' {environmentName} -> environmentName) (\s@CreateEnvironment' {} a -> s {environmentName = a} :: CreateEnvironment)

-- | The Amazon Resource Name (ARN) of the custom platform to use with the
-- environment. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- If you specify @PlatformArn@, don\'t specify @SolutionStackName@.
createEnvironment_platformArn :: Lens.Lens' CreateEnvironment (Core.Maybe Core.Text)
createEnvironment_platformArn = Lens.lens (\CreateEnvironment' {platformArn} -> platformArn) (\s@CreateEnvironment' {} a -> s {platformArn = a} :: CreateEnvironment)

-- | The name of the application version to deploy.
--
-- Default: If not specified, Elastic Beanstalk attempts to deploy the
-- sample application.
createEnvironment_versionLabel :: Lens.Lens' CreateEnvironment (Core.Maybe Core.Text)
createEnvironment_versionLabel = Lens.lens (\CreateEnvironment' {versionLabel} -> versionLabel) (\s@CreateEnvironment' {} a -> s {versionLabel = a} :: CreateEnvironment)

-- | If specified, the environment attempts to use this value as the prefix
-- for the CNAME in your Elastic Beanstalk environment URL. If not
-- specified, the CNAME is generated automatically by appending a random
-- alphanumeric string to the environment name.
createEnvironment_cNAMEPrefix :: Lens.Lens' CreateEnvironment (Core.Maybe Core.Text)
createEnvironment_cNAMEPrefix = Lens.lens (\CreateEnvironment' {cNAMEPrefix} -> cNAMEPrefix) (\s@CreateEnvironment' {} a -> s {cNAMEPrefix = a} :: CreateEnvironment)

-- | Specifies the tags applied to resources in the environment.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Core.Maybe [Tag])
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Core.. Lens.mapping Lens._Coerce

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack
-- or the configuration template.
createEnvironment_optionSettings :: Lens.Lens' CreateEnvironment (Core.Maybe [ConfigurationOptionSetting])
createEnvironment_optionSettings = Lens.lens (\CreateEnvironment' {optionSettings} -> optionSettings) (\s@CreateEnvironment' {} a -> s {optionSettings = a} :: CreateEnvironment) Core.. Lens.mapping Lens._Coerce

-- | Your description for this environment.
createEnvironment_description :: Lens.Lens' CreateEnvironment (Core.Maybe Core.Text)
createEnvironment_description = Lens.lens (\CreateEnvironment' {description} -> description) (\s@CreateEnvironment' {} a -> s {description = a} :: CreateEnvironment)

-- | Specifies the tier to use in creating this environment. The environment
-- tier that you choose determines whether Elastic Beanstalk provisions
-- resources to support a web application that handles HTTP(S) requests or
-- a web application that handles background-processing tasks.
createEnvironment_tier :: Lens.Lens' CreateEnvironment (Core.Maybe EnvironmentTier)
createEnvironment_tier = Lens.lens (\CreateEnvironment' {tier} -> tier) (\s@CreateEnvironment' {} a -> s {tier = a} :: CreateEnvironment)

-- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the
-- environment\'s operations role. If specified, Elastic Beanstalk uses the
-- operations role for permissions to downstream services during this call
-- and during subsequent calls acting on this environment. To specify an
-- operations role, you must have the @iam:PassRole@ permission for the
-- role. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles>
-- in the /AWS Elastic Beanstalk Developer Guide/.
createEnvironment_operationsRole :: Lens.Lens' CreateEnvironment (Core.Maybe Core.Text)
createEnvironment_operationsRole = Lens.lens (\CreateEnvironment' {operationsRole} -> operationsRole) (\s@CreateEnvironment' {} a -> s {operationsRole = a} :: CreateEnvironment)

-- | The name of the application that is associated with this environment.
createEnvironment_applicationName :: Lens.Lens' CreateEnvironment Core.Text
createEnvironment_applicationName = Lens.lens (\CreateEnvironment' {applicationName} -> applicationName) (\s@CreateEnvironment' {} a -> s {applicationName = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type
    AWSResponse CreateEnvironment =
      EnvironmentDescription
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateEnvironmentResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable CreateEnvironment

instance Core.NFData CreateEnvironment

instance Core.ToHeaders CreateEnvironment where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateEnvironment where
  toPath = Core.const "/"

instance Core.ToQuery CreateEnvironment where
  toQuery CreateEnvironment' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateEnvironment" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "TemplateName" Core.=: templateName,
        "GroupName" Core.=: groupName,
        "SolutionStackName" Core.=: solutionStackName,
        "OptionsToRemove"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> optionsToRemove),
        "EnvironmentName" Core.=: environmentName,
        "PlatformArn" Core.=: platformArn,
        "VersionLabel" Core.=: versionLabel,
        "CNAMEPrefix" Core.=: cNAMEPrefix,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> tags),
        "OptionSettings"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> optionSettings),
        "Description" Core.=: description,
        "Tier" Core.=: tier,
        "OperationsRole" Core.=: operationsRole,
        "ApplicationName" Core.=: applicationName
      ]
