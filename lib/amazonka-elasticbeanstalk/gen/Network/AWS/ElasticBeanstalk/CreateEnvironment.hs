{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an AWS Elastic Beanstalk environment for the specified application using the specified configuration.
module Network.AWS.ElasticBeanstalk.CreateEnvironment
  ( -- * Creating a request
    CreateEnvironment (..),
    mkCreateEnvironment,

    -- ** Request lenses
    cCNAMEPrefix,
    cTemplateName,
    cOptionsToRemove,
    cOptionSettings,
    cVersionLabel,
    cOperationsRole,
    cPlatformARN,
    cTier,
    cEnvironmentName,
    cApplicationName,
    cSolutionStackName,
    cGroupName,
    cDescription,
    cTags,

    -- * Destructuring the response
    EnvironmentDescription (..),
    mkEnvironmentDescription,

    -- ** Response lenses
    eStatus,
    eCNAME,
    eTemplateName,
    eAbortableOperationInProgress,
    eEndpointURL,
    eResources,
    eDateUpdated,
    eDateCreated,
    eHealth,
    eVersionLabel,
    eOperationsRole,
    ePlatformARN,
    eTier,
    eEnvironmentName,
    eApplicationName,
    eEnvironmentARN,
    eSolutionStackName,
    eEnvironmentId,
    eHealthStatus,
    eEnvironmentLinks,
    eDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | If specified, the environment attempts to use this value as the prefix for the CNAME in your Elastic Beanstalk environment URL. If not specified, the CNAME is generated automatically by appending a random alphanumeric string to the environment name.
    cNAMEPrefix :: Lude.Maybe Lude.Text,
    -- | The name of the Elastic Beanstalk configuration template to use with the environment.
    templateName :: Lude.Maybe Lude.Text,
    -- | A list of custom user-defined configuration options to remove from the configuration set for this new environment.
    optionsToRemove :: Lude.Maybe [OptionSpecification],
    -- | If specified, AWS Elastic Beanstalk sets the specified configuration options to the requested value in the configuration set for the new environment. These override the values obtained from the solution stack or the configuration template.
    optionSettings :: Lude.Maybe [ConfigurationOptionSetting],
    -- | The name of the application version to deploy.
    --
    -- Default: If not specified, Elastic Beanstalk attempts to deploy the sample application.
    versionLabel :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role. If specified, Elastic Beanstalk uses the operations role for permissions to downstream services during this call and during subsequent calls acting on this environment. To specify an operations role, you must have the @iam:PassRole@ permission for the role. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
    operationsRole :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the custom platform to use with the environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
    platformARN :: Lude.Maybe Lude.Text,
    -- | Specifies the tier to use in creating this environment. The environment tier that you choose determines whether Elastic Beanstalk provisions resources to support a web application that handles HTTP(S) requests or a web application that handles background-processing tasks.
    tier :: Lude.Maybe EnvironmentTier,
    -- | A unique name for the environment.
    --
    -- Constraint: Must be from 4 to 40 characters in length. The name can contain only letters, numbers, and hyphens. It can't start or end with a hyphen. This name must be unique within a region in your account. If the specified name already exists in the region, Elastic Beanstalk returns an @InvalidParameterValue@ error.
    -- If you don't specify the @CNAMEPrefix@ parameter, the environment name becomes part of the CNAME, and therefore part of the visible URL for your application.
    environmentName :: Lude.Maybe Lude.Text,
    -- | The name of the application that is associated with this environment.
    applicationName :: Lude.Text,
    -- | The name of an Elastic Beanstalk solution stack (platform version) to use with the environment. If specified, Elastic Beanstalk sets the configuration values to the default values associated with the specified solution stack. For a list of current solution stacks, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms> in the /AWS Elastic Beanstalk Platforms/ guide.
    solutionStackName :: Lude.Maybe Lude.Text,
    -- | The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name parameter. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
    groupName :: Lude.Maybe Lude.Text,
    -- | Your description for this environment.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies the tags applied to resources in the environment.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEnvironment' with the minimum fields required to make a request.
--
-- * 'cNAMEPrefix' - If specified, the environment attempts to use this value as the prefix for the CNAME in your Elastic Beanstalk environment URL. If not specified, the CNAME is generated automatically by appending a random alphanumeric string to the environment name.
-- * 'templateName' - The name of the Elastic Beanstalk configuration template to use with the environment.
-- * 'optionsToRemove' - A list of custom user-defined configuration options to remove from the configuration set for this new environment.
-- * 'optionSettings' - If specified, AWS Elastic Beanstalk sets the specified configuration options to the requested value in the configuration set for the new environment. These override the values obtained from the solution stack or the configuration template.
-- * 'versionLabel' - The name of the application version to deploy.
--
-- Default: If not specified, Elastic Beanstalk attempts to deploy the sample application.
-- * 'operationsRole' - The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role. If specified, Elastic Beanstalk uses the operations role for permissions to downstream services during this call and during subsequent calls acting on this environment. To specify an operations role, you must have the @iam:PassRole@ permission for the role. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
-- * 'platformARN' - The Amazon Resource Name (ARN) of the custom platform to use with the environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
-- * 'tier' - Specifies the tier to use in creating this environment. The environment tier that you choose determines whether Elastic Beanstalk provisions resources to support a web application that handles HTTP(S) requests or a web application that handles background-processing tasks.
-- * 'environmentName' - A unique name for the environment.
--
-- Constraint: Must be from 4 to 40 characters in length. The name can contain only letters, numbers, and hyphens. It can't start or end with a hyphen. This name must be unique within a region in your account. If the specified name already exists in the region, Elastic Beanstalk returns an @InvalidParameterValue@ error.
-- If you don't specify the @CNAMEPrefix@ parameter, the environment name becomes part of the CNAME, and therefore part of the visible URL for your application.
-- * 'applicationName' - The name of the application that is associated with this environment.
-- * 'solutionStackName' - The name of an Elastic Beanstalk solution stack (platform version) to use with the environment. If specified, Elastic Beanstalk sets the configuration values to the default values associated with the specified solution stack. For a list of current solution stacks, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms> in the /AWS Elastic Beanstalk Platforms/ guide.
-- * 'groupName' - The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name parameter. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
-- * 'description' - Your description for this environment.
-- * 'tags' - Specifies the tags applied to resources in the environment.
mkCreateEnvironment ::
  -- | 'applicationName'
  Lude.Text ->
  CreateEnvironment
mkCreateEnvironment pApplicationName_ =
  CreateEnvironment'
    { cNAMEPrefix = Lude.Nothing,
      templateName = Lude.Nothing,
      optionsToRemove = Lude.Nothing,
      optionSettings = Lude.Nothing,
      versionLabel = Lude.Nothing,
      operationsRole = Lude.Nothing,
      platformARN = Lude.Nothing,
      tier = Lude.Nothing,
      environmentName = Lude.Nothing,
      applicationName = pApplicationName_,
      solutionStackName = Lude.Nothing,
      groupName = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | If specified, the environment attempts to use this value as the prefix for the CNAME in your Elastic Beanstalk environment URL. If not specified, the CNAME is generated automatically by appending a random alphanumeric string to the environment name.
--
-- /Note:/ Consider using 'cNAMEPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCNAMEPrefix :: Lens.Lens' CreateEnvironment (Lude.Maybe Lude.Text)
cCNAMEPrefix = Lens.lens (cNAMEPrefix :: CreateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {cNAMEPrefix = a} :: CreateEnvironment)
{-# DEPRECATED cCNAMEPrefix "Use generic-lens or generic-optics with 'cNAMEPrefix' instead." #-}

-- | The name of the Elastic Beanstalk configuration template to use with the environment.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTemplateName :: Lens.Lens' CreateEnvironment (Lude.Maybe Lude.Text)
cTemplateName = Lens.lens (templateName :: CreateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: CreateEnvironment)
{-# DEPRECATED cTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | A list of custom user-defined configuration options to remove from the configuration set for this new environment.
--
-- /Note:/ Consider using 'optionsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOptionsToRemove :: Lens.Lens' CreateEnvironment (Lude.Maybe [OptionSpecification])
cOptionsToRemove = Lens.lens (optionsToRemove :: CreateEnvironment -> Lude.Maybe [OptionSpecification]) (\s a -> s {optionsToRemove = a} :: CreateEnvironment)
{-# DEPRECATED cOptionsToRemove "Use generic-lens or generic-optics with 'optionsToRemove' instead." #-}

-- | If specified, AWS Elastic Beanstalk sets the specified configuration options to the requested value in the configuration set for the new environment. These override the values obtained from the solution stack or the configuration template.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOptionSettings :: Lens.Lens' CreateEnvironment (Lude.Maybe [ConfigurationOptionSetting])
cOptionSettings = Lens.lens (optionSettings :: CreateEnvironment -> Lude.Maybe [ConfigurationOptionSetting]) (\s a -> s {optionSettings = a} :: CreateEnvironment)
{-# DEPRECATED cOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | The name of the application version to deploy.
--
-- Default: If not specified, Elastic Beanstalk attempts to deploy the sample application.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVersionLabel :: Lens.Lens' CreateEnvironment (Lude.Maybe Lude.Text)
cVersionLabel = Lens.lens (versionLabel :: CreateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: CreateEnvironment)
{-# DEPRECATED cVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role. If specified, Elastic Beanstalk uses the operations role for permissions to downstream services during this call and during subsequent calls acting on this environment. To specify an operations role, you must have the @iam:PassRole@ permission for the role. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /Note:/ Consider using 'operationsRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOperationsRole :: Lens.Lens' CreateEnvironment (Lude.Maybe Lude.Text)
cOperationsRole = Lens.lens (operationsRole :: CreateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {operationsRole = a} :: CreateEnvironment)
{-# DEPRECATED cOperationsRole "Use generic-lens or generic-optics with 'operationsRole' instead." #-}

-- | The Amazon Resource Name (ARN) of the custom platform to use with the environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPlatformARN :: Lens.Lens' CreateEnvironment (Lude.Maybe Lude.Text)
cPlatformARN = Lens.lens (platformARN :: CreateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: CreateEnvironment)
{-# DEPRECATED cPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | Specifies the tier to use in creating this environment. The environment tier that you choose determines whether Elastic Beanstalk provisions resources to support a web application that handles HTTP(S) requests or a web application that handles background-processing tasks.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTier :: Lens.Lens' CreateEnvironment (Lude.Maybe EnvironmentTier)
cTier = Lens.lens (tier :: CreateEnvironment -> Lude.Maybe EnvironmentTier) (\s a -> s {tier = a} :: CreateEnvironment)
{-# DEPRECATED cTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | A unique name for the environment.
--
-- Constraint: Must be from 4 to 40 characters in length. The name can contain only letters, numbers, and hyphens. It can't start or end with a hyphen. This name must be unique within a region in your account. If the specified name already exists in the region, Elastic Beanstalk returns an @InvalidParameterValue@ error.
-- If you don't specify the @CNAMEPrefix@ parameter, the environment name becomes part of the CNAME, and therefore part of the visible URL for your application.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnvironmentName :: Lens.Lens' CreateEnvironment (Lude.Maybe Lude.Text)
cEnvironmentName = Lens.lens (environmentName :: CreateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: CreateEnvironment)
{-# DEPRECATED cEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the application that is associated with this environment.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cApplicationName :: Lens.Lens' CreateEnvironment Lude.Text
cApplicationName = Lens.lens (applicationName :: CreateEnvironment -> Lude.Text) (\s a -> s {applicationName = a} :: CreateEnvironment)
{-# DEPRECATED cApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of an Elastic Beanstalk solution stack (platform version) to use with the environment. If specified, Elastic Beanstalk sets the configuration values to the default values associated with the specified solution stack. For a list of current solution stacks, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms> in the /AWS Elastic Beanstalk Platforms/ guide.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSolutionStackName :: Lens.Lens' CreateEnvironment (Lude.Maybe Lude.Text)
cSolutionStackName = Lens.lens (solutionStackName :: CreateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {solutionStackName = a} :: CreateEnvironment)
{-# DEPRECATED cSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name parameter. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGroupName :: Lens.Lens' CreateEnvironment (Lude.Maybe Lude.Text)
cGroupName = Lens.lens (groupName :: CreateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: CreateEnvironment)
{-# DEPRECATED cGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Your description for this environment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreateEnvironment (Lude.Maybe Lude.Text)
cDescription = Lens.lens (description :: CreateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateEnvironment)
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the tags applied to resources in the environment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateEnvironment (Lude.Maybe [Tag])
cTags = Lens.lens (tags :: CreateEnvironment -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateEnvironment)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateEnvironment where
  type Rs CreateEnvironment = EnvironmentDescription
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "CreateEnvironmentResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateEnvironment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateEnvironment where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEnvironment where
  toQuery CreateEnvironment' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateEnvironment" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "CNAMEPrefix" Lude.=: cNAMEPrefix,
        "TemplateName" Lude.=: templateName,
        "OptionsToRemove"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> optionsToRemove),
        "OptionSettings"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> optionSettings),
        "VersionLabel" Lude.=: versionLabel,
        "OperationsRole" Lude.=: operationsRole,
        "PlatformArn" Lude.=: platformARN,
        "Tier" Lude.=: tier,
        "EnvironmentName" Lude.=: environmentName,
        "ApplicationName" Lude.=: applicationName,
        "SolutionStackName" Lude.=: solutionStackName,
        "GroupName" Lude.=: groupName,
        "Description" Lude.=: description,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags)
      ]
