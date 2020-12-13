{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Elastic Beanstalk configuration template, associated with a specific Elastic Beanstalk application. You define application configuration settings in a configuration template. You can then use the configuration template to deploy different versions of the application with the same configuration settings.
--
-- Templates aren't associated with any environment. The @EnvironmentName@ response element is always @null@ .
-- Related Topics
--
--     * 'DescribeConfigurationOptions'
--
--
--     * 'DescribeConfigurationSettings'
--
--
--     * 'ListAvailableSolutionStacks'
module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
  ( -- * Creating a request
    CreateConfigurationTemplate (..),
    mkCreateConfigurationTemplate,

    -- ** Request lenses
    cctTemplateName,
    cctOptionSettings,
    cctPlatformARN,
    cctApplicationName,
    cctSourceConfiguration,
    cctSolutionStackName,
    cctEnvironmentId,
    cctDescription,
    cctTags,

    -- * Destructuring the response
    ConfigurationSettingsDescription (..),
    mkConfigurationSettingsDescription,

    -- ** Response lenses
    csdTemplateName,
    csdOptionSettings,
    csdDateUpdated,
    csdDateCreated,
    csdPlatformARN,
    csdEnvironmentName,
    csdApplicationName,
    csdDeploymentStatus,
    csdSolutionStackName,
    csdDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to create a configuration template.
--
-- /See:/ 'mkCreateConfigurationTemplate' smart constructor.
data CreateConfigurationTemplate = CreateConfigurationTemplate'
  { -- | The name of the configuration template.
    --
    -- Constraint: This name must be unique per application.
    templateName :: Lude.Text,
    -- | Option values for the Elastic Beanstalk configuration, such as the instance type. If specified, these values override the values obtained from the solution stack or the source configuration template. For a complete list of Elastic Beanstalk configuration options, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
    optionSettings :: Lude.Maybe [ConfigurationOptionSetting],
    -- | The Amazon Resource Name (ARN) of the custom platform. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
    platformARN :: Lude.Maybe Lude.Text,
    -- | The name of the Elastic Beanstalk application to associate with this configuration template.
    applicationName :: Lude.Text,
    -- | An Elastic Beanstalk configuration template to base this one on. If specified, Elastic Beanstalk uses the configuration values from the specified configuration template to create a new configuration.
    --
    -- Values specified in @OptionSettings@ override any values obtained from the @SourceConfiguration@ .
    -- You must specify @SourceConfiguration@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SolutionStackName@ .
    -- Constraint: If both solution stack name and source configuration are specified, the solution stack of the source configuration template must match the specified solution stack name.
    sourceConfiguration :: Lude.Maybe SourceConfiguration,
    -- | The name of an Elastic Beanstalk solution stack (platform version) that this configuration uses. For example, @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@ . A solution stack specifies the operating system, runtime, and application server for a configuration template. It also determines the set of configuration options as well as the possible and default values. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
    --
    -- You must specify @SolutionStackName@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SourceConfiguration@ .
    -- Use the <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html @ListAvailableSolutionStacks@ > API to obtain a list of available solution stacks.
    solutionStackName :: Lude.Maybe Lude.Text,
    -- | The ID of an environment whose settings you want to use to create the configuration template. You must specify @EnvironmentId@ if you don't specify @PlatformArn@ , @SolutionStackName@ , or @SourceConfiguration@ .
    environmentId :: Lude.Maybe Lude.Text,
    -- | An optional description for this configuration.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies the tags applied to the configuration template.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConfigurationTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the configuration template.
--
-- Constraint: This name must be unique per application.
-- * 'optionSettings' - Option values for the Elastic Beanstalk configuration, such as the instance type. If specified, these values override the values obtained from the solution stack or the source configuration template. For a complete list of Elastic Beanstalk configuration options, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
-- * 'platformARN' - The Amazon Resource Name (ARN) of the custom platform. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
-- * 'applicationName' - The name of the Elastic Beanstalk application to associate with this configuration template.
-- * 'sourceConfiguration' - An Elastic Beanstalk configuration template to base this one on. If specified, Elastic Beanstalk uses the configuration values from the specified configuration template to create a new configuration.
--
-- Values specified in @OptionSettings@ override any values obtained from the @SourceConfiguration@ .
-- You must specify @SourceConfiguration@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SolutionStackName@ .
-- Constraint: If both solution stack name and source configuration are specified, the solution stack of the source configuration template must match the specified solution stack name.
-- * 'solutionStackName' - The name of an Elastic Beanstalk solution stack (platform version) that this configuration uses. For example, @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@ . A solution stack specifies the operating system, runtime, and application server for a configuration template. It also determines the set of configuration options as well as the possible and default values. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- You must specify @SolutionStackName@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SourceConfiguration@ .
-- Use the <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html @ListAvailableSolutionStacks@ > API to obtain a list of available solution stacks.
-- * 'environmentId' - The ID of an environment whose settings you want to use to create the configuration template. You must specify @EnvironmentId@ if you don't specify @PlatformArn@ , @SolutionStackName@ , or @SourceConfiguration@ .
-- * 'description' - An optional description for this configuration.
-- * 'tags' - Specifies the tags applied to the configuration template.
mkCreateConfigurationTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'applicationName'
  Lude.Text ->
  CreateConfigurationTemplate
mkCreateConfigurationTemplate pTemplateName_ pApplicationName_ =
  CreateConfigurationTemplate'
    { templateName = pTemplateName_,
      optionSettings = Lude.Nothing,
      platformARN = Lude.Nothing,
      applicationName = pApplicationName_,
      sourceConfiguration = Lude.Nothing,
      solutionStackName = Lude.Nothing,
      environmentId = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the configuration template.
--
-- Constraint: This name must be unique per application.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctTemplateName :: Lens.Lens' CreateConfigurationTemplate Lude.Text
cctTemplateName = Lens.lens (templateName :: CreateConfigurationTemplate -> Lude.Text) (\s a -> s {templateName = a} :: CreateConfigurationTemplate)
{-# DEPRECATED cctTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Option values for the Elastic Beanstalk configuration, such as the instance type. If specified, these values override the values obtained from the solution stack or the source configuration template. For a complete list of Elastic Beanstalk configuration options, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctOptionSettings :: Lens.Lens' CreateConfigurationTemplate (Lude.Maybe [ConfigurationOptionSetting])
cctOptionSettings = Lens.lens (optionSettings :: CreateConfigurationTemplate -> Lude.Maybe [ConfigurationOptionSetting]) (\s a -> s {optionSettings = a} :: CreateConfigurationTemplate)
{-# DEPRECATED cctOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | The Amazon Resource Name (ARN) of the custom platform. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctPlatformARN :: Lens.Lens' CreateConfigurationTemplate (Lude.Maybe Lude.Text)
cctPlatformARN = Lens.lens (platformARN :: CreateConfigurationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: CreateConfigurationTemplate)
{-# DEPRECATED cctPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | The name of the Elastic Beanstalk application to associate with this configuration template.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctApplicationName :: Lens.Lens' CreateConfigurationTemplate Lude.Text
cctApplicationName = Lens.lens (applicationName :: CreateConfigurationTemplate -> Lude.Text) (\s a -> s {applicationName = a} :: CreateConfigurationTemplate)
{-# DEPRECATED cctApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | An Elastic Beanstalk configuration template to base this one on. If specified, Elastic Beanstalk uses the configuration values from the specified configuration template to create a new configuration.
--
-- Values specified in @OptionSettings@ override any values obtained from the @SourceConfiguration@ .
-- You must specify @SourceConfiguration@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SolutionStackName@ .
-- Constraint: If both solution stack name and source configuration are specified, the solution stack of the source configuration template must match the specified solution stack name.
--
-- /Note:/ Consider using 'sourceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctSourceConfiguration :: Lens.Lens' CreateConfigurationTemplate (Lude.Maybe SourceConfiguration)
cctSourceConfiguration = Lens.lens (sourceConfiguration :: CreateConfigurationTemplate -> Lude.Maybe SourceConfiguration) (\s a -> s {sourceConfiguration = a} :: CreateConfigurationTemplate)
{-# DEPRECATED cctSourceConfiguration "Use generic-lens or generic-optics with 'sourceConfiguration' instead." #-}

-- | The name of an Elastic Beanstalk solution stack (platform version) that this configuration uses. For example, @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@ . A solution stack specifies the operating system, runtime, and application server for a configuration template. It also determines the set of configuration options as well as the possible and default values. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- You must specify @SolutionStackName@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SourceConfiguration@ .
-- Use the <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html @ListAvailableSolutionStacks@ > API to obtain a list of available solution stacks.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctSolutionStackName :: Lens.Lens' CreateConfigurationTemplate (Lude.Maybe Lude.Text)
cctSolutionStackName = Lens.lens (solutionStackName :: CreateConfigurationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {solutionStackName = a} :: CreateConfigurationTemplate)
{-# DEPRECATED cctSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | The ID of an environment whose settings you want to use to create the configuration template. You must specify @EnvironmentId@ if you don't specify @PlatformArn@ , @SolutionStackName@ , or @SourceConfiguration@ .
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctEnvironmentId :: Lens.Lens' CreateConfigurationTemplate (Lude.Maybe Lude.Text)
cctEnvironmentId = Lens.lens (environmentId :: CreateConfigurationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: CreateConfigurationTemplate)
{-# DEPRECATED cctEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | An optional description for this configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctDescription :: Lens.Lens' CreateConfigurationTemplate (Lude.Maybe Lude.Text)
cctDescription = Lens.lens (description :: CreateConfigurationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateConfigurationTemplate)
{-# DEPRECATED cctDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the tags applied to the configuration template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctTags :: Lens.Lens' CreateConfigurationTemplate (Lude.Maybe [Tag])
cctTags = Lens.lens (tags :: CreateConfigurationTemplate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateConfigurationTemplate)
{-# DEPRECATED cctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateConfigurationTemplate where
  type
    Rs CreateConfigurationTemplate =
      ConfigurationSettingsDescription
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "CreateConfigurationTemplateResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateConfigurationTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateConfigurationTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConfigurationTemplate where
  toQuery CreateConfigurationTemplate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateConfigurationTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName,
        "OptionSettings"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> optionSettings),
        "PlatformArn" Lude.=: platformARN,
        "ApplicationName" Lude.=: applicationName,
        "SourceConfiguration" Lude.=: sourceConfiguration,
        "SolutionStackName" Lude.=: solutionStackName,
        "EnvironmentId" Lude.=: environmentId,
        "Description" Lude.=: description,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags)
      ]
