{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
    (
    -- * Creating a request
      CreateConfigurationTemplate (..)
    , mkCreateConfigurationTemplate
    -- ** Request lenses
    , cctApplicationName
    , cctTemplateName
    , cctDescription
    , cctEnvironmentId
    , cctOptionSettings
    , cctPlatformArn
    , cctSolutionStackName
    , cctSourceConfiguration
    , cctTags

     -- * Destructuring the response
    , Types.ConfigurationSettingsDescription (..)
    , Types.mkConfigurationSettingsDescription
    -- ** Response lenses
    , Types.csdApplicationName
    , Types.csdDateCreated
    , Types.csdDateUpdated
    , Types.csdDeploymentStatus
    , Types.csdDescription
    , Types.csdEnvironmentName
    , Types.csdOptionSettings
    , Types.csdPlatformArn
    , Types.csdSolutionStackName
    , Types.csdTemplateName
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create a configuration template.
--
-- /See:/ 'mkCreateConfigurationTemplate' smart constructor.
data CreateConfigurationTemplate = CreateConfigurationTemplate'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the Elastic Beanstalk application to associate with this configuration template.
  , templateName :: Types.TemplateName
    -- ^ The name of the configuration template.
--
-- Constraint: This name must be unique per application.
  , description :: Core.Maybe Types.Description
    -- ^ An optional description for this configuration.
  , environmentId :: Core.Maybe Types.EnvironmentId
    -- ^ The ID of an environment whose settings you want to use to create the configuration template. You must specify @EnvironmentId@ if you don't specify @PlatformArn@ , @SolutionStackName@ , or @SourceConfiguration@ .
  , optionSettings :: Core.Maybe [Types.ConfigurationOptionSetting]
    -- ^ Option values for the Elastic Beanstalk configuration, such as the instance type. If specified, these values override the values obtained from the solution stack or the source configuration template. For a complete list of Elastic Beanstalk configuration options, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
  , platformArn :: Core.Maybe Types.PlatformArn
    -- ^ The Amazon Resource Name (ARN) of the custom platform. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
  , solutionStackName :: Core.Maybe Types.SolutionStackName
    -- ^ The name of an Elastic Beanstalk solution stack (platform version) that this configuration uses. For example, @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@ . A solution stack specifies the operating system, runtime, and application server for a configuration template. It also determines the set of configuration options as well as the possible and default values. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- You must specify @SolutionStackName@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SourceConfiguration@ .
-- Use the <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html @ListAvailableSolutionStacks@ > API to obtain a list of available solution stacks.
  , sourceConfiguration :: Core.Maybe Types.SourceConfiguration
    -- ^ An Elastic Beanstalk configuration template to base this one on. If specified, Elastic Beanstalk uses the configuration values from the specified configuration template to create a new configuration.
--
-- Values specified in @OptionSettings@ override any values obtained from the @SourceConfiguration@ .
-- You must specify @SourceConfiguration@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SolutionStackName@ .
-- Constraint: If both solution stack name and source configuration are specified, the solution stack of the source configuration template must match the specified solution stack name.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Specifies the tags applied to the configuration template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConfigurationTemplate' value with any optional fields omitted.
mkCreateConfigurationTemplate
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.TemplateName -- ^ 'templateName'
    -> CreateConfigurationTemplate
mkCreateConfigurationTemplate applicationName templateName
  = CreateConfigurationTemplate'{applicationName, templateName,
                                 description = Core.Nothing, environmentId = Core.Nothing,
                                 optionSettings = Core.Nothing, platformArn = Core.Nothing,
                                 solutionStackName = Core.Nothing,
                                 sourceConfiguration = Core.Nothing, tags = Core.Nothing}

-- | The name of the Elastic Beanstalk application to associate with this configuration template.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctApplicationName :: Lens.Lens' CreateConfigurationTemplate Types.ApplicationName
cctApplicationName = Lens.field @"applicationName"
{-# INLINEABLE cctApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The name of the configuration template.
--
-- Constraint: This name must be unique per application.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctTemplateName :: Lens.Lens' CreateConfigurationTemplate Types.TemplateName
cctTemplateName = Lens.field @"templateName"
{-# INLINEABLE cctTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | An optional description for this configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctDescription :: Lens.Lens' CreateConfigurationTemplate (Core.Maybe Types.Description)
cctDescription = Lens.field @"description"
{-# INLINEABLE cctDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of an environment whose settings you want to use to create the configuration template. You must specify @EnvironmentId@ if you don't specify @PlatformArn@ , @SolutionStackName@ , or @SourceConfiguration@ .
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctEnvironmentId :: Lens.Lens' CreateConfigurationTemplate (Core.Maybe Types.EnvironmentId)
cctEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE cctEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | Option values for the Elastic Beanstalk configuration, such as the instance type. If specified, these values override the values obtained from the solution stack or the source configuration template. For a complete list of Elastic Beanstalk configuration options, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctOptionSettings :: Lens.Lens' CreateConfigurationTemplate (Core.Maybe [Types.ConfigurationOptionSetting])
cctOptionSettings = Lens.field @"optionSettings"
{-# INLINEABLE cctOptionSettings #-}
{-# DEPRECATED optionSettings "Use generic-lens or generic-optics with 'optionSettings' instead"  #-}

-- | The Amazon Resource Name (ARN) of the custom platform. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctPlatformArn :: Lens.Lens' CreateConfigurationTemplate (Core.Maybe Types.PlatformArn)
cctPlatformArn = Lens.field @"platformArn"
{-# INLINEABLE cctPlatformArn #-}
{-# DEPRECATED platformArn "Use generic-lens or generic-optics with 'platformArn' instead"  #-}

-- | The name of an Elastic Beanstalk solution stack (platform version) that this configuration uses. For example, @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@ . A solution stack specifies the operating system, runtime, and application server for a configuration template. It also determines the set of configuration options as well as the possible and default values. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- You must specify @SolutionStackName@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SourceConfiguration@ .
-- Use the <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html @ListAvailableSolutionStacks@ > API to obtain a list of available solution stacks.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctSolutionStackName :: Lens.Lens' CreateConfigurationTemplate (Core.Maybe Types.SolutionStackName)
cctSolutionStackName = Lens.field @"solutionStackName"
{-# INLINEABLE cctSolutionStackName #-}
{-# DEPRECATED solutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead"  #-}

-- | An Elastic Beanstalk configuration template to base this one on. If specified, Elastic Beanstalk uses the configuration values from the specified configuration template to create a new configuration.
--
-- Values specified in @OptionSettings@ override any values obtained from the @SourceConfiguration@ .
-- You must specify @SourceConfiguration@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SolutionStackName@ .
-- Constraint: If both solution stack name and source configuration are specified, the solution stack of the source configuration template must match the specified solution stack name.
--
-- /Note:/ Consider using 'sourceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctSourceConfiguration :: Lens.Lens' CreateConfigurationTemplate (Core.Maybe Types.SourceConfiguration)
cctSourceConfiguration = Lens.field @"sourceConfiguration"
{-# INLINEABLE cctSourceConfiguration #-}
{-# DEPRECATED sourceConfiguration "Use generic-lens or generic-optics with 'sourceConfiguration' instead"  #-}

-- | Specifies the tags applied to the configuration template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctTags :: Lens.Lens' CreateConfigurationTemplate (Core.Maybe [Types.Tag])
cctTags = Lens.field @"tags"
{-# INLINEABLE cctTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateConfigurationTemplate where
        toQuery CreateConfigurationTemplate{..}
          = Core.toQueryPair "Action"
              ("CreateConfigurationTemplate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<> Core.toQueryPair "TemplateName" templateName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentId")
                environmentId
              Core.<>
              Core.toQueryPair "OptionSettings"
                (Core.maybe Core.mempty (Core.toQueryList "member") optionSettings)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PlatformArn") platformArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SolutionStackName")
                solutionStackName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceConfiguration")
                sourceConfiguration
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)

instance Core.ToHeaders CreateConfigurationTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateConfigurationTemplate where
        type Rs CreateConfigurationTemplate =
             Types.ConfigurationSettingsDescription
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateConfigurationTemplateResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
