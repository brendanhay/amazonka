{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateEnvironment (..)
    , mkCreateEnvironment
    -- ** Request lenses
    , cApplicationName
    , cCNAMEPrefix
    , cDescription
    , cEnvironmentName
    , cGroupName
    , cOperationsRole
    , cOptionSettings
    , cOptionsToRemove
    , cPlatformArn
    , cSolutionStackName
    , cTags
    , cTemplateName
    , cTier
    , cVersionLabel

     -- * Destructuring the response
    , Types.EnvironmentDescription (..)
    , Types.mkEnvironmentDescription
    -- ** Response lenses
    , Types.eAbortableOperationInProgress
    , Types.eApplicationName
    , Types.eCNAME
    , Types.eDateCreated
    , Types.eDateUpdated
    , Types.eDescription
    , Types.eEndpointURL
    , Types.eEnvironmentArn
    , Types.eEnvironmentId
    , Types.eEnvironmentLinks
    , Types.eEnvironmentName
    , Types.eHealth
    , Types.eHealthStatus
    , Types.eOperationsRole
    , Types.ePlatformArn
    , Types.eResources
    , Types.eSolutionStackName
    , Types.eStatus
    , Types.eTemplateName
    , Types.eTier
    , Types.eVersionLabel
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the application that is associated with this environment.
  , cNAMEPrefix :: Core.Maybe Types.DNSCnamePrefix
    -- ^ If specified, the environment attempts to use this value as the prefix for the CNAME in your Elastic Beanstalk environment URL. If not specified, the CNAME is generated automatically by appending a random alphanumeric string to the environment name.
  , description :: Core.Maybe Types.Description
    -- ^ Your description for this environment.
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ A unique name for the environment.
--
-- Constraint: Must be from 4 to 40 characters in length. The name can contain only letters, numbers, and hyphens. It can't start or end with a hyphen. This name must be unique within a region in your account. If the specified name already exists in the region, Elastic Beanstalk returns an @InvalidParameterValue@ error. 
-- If you don't specify the @CNAMEPrefix@ parameter, the environment name becomes part of the CNAME, and therefore part of the visible URL for your application.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name parameter. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
  , operationsRole :: Core.Maybe Types.OperationsRole
    -- ^ The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role. If specified, Elastic Beanstalk uses the operations role for permissions to downstream services during this call and during subsequent calls acting on this environment. To specify an operations role, you must have the @iam:PassRole@ permission for the role. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
  , optionSettings :: Core.Maybe [Types.ConfigurationOptionSetting]
    -- ^ If specified, AWS Elastic Beanstalk sets the specified configuration options to the requested value in the configuration set for the new environment. These override the values obtained from the solution stack or the configuration template.
  , optionsToRemove :: Core.Maybe [Types.OptionSpecification]
    -- ^ A list of custom user-defined configuration options to remove from the configuration set for this new environment.
  , platformArn :: Core.Maybe Types.PlatformArn
    -- ^ The Amazon Resource Name (ARN) of the custom platform to use with the environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
  , solutionStackName :: Core.Maybe Types.SolutionStackName
    -- ^ The name of an Elastic Beanstalk solution stack (platform version) to use with the environment. If specified, Elastic Beanstalk sets the configuration values to the default values associated with the specified solution stack. For a list of current solution stacks, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms> in the /AWS Elastic Beanstalk Platforms/ guide.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Specifies the tags applied to resources in the environment.
  , templateName :: Core.Maybe Types.ConfigurationTemplateName
    -- ^ The name of the Elastic Beanstalk configuration template to use with the environment.
  , tier :: Core.Maybe Types.EnvironmentTier
    -- ^ Specifies the tier to use in creating this environment. The environment tier that you choose determines whether Elastic Beanstalk provisions resources to support a web application that handles HTTP(S) requests or a web application that handles background-processing tasks.
  , versionLabel :: Core.Maybe Types.VersionLabel
    -- ^ The name of the application version to deploy.
--
-- Default: If not specified, Elastic Beanstalk attempts to deploy the sample application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEnvironment' value with any optional fields omitted.
mkCreateEnvironment
    :: Types.ApplicationName -- ^ 'applicationName'
    -> CreateEnvironment
mkCreateEnvironment applicationName
  = CreateEnvironment'{applicationName, cNAMEPrefix = Core.Nothing,
                       description = Core.Nothing, environmentName = Core.Nothing,
                       groupName = Core.Nothing, operationsRole = Core.Nothing,
                       optionSettings = Core.Nothing, optionsToRemove = Core.Nothing,
                       platformArn = Core.Nothing, solutionStackName = Core.Nothing,
                       tags = Core.Nothing, templateName = Core.Nothing,
                       tier = Core.Nothing, versionLabel = Core.Nothing}

-- | The name of the application that is associated with this environment.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cApplicationName :: Lens.Lens' CreateEnvironment Types.ApplicationName
cApplicationName = Lens.field @"applicationName"
{-# INLINEABLE cApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | If specified, the environment attempts to use this value as the prefix for the CNAME in your Elastic Beanstalk environment URL. If not specified, the CNAME is generated automatically by appending a random alphanumeric string to the environment name.
--
-- /Note:/ Consider using 'cNAMEPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCNAMEPrefix :: Lens.Lens' CreateEnvironment (Core.Maybe Types.DNSCnamePrefix)
cCNAMEPrefix = Lens.field @"cNAMEPrefix"
{-# INLINEABLE cCNAMEPrefix #-}
{-# DEPRECATED cNAMEPrefix "Use generic-lens or generic-optics with 'cNAMEPrefix' instead"  #-}

-- | Your description for this environment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreateEnvironment (Core.Maybe Types.Description)
cDescription = Lens.field @"description"
{-# INLINEABLE cDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A unique name for the environment.
--
-- Constraint: Must be from 4 to 40 characters in length. The name can contain only letters, numbers, and hyphens. It can't start or end with a hyphen. This name must be unique within a region in your account. If the specified name already exists in the region, Elastic Beanstalk returns an @InvalidParameterValue@ error. 
-- If you don't specify the @CNAMEPrefix@ parameter, the environment name becomes part of the CNAME, and therefore part of the visible URL for your application.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnvironmentName :: Lens.Lens' CreateEnvironment (Core.Maybe Types.EnvironmentName)
cEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE cEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

-- | The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name parameter. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGroupName :: Lens.Lens' CreateEnvironment (Core.Maybe Types.GroupName)
cGroupName = Lens.field @"groupName"
{-# INLINEABLE cGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role. If specified, Elastic Beanstalk uses the operations role for permissions to downstream services during this call and during subsequent calls acting on this environment. To specify an operations role, you must have the @iam:PassRole@ permission for the role. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /Note:/ Consider using 'operationsRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOperationsRole :: Lens.Lens' CreateEnvironment (Core.Maybe Types.OperationsRole)
cOperationsRole = Lens.field @"operationsRole"
{-# INLINEABLE cOperationsRole #-}
{-# DEPRECATED operationsRole "Use generic-lens or generic-optics with 'operationsRole' instead"  #-}

-- | If specified, AWS Elastic Beanstalk sets the specified configuration options to the requested value in the configuration set for the new environment. These override the values obtained from the solution stack or the configuration template.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOptionSettings :: Lens.Lens' CreateEnvironment (Core.Maybe [Types.ConfigurationOptionSetting])
cOptionSettings = Lens.field @"optionSettings"
{-# INLINEABLE cOptionSettings #-}
{-# DEPRECATED optionSettings "Use generic-lens or generic-optics with 'optionSettings' instead"  #-}

-- | A list of custom user-defined configuration options to remove from the configuration set for this new environment.
--
-- /Note:/ Consider using 'optionsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOptionsToRemove :: Lens.Lens' CreateEnvironment (Core.Maybe [Types.OptionSpecification])
cOptionsToRemove = Lens.field @"optionsToRemove"
{-# INLINEABLE cOptionsToRemove #-}
{-# DEPRECATED optionsToRemove "Use generic-lens or generic-optics with 'optionsToRemove' instead"  #-}

-- | The Amazon Resource Name (ARN) of the custom platform to use with the environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPlatformArn :: Lens.Lens' CreateEnvironment (Core.Maybe Types.PlatformArn)
cPlatformArn = Lens.field @"platformArn"
{-# INLINEABLE cPlatformArn #-}
{-# DEPRECATED platformArn "Use generic-lens or generic-optics with 'platformArn' instead"  #-}

-- | The name of an Elastic Beanstalk solution stack (platform version) to use with the environment. If specified, Elastic Beanstalk sets the configuration values to the default values associated with the specified solution stack. For a list of current solution stacks, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/platforms/platforms-supported.html Elastic Beanstalk Supported Platforms> in the /AWS Elastic Beanstalk Platforms/ guide.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSolutionStackName :: Lens.Lens' CreateEnvironment (Core.Maybe Types.SolutionStackName)
cSolutionStackName = Lens.field @"solutionStackName"
{-# INLINEABLE cSolutionStackName #-}
{-# DEPRECATED solutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead"  #-}

-- | Specifies the tags applied to resources in the environment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateEnvironment (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The name of the Elastic Beanstalk configuration template to use with the environment.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTemplateName :: Lens.Lens' CreateEnvironment (Core.Maybe Types.ConfigurationTemplateName)
cTemplateName = Lens.field @"templateName"
{-# INLINEABLE cTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | Specifies the tier to use in creating this environment. The environment tier that you choose determines whether Elastic Beanstalk provisions resources to support a web application that handles HTTP(S) requests or a web application that handles background-processing tasks.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTier :: Lens.Lens' CreateEnvironment (Core.Maybe Types.EnvironmentTier)
cTier = Lens.field @"tier"
{-# INLINEABLE cTier #-}
{-# DEPRECATED tier "Use generic-lens or generic-optics with 'tier' instead"  #-}

-- | The name of the application version to deploy.
--
-- Default: If not specified, Elastic Beanstalk attempts to deploy the sample application.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVersionLabel :: Lens.Lens' CreateEnvironment (Core.Maybe Types.VersionLabel)
cVersionLabel = Lens.field @"versionLabel"
{-# INLINEABLE cVersionLabel #-}
{-# DEPRECATED versionLabel "Use generic-lens or generic-optics with 'versionLabel' instead"  #-}

instance Core.ToQuery CreateEnvironment where
        toQuery CreateEnvironment{..}
          = Core.toQueryPair "Action" ("CreateEnvironment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CNAMEPrefix") cNAMEPrefix
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OperationsRole")
                operationsRole
              Core.<>
              Core.toQueryPair "OptionSettings"
                (Core.maybe Core.mempty (Core.toQueryList "member") optionSettings)
              Core.<>
              Core.toQueryPair "OptionsToRemove"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   optionsToRemove)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PlatformArn") platformArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SolutionStackName")
                solutionStackName
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateName")
                templateName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Tier") tier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VersionLabel")
                versionLabel

instance Core.ToHeaders CreateEnvironment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateEnvironment where
        type Rs CreateEnvironment = Types.EnvironmentDescription
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
          = Response.receiveXMLWrapper "CreateEnvironmentResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
