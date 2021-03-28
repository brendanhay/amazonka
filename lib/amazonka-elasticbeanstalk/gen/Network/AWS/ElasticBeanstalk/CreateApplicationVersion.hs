{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateApplicationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application version for the specified application. You can create an application version from a source bundle in Amazon S3, a commit in AWS CodeCommit, or the output of an AWS CodeBuild build as follows:
--
-- Specify a commit in an AWS CodeCommit repository with @SourceBuildInformation@ .
-- Specify a build in an AWS CodeBuild with @SourceBuildInformation@ and @BuildConfiguration@ .
-- Specify a source bundle in S3 with @SourceBundle@ 
-- Omit both @SourceBuildInformation@ and @SourceBundle@ to use the default sample application.
module Network.AWS.ElasticBeanstalk.CreateApplicationVersion
    (
    -- * Creating a request
      CreateApplicationVersion (..)
    , mkCreateApplicationVersion
    -- ** Request lenses
    , cavApplicationName
    , cavVersionLabel
    , cavAutoCreateApplication
    , cavBuildConfiguration
    , cavDescription
    , cavProcess
    , cavSourceBuildInformation
    , cavSourceBundle
    , cavTags

     -- * Destructuring the response
    , Types.ApplicationVersionDescriptionMessage (..)
    , Types.mkApplicationVersionDescriptionMessage
    -- ** Response lenses
    , Types.avdmApplicationVersion
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the application. If no application is found with this name, and @AutoCreateApplication@ is @false@ , returns an @InvalidParameterValue@ error. 
  , versionLabel :: Types.VersionLabel
    -- ^ A label identifying this version.
--
-- Constraint: Must be unique per application. If an application version already exists with this label for the specified application, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error. 
  , autoCreateApplication :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to create an application with the specified name if it doesn't already exist.
  , buildConfiguration :: Core.Maybe Types.BuildConfiguration
    -- ^ Settings for an AWS CodeBuild build.
  , description :: Core.Maybe Types.Description
    -- ^ A description of this application version.
  , process :: Core.Maybe Core.Bool
    -- ^ Pre-processes and validates the environment manifest (@env.yaml@ ) and configuration files (@*.config@ files in the @.ebextensions@ folder) in the source bundle. Validating configuration files can identify issues prior to deploying the application version to an environment.
--
-- You must turn processing on for application versions that you create using AWS CodeBuild or AWS CodeCommit. For application versions built from a source bundle in Amazon S3, processing is optional.
  , sourceBuildInformation :: Core.Maybe Types.SourceBuildInformation
    -- ^ Specify a commit in an AWS CodeCommit Git repository to use as the source code for the application version.
  , sourceBundle :: Core.Maybe Types.S3Location
    -- ^ The Amazon S3 bucket and key that identify the location of the source bundle for this version.
--
-- Specify a source bundle in S3 or a commit in an AWS CodeCommit repository (with @SourceBuildInformation@ ), but not both. If neither @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic Beanstalk uses a sample application.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Specifies the tags applied to the application version.
--
-- Elastic Beanstalk applies these tags only to the application version. Environments that use the application version don't inherit the tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplicationVersion' value with any optional fields omitted.
mkCreateApplicationVersion
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.VersionLabel -- ^ 'versionLabel'
    -> CreateApplicationVersion
mkCreateApplicationVersion applicationName versionLabel
  = CreateApplicationVersion'{applicationName, versionLabel,
                              autoCreateApplication = Core.Nothing,
                              buildConfiguration = Core.Nothing, description = Core.Nothing,
                              process = Core.Nothing, sourceBuildInformation = Core.Nothing,
                              sourceBundle = Core.Nothing, tags = Core.Nothing}

-- | The name of the application. If no application is found with this name, and @AutoCreateApplication@ is @false@ , returns an @InvalidParameterValue@ error. 
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavApplicationName :: Lens.Lens' CreateApplicationVersion Types.ApplicationName
cavApplicationName = Lens.field @"applicationName"
{-# INLINEABLE cavApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | A label identifying this version.
--
-- Constraint: Must be unique per application. If an application version already exists with this label for the specified application, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error. 
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavVersionLabel :: Lens.Lens' CreateApplicationVersion Types.VersionLabel
cavVersionLabel = Lens.field @"versionLabel"
{-# INLINEABLE cavVersionLabel #-}
{-# DEPRECATED versionLabel "Use generic-lens or generic-optics with 'versionLabel' instead"  #-}

-- | Set to @true@ to create an application with the specified name if it doesn't already exist.
--
-- /Note:/ Consider using 'autoCreateApplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavAutoCreateApplication :: Lens.Lens' CreateApplicationVersion (Core.Maybe Core.Bool)
cavAutoCreateApplication = Lens.field @"autoCreateApplication"
{-# INLINEABLE cavAutoCreateApplication #-}
{-# DEPRECATED autoCreateApplication "Use generic-lens or generic-optics with 'autoCreateApplication' instead"  #-}

-- | Settings for an AWS CodeBuild build.
--
-- /Note:/ Consider using 'buildConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavBuildConfiguration :: Lens.Lens' CreateApplicationVersion (Core.Maybe Types.BuildConfiguration)
cavBuildConfiguration = Lens.field @"buildConfiguration"
{-# INLINEABLE cavBuildConfiguration #-}
{-# DEPRECATED buildConfiguration "Use generic-lens or generic-optics with 'buildConfiguration' instead"  #-}

-- | A description of this application version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavDescription :: Lens.Lens' CreateApplicationVersion (Core.Maybe Types.Description)
cavDescription = Lens.field @"description"
{-# INLINEABLE cavDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Pre-processes and validates the environment manifest (@env.yaml@ ) and configuration files (@*.config@ files in the @.ebextensions@ folder) in the source bundle. Validating configuration files can identify issues prior to deploying the application version to an environment.
--
-- You must turn processing on for application versions that you create using AWS CodeBuild or AWS CodeCommit. For application versions built from a source bundle in Amazon S3, processing is optional.
--
-- /Note:/ Consider using 'process' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavProcess :: Lens.Lens' CreateApplicationVersion (Core.Maybe Core.Bool)
cavProcess = Lens.field @"process"
{-# INLINEABLE cavProcess #-}
{-# DEPRECATED process "Use generic-lens or generic-optics with 'process' instead"  #-}

-- | Specify a commit in an AWS CodeCommit Git repository to use as the source code for the application version.
--
-- /Note:/ Consider using 'sourceBuildInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSourceBuildInformation :: Lens.Lens' CreateApplicationVersion (Core.Maybe Types.SourceBuildInformation)
cavSourceBuildInformation = Lens.field @"sourceBuildInformation"
{-# INLINEABLE cavSourceBuildInformation #-}
{-# DEPRECATED sourceBuildInformation "Use generic-lens or generic-optics with 'sourceBuildInformation' instead"  #-}

-- | The Amazon S3 bucket and key that identify the location of the source bundle for this version.
--
-- Specify a source bundle in S3 or a commit in an AWS CodeCommit repository (with @SourceBuildInformation@ ), but not both. If neither @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic Beanstalk uses a sample application.
--
-- /Note:/ Consider using 'sourceBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSourceBundle :: Lens.Lens' CreateApplicationVersion (Core.Maybe Types.S3Location)
cavSourceBundle = Lens.field @"sourceBundle"
{-# INLINEABLE cavSourceBundle #-}
{-# DEPRECATED sourceBundle "Use generic-lens or generic-optics with 'sourceBundle' instead"  #-}

-- | Specifies the tags applied to the application version.
--
-- Elastic Beanstalk applies these tags only to the application version. Environments that use the application version don't inherit the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavTags :: Lens.Lens' CreateApplicationVersion (Core.Maybe [Types.Tag])
cavTags = Lens.field @"tags"
{-# INLINEABLE cavTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateApplicationVersion where
        toQuery CreateApplicationVersion{..}
          = Core.toQueryPair "Action"
              ("CreateApplicationVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<> Core.toQueryPair "VersionLabel" versionLabel
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoCreateApplication")
                autoCreateApplication
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BuildConfiguration")
                buildConfiguration
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Process") process
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceBuildInformation")
                sourceBuildInformation
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceBundle")
                sourceBundle
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)

instance Core.ToHeaders CreateApplicationVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateApplicationVersion where
        type Rs CreateApplicationVersion =
             Types.ApplicationVersionDescriptionMessage
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
          = Response.receiveXMLWrapper "CreateApplicationVersionResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
