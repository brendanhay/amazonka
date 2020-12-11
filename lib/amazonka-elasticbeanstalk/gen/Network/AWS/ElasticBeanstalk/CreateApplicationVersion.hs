{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateApplicationVersion (..),
    mkCreateApplicationVersion,

    -- ** Request lenses
    cavProcess,
    cavSourceBundle,
    cavAutoCreateApplication,
    cavSourceBuildInformation,
    cavDescription,
    cavBuildConfiguration,
    cavTags,
    cavApplicationName,
    cavVersionLabel,

    -- * Destructuring the response
    ApplicationVersionDescriptionMessage (..),
    mkApplicationVersionDescriptionMessage,

    -- ** Response lenses
    avdmApplicationVersion,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
  { process ::
      Lude.Maybe Lude.Bool,
    sourceBundle :: Lude.Maybe S3Location,
    autoCreateApplication ::
      Lude.Maybe Lude.Bool,
    sourceBuildInformation ::
      Lude.Maybe SourceBuildInformation,
    description :: Lude.Maybe Lude.Text,
    buildConfiguration ::
      Lude.Maybe BuildConfiguration,
    tags :: Lude.Maybe [Tag],
    applicationName :: Lude.Text,
    versionLabel :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplicationVersion' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application. If no application is found with this name, and @AutoCreateApplication@ is @false@ , returns an @InvalidParameterValue@ error.
-- * 'autoCreateApplication' - Set to @true@ to create an application with the specified name if it doesn't already exist.
-- * 'buildConfiguration' - Settings for an AWS CodeBuild build.
-- * 'description' - A description of this application version.
-- * 'process' - Pre-processes and validates the environment manifest (@env.yaml@ ) and configuration files (@*.config@ files in the @.ebextensions@ folder) in the source bundle. Validating configuration files can identify issues prior to deploying the application version to an environment.
--
-- You must turn processing on for application versions that you create using AWS CodeBuild or AWS CodeCommit. For application versions built from a source bundle in Amazon S3, processing is optional.
-- * 'sourceBuildInformation' - Specify a commit in an AWS CodeCommit Git repository to use as the source code for the application version.
-- * 'sourceBundle' - The Amazon S3 bucket and key that identify the location of the source bundle for this version.
--
-- Specify a source bundle in S3 or a commit in an AWS CodeCommit repository (with @SourceBuildInformation@ ), but not both. If neither @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic Beanstalk uses a sample application.
-- * 'tags' - Specifies the tags applied to the application version.
--
-- Elastic Beanstalk applies these tags only to the application version. Environments that use the application version don't inherit the tags.
-- * 'versionLabel' - A label identifying this version.
--
-- Constraint: Must be unique per application. If an application version already exists with this label for the specified application, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
mkCreateApplicationVersion ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'versionLabel'
  Lude.Text ->
  CreateApplicationVersion
mkCreateApplicationVersion pApplicationName_ pVersionLabel_ =
  CreateApplicationVersion'
    { process = Lude.Nothing,
      sourceBundle = Lude.Nothing,
      autoCreateApplication = Lude.Nothing,
      sourceBuildInformation = Lude.Nothing,
      description = Lude.Nothing,
      buildConfiguration = Lude.Nothing,
      tags = Lude.Nothing,
      applicationName = pApplicationName_,
      versionLabel = pVersionLabel_
    }

-- | Pre-processes and validates the environment manifest (@env.yaml@ ) and configuration files (@*.config@ files in the @.ebextensions@ folder) in the source bundle. Validating configuration files can identify issues prior to deploying the application version to an environment.
--
-- You must turn processing on for application versions that you create using AWS CodeBuild or AWS CodeCommit. For application versions built from a source bundle in Amazon S3, processing is optional.
--
-- /Note:/ Consider using 'process' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavProcess :: Lens.Lens' CreateApplicationVersion (Lude.Maybe Lude.Bool)
cavProcess = Lens.lens (process :: CreateApplicationVersion -> Lude.Maybe Lude.Bool) (\s a -> s {process = a} :: CreateApplicationVersion)
{-# DEPRECATED cavProcess "Use generic-lens or generic-optics with 'process' instead." #-}

-- | The Amazon S3 bucket and key that identify the location of the source bundle for this version.
--
-- Specify a source bundle in S3 or a commit in an AWS CodeCommit repository (with @SourceBuildInformation@ ), but not both. If neither @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic Beanstalk uses a sample application.
--
-- /Note:/ Consider using 'sourceBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSourceBundle :: Lens.Lens' CreateApplicationVersion (Lude.Maybe S3Location)
cavSourceBundle = Lens.lens (sourceBundle :: CreateApplicationVersion -> Lude.Maybe S3Location) (\s a -> s {sourceBundle = a} :: CreateApplicationVersion)
{-# DEPRECATED cavSourceBundle "Use generic-lens or generic-optics with 'sourceBundle' instead." #-}

-- | Set to @true@ to create an application with the specified name if it doesn't already exist.
--
-- /Note:/ Consider using 'autoCreateApplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavAutoCreateApplication :: Lens.Lens' CreateApplicationVersion (Lude.Maybe Lude.Bool)
cavAutoCreateApplication = Lens.lens (autoCreateApplication :: CreateApplicationVersion -> Lude.Maybe Lude.Bool) (\s a -> s {autoCreateApplication = a} :: CreateApplicationVersion)
{-# DEPRECATED cavAutoCreateApplication "Use generic-lens or generic-optics with 'autoCreateApplication' instead." #-}

-- | Specify a commit in an AWS CodeCommit Git repository to use as the source code for the application version.
--
-- /Note:/ Consider using 'sourceBuildInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSourceBuildInformation :: Lens.Lens' CreateApplicationVersion (Lude.Maybe SourceBuildInformation)
cavSourceBuildInformation = Lens.lens (sourceBuildInformation :: CreateApplicationVersion -> Lude.Maybe SourceBuildInformation) (\s a -> s {sourceBuildInformation = a} :: CreateApplicationVersion)
{-# DEPRECATED cavSourceBuildInformation "Use generic-lens or generic-optics with 'sourceBuildInformation' instead." #-}

-- | A description of this application version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavDescription :: Lens.Lens' CreateApplicationVersion (Lude.Maybe Lude.Text)
cavDescription = Lens.lens (description :: CreateApplicationVersion -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateApplicationVersion)
{-# DEPRECATED cavDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Settings for an AWS CodeBuild build.
--
-- /Note:/ Consider using 'buildConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavBuildConfiguration :: Lens.Lens' CreateApplicationVersion (Lude.Maybe BuildConfiguration)
cavBuildConfiguration = Lens.lens (buildConfiguration :: CreateApplicationVersion -> Lude.Maybe BuildConfiguration) (\s a -> s {buildConfiguration = a} :: CreateApplicationVersion)
{-# DEPRECATED cavBuildConfiguration "Use generic-lens or generic-optics with 'buildConfiguration' instead." #-}

-- | Specifies the tags applied to the application version.
--
-- Elastic Beanstalk applies these tags only to the application version. Environments that use the application version don't inherit the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavTags :: Lens.Lens' CreateApplicationVersion (Lude.Maybe [Tag])
cavTags = Lens.lens (tags :: CreateApplicationVersion -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateApplicationVersion)
{-# DEPRECATED cavTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the application. If no application is found with this name, and @AutoCreateApplication@ is @false@ , returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavApplicationName :: Lens.Lens' CreateApplicationVersion Lude.Text
cavApplicationName = Lens.lens (applicationName :: CreateApplicationVersion -> Lude.Text) (\s a -> s {applicationName = a} :: CreateApplicationVersion)
{-# DEPRECATED cavApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | A label identifying this version.
--
-- Constraint: Must be unique per application. If an application version already exists with this label for the specified application, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavVersionLabel :: Lens.Lens' CreateApplicationVersion Lude.Text
cavVersionLabel = Lens.lens (versionLabel :: CreateApplicationVersion -> Lude.Text) (\s a -> s {versionLabel = a} :: CreateApplicationVersion)
{-# DEPRECATED cavVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

instance Lude.AWSRequest CreateApplicationVersion where
  type
    Rs CreateApplicationVersion =
      ApplicationVersionDescriptionMessage
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "CreateApplicationVersionResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateApplicationVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateApplicationVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateApplicationVersion where
  toQuery CreateApplicationVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateApplicationVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Process" Lude.=: process,
        "SourceBundle" Lude.=: sourceBundle,
        "AutoCreateApplication" Lude.=: autoCreateApplication,
        "SourceBuildInformation" Lude.=: sourceBuildInformation,
        "Description" Lude.=: description,
        "BuildConfiguration" Lude.=: buildConfiguration,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "ApplicationName" Lude.=: applicationName,
        "VersionLabel" Lude.=: versionLabel
      ]
