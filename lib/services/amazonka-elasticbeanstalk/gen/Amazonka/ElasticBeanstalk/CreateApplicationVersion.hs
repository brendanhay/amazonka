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
-- Module      : Amazonka.ElasticBeanstalk.CreateApplicationVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application version for the specified application. You can
-- create an application version from a source bundle in Amazon S3, a
-- commit in AWS CodeCommit, or the output of an AWS CodeBuild build as
-- follows:
--
-- Specify a commit in an AWS CodeCommit repository with
-- @SourceBuildInformation@.
--
-- Specify a build in an AWS CodeBuild with @SourceBuildInformation@ and
-- @BuildConfiguration@.
--
-- Specify a source bundle in S3 with @SourceBundle@
--
-- Omit both @SourceBuildInformation@ and @SourceBundle@ to use the default
-- sample application.
--
-- After you create an application version with a specified Amazon S3
-- bucket and key location, you can\'t change that Amazon S3 location. If
-- you change the Amazon S3 location, you receive an exception when you
-- attempt to launch an environment from the application version.
module Amazonka.ElasticBeanstalk.CreateApplicationVersion
  ( -- * Creating a Request
    CreateApplicationVersion (..),
    newCreateApplicationVersion,

    -- * Request Lenses
    createApplicationVersion_process,
    createApplicationVersion_sourceBundle,
    createApplicationVersion_autoCreateApplication,
    createApplicationVersion_sourceBuildInformation,
    createApplicationVersion_description,
    createApplicationVersion_buildConfiguration,
    createApplicationVersion_tags,
    createApplicationVersion_applicationName,
    createApplicationVersion_versionLabel,

    -- * Destructuring the Response
    ApplicationVersionDescriptionMessage (..),
    newApplicationVersionDescriptionMessage,

    -- * Response Lenses
    applicationVersionDescriptionMessage_applicationVersion,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
  { -- | Pre-processes and validates the environment manifest (@env.yaml@) and
    -- configuration files (@*.config@ files in the @.ebextensions@ folder) in
    -- the source bundle. Validating configuration files can identify issues
    -- prior to deploying the application version to an environment.
    --
    -- You must turn processing on for application versions that you create
    -- using AWS CodeBuild or AWS CodeCommit. For application versions built
    -- from a source bundle in Amazon S3, processing is optional.
    --
    -- The @Process@ option validates Elastic Beanstalk configuration files. It
    -- doesn\'t validate your application\'s configuration files, like proxy
    -- server or Docker configuration.
    process :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon S3 bucket and key that identify the location of the source
    -- bundle for this version.
    --
    -- The Amazon S3 bucket must be in the same region as the environment.
    --
    -- Specify a source bundle in S3 or a commit in an AWS CodeCommit
    -- repository (with @SourceBuildInformation@), but not both. If neither
    -- @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic
    -- Beanstalk uses a sample application.
    sourceBundle :: Prelude.Maybe S3Location,
    -- | Set to @true@ to create an application with the specified name if it
    -- doesn\'t already exist.
    autoCreateApplication :: Prelude.Maybe Prelude.Bool,
    -- | Specify a commit in an AWS CodeCommit Git repository to use as the
    -- source code for the application version.
    sourceBuildInformation :: Prelude.Maybe SourceBuildInformation,
    -- | A description of this application version.
    description :: Prelude.Maybe Prelude.Text,
    -- | Settings for an AWS CodeBuild build.
    buildConfiguration :: Prelude.Maybe BuildConfiguration,
    -- | Specifies the tags applied to the application version.
    --
    -- Elastic Beanstalk applies these tags only to the application version.
    -- Environments that use the application version don\'t inherit the tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the application. If no application is found with this name,
    -- and @AutoCreateApplication@ is @false@, returns an
    -- @InvalidParameterValue@ error.
    applicationName :: Prelude.Text,
    -- | A label identifying this version.
    --
    -- Constraint: Must be unique per application. If an application version
    -- already exists with this label for the specified application, AWS
    -- Elastic Beanstalk returns an @InvalidParameterValue@ error.
    versionLabel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'process', 'createApplicationVersion_process' - Pre-processes and validates the environment manifest (@env.yaml@) and
-- configuration files (@*.config@ files in the @.ebextensions@ folder) in
-- the source bundle. Validating configuration files can identify issues
-- prior to deploying the application version to an environment.
--
-- You must turn processing on for application versions that you create
-- using AWS CodeBuild or AWS CodeCommit. For application versions built
-- from a source bundle in Amazon S3, processing is optional.
--
-- The @Process@ option validates Elastic Beanstalk configuration files. It
-- doesn\'t validate your application\'s configuration files, like proxy
-- server or Docker configuration.
--
-- 'sourceBundle', 'createApplicationVersion_sourceBundle' - The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version.
--
-- The Amazon S3 bucket must be in the same region as the environment.
--
-- Specify a source bundle in S3 or a commit in an AWS CodeCommit
-- repository (with @SourceBuildInformation@), but not both. If neither
-- @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic
-- Beanstalk uses a sample application.
--
-- 'autoCreateApplication', 'createApplicationVersion_autoCreateApplication' - Set to @true@ to create an application with the specified name if it
-- doesn\'t already exist.
--
-- 'sourceBuildInformation', 'createApplicationVersion_sourceBuildInformation' - Specify a commit in an AWS CodeCommit Git repository to use as the
-- source code for the application version.
--
-- 'description', 'createApplicationVersion_description' - A description of this application version.
--
-- 'buildConfiguration', 'createApplicationVersion_buildConfiguration' - Settings for an AWS CodeBuild build.
--
-- 'tags', 'createApplicationVersion_tags' - Specifies the tags applied to the application version.
--
-- Elastic Beanstalk applies these tags only to the application version.
-- Environments that use the application version don\'t inherit the tags.
--
-- 'applicationName', 'createApplicationVersion_applicationName' - The name of the application. If no application is found with this name,
-- and @AutoCreateApplication@ is @false@, returns an
-- @InvalidParameterValue@ error.
--
-- 'versionLabel', 'createApplicationVersion_versionLabel' - A label identifying this version.
--
-- Constraint: Must be unique per application. If an application version
-- already exists with this label for the specified application, AWS
-- Elastic Beanstalk returns an @InvalidParameterValue@ error.
newCreateApplicationVersion ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'versionLabel'
  Prelude.Text ->
  CreateApplicationVersion
newCreateApplicationVersion
  pApplicationName_
  pVersionLabel_ =
    CreateApplicationVersion'
      { process =
          Prelude.Nothing,
        sourceBundle = Prelude.Nothing,
        autoCreateApplication = Prelude.Nothing,
        sourceBuildInformation = Prelude.Nothing,
        description = Prelude.Nothing,
        buildConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        applicationName = pApplicationName_,
        versionLabel = pVersionLabel_
      }

-- | Pre-processes and validates the environment manifest (@env.yaml@) and
-- configuration files (@*.config@ files in the @.ebextensions@ folder) in
-- the source bundle. Validating configuration files can identify issues
-- prior to deploying the application version to an environment.
--
-- You must turn processing on for application versions that you create
-- using AWS CodeBuild or AWS CodeCommit. For application versions built
-- from a source bundle in Amazon S3, processing is optional.
--
-- The @Process@ option validates Elastic Beanstalk configuration files. It
-- doesn\'t validate your application\'s configuration files, like proxy
-- server or Docker configuration.
createApplicationVersion_process :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe Prelude.Bool)
createApplicationVersion_process = Lens.lens (\CreateApplicationVersion' {process} -> process) (\s@CreateApplicationVersion' {} a -> s {process = a} :: CreateApplicationVersion)

-- | The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version.
--
-- The Amazon S3 bucket must be in the same region as the environment.
--
-- Specify a source bundle in S3 or a commit in an AWS CodeCommit
-- repository (with @SourceBuildInformation@), but not both. If neither
-- @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic
-- Beanstalk uses a sample application.
createApplicationVersion_sourceBundle :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe S3Location)
createApplicationVersion_sourceBundle = Lens.lens (\CreateApplicationVersion' {sourceBundle} -> sourceBundle) (\s@CreateApplicationVersion' {} a -> s {sourceBundle = a} :: CreateApplicationVersion)

-- | Set to @true@ to create an application with the specified name if it
-- doesn\'t already exist.
createApplicationVersion_autoCreateApplication :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe Prelude.Bool)
createApplicationVersion_autoCreateApplication = Lens.lens (\CreateApplicationVersion' {autoCreateApplication} -> autoCreateApplication) (\s@CreateApplicationVersion' {} a -> s {autoCreateApplication = a} :: CreateApplicationVersion)

-- | Specify a commit in an AWS CodeCommit Git repository to use as the
-- source code for the application version.
createApplicationVersion_sourceBuildInformation :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe SourceBuildInformation)
createApplicationVersion_sourceBuildInformation = Lens.lens (\CreateApplicationVersion' {sourceBuildInformation} -> sourceBuildInformation) (\s@CreateApplicationVersion' {} a -> s {sourceBuildInformation = a} :: CreateApplicationVersion)

-- | A description of this application version.
createApplicationVersion_description :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe Prelude.Text)
createApplicationVersion_description = Lens.lens (\CreateApplicationVersion' {description} -> description) (\s@CreateApplicationVersion' {} a -> s {description = a} :: CreateApplicationVersion)

-- | Settings for an AWS CodeBuild build.
createApplicationVersion_buildConfiguration :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe BuildConfiguration)
createApplicationVersion_buildConfiguration = Lens.lens (\CreateApplicationVersion' {buildConfiguration} -> buildConfiguration) (\s@CreateApplicationVersion' {} a -> s {buildConfiguration = a} :: CreateApplicationVersion)

-- | Specifies the tags applied to the application version.
--
-- Elastic Beanstalk applies these tags only to the application version.
-- Environments that use the application version don\'t inherit the tags.
createApplicationVersion_tags :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe [Tag])
createApplicationVersion_tags = Lens.lens (\CreateApplicationVersion' {tags} -> tags) (\s@CreateApplicationVersion' {} a -> s {tags = a} :: CreateApplicationVersion) Prelude.. Lens.mapping Lens.coerced

-- | The name of the application. If no application is found with this name,
-- and @AutoCreateApplication@ is @false@, returns an
-- @InvalidParameterValue@ error.
createApplicationVersion_applicationName :: Lens.Lens' CreateApplicationVersion Prelude.Text
createApplicationVersion_applicationName = Lens.lens (\CreateApplicationVersion' {applicationName} -> applicationName) (\s@CreateApplicationVersion' {} a -> s {applicationName = a} :: CreateApplicationVersion)

-- | A label identifying this version.
--
-- Constraint: Must be unique per application. If an application version
-- already exists with this label for the specified application, AWS
-- Elastic Beanstalk returns an @InvalidParameterValue@ error.
createApplicationVersion_versionLabel :: Lens.Lens' CreateApplicationVersion Prelude.Text
createApplicationVersion_versionLabel = Lens.lens (\CreateApplicationVersion' {versionLabel} -> versionLabel) (\s@CreateApplicationVersion' {} a -> s {versionLabel = a} :: CreateApplicationVersion)

instance Core.AWSRequest CreateApplicationVersion where
  type
    AWSResponse CreateApplicationVersion =
      ApplicationVersionDescriptionMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateApplicationVersionResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable CreateApplicationVersion where
  hashWithSalt salt' CreateApplicationVersion' {..} =
    salt' `Prelude.hashWithSalt` versionLabel
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` buildConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sourceBuildInformation
      `Prelude.hashWithSalt` autoCreateApplication
      `Prelude.hashWithSalt` sourceBundle
      `Prelude.hashWithSalt` process

instance Prelude.NFData CreateApplicationVersion where
  rnf CreateApplicationVersion' {..} =
    Prelude.rnf process
      `Prelude.seq` Prelude.rnf versionLabel
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf buildConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sourceBuildInformation
      `Prelude.seq` Prelude.rnf autoCreateApplication
      `Prelude.seq` Prelude.rnf sourceBundle

instance Core.ToHeaders CreateApplicationVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateApplicationVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateApplicationVersion where
  toQuery CreateApplicationVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateApplicationVersion" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Process" Core.=: process,
        "SourceBundle" Core.=: sourceBundle,
        "AutoCreateApplication"
          Core.=: autoCreateApplication,
        "SourceBuildInformation"
          Core.=: sourceBuildInformation,
        "Description" Core.=: description,
        "BuildConfiguration" Core.=: buildConfiguration,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "ApplicationName" Core.=: applicationName,
        "VersionLabel" Core.=: versionLabel
      ]
