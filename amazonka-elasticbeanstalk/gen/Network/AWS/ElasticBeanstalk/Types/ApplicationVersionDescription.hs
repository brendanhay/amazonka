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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
import Network.AWS.ElasticBeanstalk.Types.S3Location
import Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
import qualified Network.AWS.Lens as Lens

-- | Describes the properties of an application version.
--
-- /See:/ 'newApplicationVersionDescription' smart constructor.
data ApplicationVersionDescription = ApplicationVersionDescription'
  { -- | The processing status of the application version. Reflects the state of
    -- the application version during its creation. Many of the values are only
    -- applicable if you specified @True@ for the @Process@ parameter of the
    -- @CreateApplicationVersion@ action. The following list describes the
    -- possible values.
    --
    -- -   @Unprocessed@ – Application version wasn\'t pre-processed or
    --     validated. Elastic Beanstalk will validate configuration files
    --     during deployment of the application version to an environment.
    --
    -- -   @Processing@ – Elastic Beanstalk is currently processing the
    --     application version.
    --
    -- -   @Building@ – Application version is currently undergoing an AWS
    --     CodeBuild build.
    --
    -- -   @Processed@ – Elastic Beanstalk was successfully pre-processed and
    --     validated.
    --
    -- -   @Failed@ – Either the AWS CodeBuild build failed or configuration
    --     files didn\'t pass validation. This application version isn\'t
    --     usable.
    status :: Core.Maybe ApplicationVersionStatus,
    -- | The creation date of the application version.
    dateCreated :: Core.Maybe Core.ISO8601,
    -- | The storage location of the application version\'s source bundle in
    -- Amazon S3.
    sourceBundle :: Core.Maybe S3Location,
    -- | If the version\'s source code was retrieved from AWS CodeCommit, the
    -- location of the source code for the application version.
    sourceBuildInformation :: Core.Maybe SourceBuildInformation,
    -- | A unique identifier for the application version.
    versionLabel :: Core.Maybe Core.Text,
    -- | The last modified date of the application version.
    dateUpdated :: Core.Maybe Core.ISO8601,
    -- | The description of the application version.
    description :: Core.Maybe Core.Text,
    -- | Reference to the artifact from the AWS CodeBuild build.
    buildArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the application version.
    applicationVersionArn :: Core.Maybe Core.Text,
    -- | The name of the application to which the application version belongs.
    applicationName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationVersionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'applicationVersionDescription_status' - The processing status of the application version. Reflects the state of
-- the application version during its creation. Many of the values are only
-- applicable if you specified @True@ for the @Process@ parameter of the
-- @CreateApplicationVersion@ action. The following list describes the
-- possible values.
--
-- -   @Unprocessed@ – Application version wasn\'t pre-processed or
--     validated. Elastic Beanstalk will validate configuration files
--     during deployment of the application version to an environment.
--
-- -   @Processing@ – Elastic Beanstalk is currently processing the
--     application version.
--
-- -   @Building@ – Application version is currently undergoing an AWS
--     CodeBuild build.
--
-- -   @Processed@ – Elastic Beanstalk was successfully pre-processed and
--     validated.
--
-- -   @Failed@ – Either the AWS CodeBuild build failed or configuration
--     files didn\'t pass validation. This application version isn\'t
--     usable.
--
-- 'dateCreated', 'applicationVersionDescription_dateCreated' - The creation date of the application version.
--
-- 'sourceBundle', 'applicationVersionDescription_sourceBundle' - The storage location of the application version\'s source bundle in
-- Amazon S3.
--
-- 'sourceBuildInformation', 'applicationVersionDescription_sourceBuildInformation' - If the version\'s source code was retrieved from AWS CodeCommit, the
-- location of the source code for the application version.
--
-- 'versionLabel', 'applicationVersionDescription_versionLabel' - A unique identifier for the application version.
--
-- 'dateUpdated', 'applicationVersionDescription_dateUpdated' - The last modified date of the application version.
--
-- 'description', 'applicationVersionDescription_description' - The description of the application version.
--
-- 'buildArn', 'applicationVersionDescription_buildArn' - Reference to the artifact from the AWS CodeBuild build.
--
-- 'applicationVersionArn', 'applicationVersionDescription_applicationVersionArn' - The Amazon Resource Name (ARN) of the application version.
--
-- 'applicationName', 'applicationVersionDescription_applicationName' - The name of the application to which the application version belongs.
newApplicationVersionDescription ::
  ApplicationVersionDescription
newApplicationVersionDescription =
  ApplicationVersionDescription'
    { status =
        Core.Nothing,
      dateCreated = Core.Nothing,
      sourceBundle = Core.Nothing,
      sourceBuildInformation = Core.Nothing,
      versionLabel = Core.Nothing,
      dateUpdated = Core.Nothing,
      description = Core.Nothing,
      buildArn = Core.Nothing,
      applicationVersionArn = Core.Nothing,
      applicationName = Core.Nothing
    }

-- | The processing status of the application version. Reflects the state of
-- the application version during its creation. Many of the values are only
-- applicable if you specified @True@ for the @Process@ parameter of the
-- @CreateApplicationVersion@ action. The following list describes the
-- possible values.
--
-- -   @Unprocessed@ – Application version wasn\'t pre-processed or
--     validated. Elastic Beanstalk will validate configuration files
--     during deployment of the application version to an environment.
--
-- -   @Processing@ – Elastic Beanstalk is currently processing the
--     application version.
--
-- -   @Building@ – Application version is currently undergoing an AWS
--     CodeBuild build.
--
-- -   @Processed@ – Elastic Beanstalk was successfully pre-processed and
--     validated.
--
-- -   @Failed@ – Either the AWS CodeBuild build failed or configuration
--     files didn\'t pass validation. This application version isn\'t
--     usable.
applicationVersionDescription_status :: Lens.Lens' ApplicationVersionDescription (Core.Maybe ApplicationVersionStatus)
applicationVersionDescription_status = Lens.lens (\ApplicationVersionDescription' {status} -> status) (\s@ApplicationVersionDescription' {} a -> s {status = a} :: ApplicationVersionDescription)

-- | The creation date of the application version.
applicationVersionDescription_dateCreated :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Core.UTCTime)
applicationVersionDescription_dateCreated = Lens.lens (\ApplicationVersionDescription' {dateCreated} -> dateCreated) (\s@ApplicationVersionDescription' {} a -> s {dateCreated = a} :: ApplicationVersionDescription) Core.. Lens.mapping Core._Time

-- | The storage location of the application version\'s source bundle in
-- Amazon S3.
applicationVersionDescription_sourceBundle :: Lens.Lens' ApplicationVersionDescription (Core.Maybe S3Location)
applicationVersionDescription_sourceBundle = Lens.lens (\ApplicationVersionDescription' {sourceBundle} -> sourceBundle) (\s@ApplicationVersionDescription' {} a -> s {sourceBundle = a} :: ApplicationVersionDescription)

-- | If the version\'s source code was retrieved from AWS CodeCommit, the
-- location of the source code for the application version.
applicationVersionDescription_sourceBuildInformation :: Lens.Lens' ApplicationVersionDescription (Core.Maybe SourceBuildInformation)
applicationVersionDescription_sourceBuildInformation = Lens.lens (\ApplicationVersionDescription' {sourceBuildInformation} -> sourceBuildInformation) (\s@ApplicationVersionDescription' {} a -> s {sourceBuildInformation = a} :: ApplicationVersionDescription)

-- | A unique identifier for the application version.
applicationVersionDescription_versionLabel :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Core.Text)
applicationVersionDescription_versionLabel = Lens.lens (\ApplicationVersionDescription' {versionLabel} -> versionLabel) (\s@ApplicationVersionDescription' {} a -> s {versionLabel = a} :: ApplicationVersionDescription)

-- | The last modified date of the application version.
applicationVersionDescription_dateUpdated :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Core.UTCTime)
applicationVersionDescription_dateUpdated = Lens.lens (\ApplicationVersionDescription' {dateUpdated} -> dateUpdated) (\s@ApplicationVersionDescription' {} a -> s {dateUpdated = a} :: ApplicationVersionDescription) Core.. Lens.mapping Core._Time

-- | The description of the application version.
applicationVersionDescription_description :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Core.Text)
applicationVersionDescription_description = Lens.lens (\ApplicationVersionDescription' {description} -> description) (\s@ApplicationVersionDescription' {} a -> s {description = a} :: ApplicationVersionDescription)

-- | Reference to the artifact from the AWS CodeBuild build.
applicationVersionDescription_buildArn :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Core.Text)
applicationVersionDescription_buildArn = Lens.lens (\ApplicationVersionDescription' {buildArn} -> buildArn) (\s@ApplicationVersionDescription' {} a -> s {buildArn = a} :: ApplicationVersionDescription)

-- | The Amazon Resource Name (ARN) of the application version.
applicationVersionDescription_applicationVersionArn :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Core.Text)
applicationVersionDescription_applicationVersionArn = Lens.lens (\ApplicationVersionDescription' {applicationVersionArn} -> applicationVersionArn) (\s@ApplicationVersionDescription' {} a -> s {applicationVersionArn = a} :: ApplicationVersionDescription)

-- | The name of the application to which the application version belongs.
applicationVersionDescription_applicationName :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Core.Text)
applicationVersionDescription_applicationName = Lens.lens (\ApplicationVersionDescription' {applicationName} -> applicationName) (\s@ApplicationVersionDescription' {} a -> s {applicationName = a} :: ApplicationVersionDescription)

instance Core.FromXML ApplicationVersionDescription where
  parseXML x =
    ApplicationVersionDescription'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "DateCreated")
      Core.<*> (x Core..@? "SourceBundle")
      Core.<*> (x Core..@? "SourceBuildInformation")
      Core.<*> (x Core..@? "VersionLabel")
      Core.<*> (x Core..@? "DateUpdated")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "BuildArn")
      Core.<*> (x Core..@? "ApplicationVersionArn")
      Core.<*> (x Core..@? "ApplicationName")

instance Core.Hashable ApplicationVersionDescription

instance Core.NFData ApplicationVersionDescription
