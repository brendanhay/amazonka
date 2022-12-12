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
-- Module      : Amazonka.ElasticBeanstalk.Types.ApplicationVersionDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ApplicationVersionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.ApplicationVersionStatus
import Amazonka.ElasticBeanstalk.Types.S3Location
import Amazonka.ElasticBeanstalk.Types.SourceBuildInformation
import qualified Amazonka.Prelude as Prelude

-- | Describes the properties of an application version.
--
-- /See:/ 'newApplicationVersionDescription' smart constructor.
data ApplicationVersionDescription = ApplicationVersionDescription'
  { -- | The name of the application to which the application version belongs.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application version.
    applicationVersionArn :: Prelude.Maybe Prelude.Text,
    -- | Reference to the artifact from the AWS CodeBuild build.
    buildArn :: Prelude.Maybe Prelude.Text,
    -- | The creation date of the application version.
    dateCreated :: Prelude.Maybe Data.ISO8601,
    -- | The last modified date of the application version.
    dateUpdated :: Prelude.Maybe Data.ISO8601,
    -- | The description of the application version.
    description :: Prelude.Maybe Prelude.Text,
    -- | If the version\'s source code was retrieved from AWS CodeCommit, the
    -- location of the source code for the application version.
    sourceBuildInformation :: Prelude.Maybe SourceBuildInformation,
    -- | The storage location of the application version\'s source bundle in
    -- Amazon S3.
    sourceBundle :: Prelude.Maybe S3Location,
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
    status :: Prelude.Maybe ApplicationVersionStatus,
    -- | A unique identifier for the application version.
    versionLabel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationVersionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'applicationVersionDescription_applicationName' - The name of the application to which the application version belongs.
--
-- 'applicationVersionArn', 'applicationVersionDescription_applicationVersionArn' - The Amazon Resource Name (ARN) of the application version.
--
-- 'buildArn', 'applicationVersionDescription_buildArn' - Reference to the artifact from the AWS CodeBuild build.
--
-- 'dateCreated', 'applicationVersionDescription_dateCreated' - The creation date of the application version.
--
-- 'dateUpdated', 'applicationVersionDescription_dateUpdated' - The last modified date of the application version.
--
-- 'description', 'applicationVersionDescription_description' - The description of the application version.
--
-- 'sourceBuildInformation', 'applicationVersionDescription_sourceBuildInformation' - If the version\'s source code was retrieved from AWS CodeCommit, the
-- location of the source code for the application version.
--
-- 'sourceBundle', 'applicationVersionDescription_sourceBundle' - The storage location of the application version\'s source bundle in
-- Amazon S3.
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
-- 'versionLabel', 'applicationVersionDescription_versionLabel' - A unique identifier for the application version.
newApplicationVersionDescription ::
  ApplicationVersionDescription
newApplicationVersionDescription =
  ApplicationVersionDescription'
    { applicationName =
        Prelude.Nothing,
      applicationVersionArn = Prelude.Nothing,
      buildArn = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      description = Prelude.Nothing,
      sourceBuildInformation = Prelude.Nothing,
      sourceBundle = Prelude.Nothing,
      status = Prelude.Nothing,
      versionLabel = Prelude.Nothing
    }

-- | The name of the application to which the application version belongs.
applicationVersionDescription_applicationName :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe Prelude.Text)
applicationVersionDescription_applicationName = Lens.lens (\ApplicationVersionDescription' {applicationName} -> applicationName) (\s@ApplicationVersionDescription' {} a -> s {applicationName = a} :: ApplicationVersionDescription)

-- | The Amazon Resource Name (ARN) of the application version.
applicationVersionDescription_applicationVersionArn :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe Prelude.Text)
applicationVersionDescription_applicationVersionArn = Lens.lens (\ApplicationVersionDescription' {applicationVersionArn} -> applicationVersionArn) (\s@ApplicationVersionDescription' {} a -> s {applicationVersionArn = a} :: ApplicationVersionDescription)

-- | Reference to the artifact from the AWS CodeBuild build.
applicationVersionDescription_buildArn :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe Prelude.Text)
applicationVersionDescription_buildArn = Lens.lens (\ApplicationVersionDescription' {buildArn} -> buildArn) (\s@ApplicationVersionDescription' {} a -> s {buildArn = a} :: ApplicationVersionDescription)

-- | The creation date of the application version.
applicationVersionDescription_dateCreated :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe Prelude.UTCTime)
applicationVersionDescription_dateCreated = Lens.lens (\ApplicationVersionDescription' {dateCreated} -> dateCreated) (\s@ApplicationVersionDescription' {} a -> s {dateCreated = a} :: ApplicationVersionDescription) Prelude.. Lens.mapping Data._Time

-- | The last modified date of the application version.
applicationVersionDescription_dateUpdated :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe Prelude.UTCTime)
applicationVersionDescription_dateUpdated = Lens.lens (\ApplicationVersionDescription' {dateUpdated} -> dateUpdated) (\s@ApplicationVersionDescription' {} a -> s {dateUpdated = a} :: ApplicationVersionDescription) Prelude.. Lens.mapping Data._Time

-- | The description of the application version.
applicationVersionDescription_description :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe Prelude.Text)
applicationVersionDescription_description = Lens.lens (\ApplicationVersionDescription' {description} -> description) (\s@ApplicationVersionDescription' {} a -> s {description = a} :: ApplicationVersionDescription)

-- | If the version\'s source code was retrieved from AWS CodeCommit, the
-- location of the source code for the application version.
applicationVersionDescription_sourceBuildInformation :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe SourceBuildInformation)
applicationVersionDescription_sourceBuildInformation = Lens.lens (\ApplicationVersionDescription' {sourceBuildInformation} -> sourceBuildInformation) (\s@ApplicationVersionDescription' {} a -> s {sourceBuildInformation = a} :: ApplicationVersionDescription)

-- | The storage location of the application version\'s source bundle in
-- Amazon S3.
applicationVersionDescription_sourceBundle :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe S3Location)
applicationVersionDescription_sourceBundle = Lens.lens (\ApplicationVersionDescription' {sourceBundle} -> sourceBundle) (\s@ApplicationVersionDescription' {} a -> s {sourceBundle = a} :: ApplicationVersionDescription)

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
applicationVersionDescription_status :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe ApplicationVersionStatus)
applicationVersionDescription_status = Lens.lens (\ApplicationVersionDescription' {status} -> status) (\s@ApplicationVersionDescription' {} a -> s {status = a} :: ApplicationVersionDescription)

-- | A unique identifier for the application version.
applicationVersionDescription_versionLabel :: Lens.Lens' ApplicationVersionDescription (Prelude.Maybe Prelude.Text)
applicationVersionDescription_versionLabel = Lens.lens (\ApplicationVersionDescription' {versionLabel} -> versionLabel) (\s@ApplicationVersionDescription' {} a -> s {versionLabel = a} :: ApplicationVersionDescription)

instance Data.FromXML ApplicationVersionDescription where
  parseXML x =
    ApplicationVersionDescription'
      Prelude.<$> (x Data..@? "ApplicationName")
      Prelude.<*> (x Data..@? "ApplicationVersionArn")
      Prelude.<*> (x Data..@? "BuildArn")
      Prelude.<*> (x Data..@? "DateCreated")
      Prelude.<*> (x Data..@? "DateUpdated")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "SourceBuildInformation")
      Prelude.<*> (x Data..@? "SourceBundle")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "VersionLabel")

instance
  Prelude.Hashable
    ApplicationVersionDescription
  where
  hashWithSalt _salt ApplicationVersionDescription' {..} =
    _salt `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` applicationVersionArn
      `Prelude.hashWithSalt` buildArn
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateUpdated
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sourceBuildInformation
      `Prelude.hashWithSalt` sourceBundle
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` versionLabel

instance Prelude.NFData ApplicationVersionDescription where
  rnf ApplicationVersionDescription' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf applicationVersionArn
      `Prelude.seq` Prelude.rnf buildArn
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sourceBuildInformation
      `Prelude.seq` Prelude.rnf sourceBundle
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf versionLabel
