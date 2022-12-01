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
-- Module      : Amazonka.CodeBuild.Types.Project
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.Project where

import Amazonka.CodeBuild.Types.LogsConfig
import Amazonka.CodeBuild.Types.ProjectArtifacts
import Amazonka.CodeBuild.Types.ProjectBadge
import Amazonka.CodeBuild.Types.ProjectBuildBatchConfig
import Amazonka.CodeBuild.Types.ProjectCache
import Amazonka.CodeBuild.Types.ProjectEnvironment
import Amazonka.CodeBuild.Types.ProjectFileSystemLocation
import Amazonka.CodeBuild.Types.ProjectSource
import Amazonka.CodeBuild.Types.ProjectSourceVersion
import Amazonka.CodeBuild.Types.ProjectVisibilityType
import Amazonka.CodeBuild.Types.Tag
import Amazonka.CodeBuild.Types.VpcConfig
import Amazonka.CodeBuild.Types.Webhook
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a build project.
--
-- /See:/ 'newProject' smart constructor.
data Project = Project'
  { -- | A list of tag key and value pairs associated with this build project.
    --
    -- These tags are available for use by Amazon Web Services services that
    -- support CodeBuild build project tags.
    tags :: Prelude.Maybe [Tag],
    -- | Information about a webhook that connects repository events to a build
    -- project in CodeBuild.
    webhook :: Prelude.Maybe Webhook,
    -- | The name of the build project.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the build environment for this build project.
    environment :: Prelude.Maybe ProjectEnvironment,
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Prelude.Maybe [ProjectSource],
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Prelude.Maybe [ProjectFileSystemLocation],
    -- | How long, in minutes, from 5 to 480 (8 hours), for CodeBuild to wait
    -- before timing out any related build that did not get marked as
    -- completed. The default is 60 minutes.
    timeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Information about the VPC configuration that CodeBuild accesses.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifacts :: Prelude.Maybe [ProjectArtifacts],
    -- | When the build project was created, expressed in Unix time format.
    created :: Prelude.Maybe Core.POSIX,
    -- | A version of the build input to be built for this project. If not
    -- specified, the latest version is used. If specified, it must be one of:
    --
    -- -   For CodeCommit: the commit ID, branch, or Git tag to use.
    --
    -- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
    --     that corresponds to the version of the source code you want to
    --     build. If a pull request ID is specified, it must use the format
    --     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
    --     specified, the branch\'s HEAD commit ID is used. If not specified,
    --     the default branch\'s HEAD commit ID is used.
    --
    -- -   For Bitbucket: the commit ID, branch name, or tag name that
    --     corresponds to the version of the source code you want to build. If
    --     a branch name is specified, the branch\'s HEAD commit ID is used. If
    --     not specified, the default branch\'s HEAD commit ID is used.
    --
    -- -   For Amazon S3: the version ID of the object that represents the
    --     build input ZIP file to use.
    --
    -- If @sourceVersion@ is specified at the build level, then that version
    -- takes precedence over this @sourceVersion@ (at the project level).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
    -- in the /CodeBuild User Guide/.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the build project.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of concurrent builds that are allowed for this
    -- project.
    --
    -- New builds are only started if the current number of builds is less than
    -- or equal to this limit. If the current build count meets this limit, new
    -- builds are throttled and are not run.
    concurrentBuildLimit :: Prelude.Maybe Prelude.Int,
    projectVisibility :: Prelude.Maybe ProjectVisibilityType,
    -- | A description that makes the build project easy to identify.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the cache for the build project.
    cache :: Prelude.Maybe ProjectCache,
    -- | The ARN of the IAM role that enables CodeBuild to interact with
    -- dependent Amazon Web Services services on behalf of the Amazon Web
    -- Services account.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | Information about the build badge for the build project.
    badge :: Prelude.Maybe ProjectBadge,
    -- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
    -- is specified at the build level, then they take over these
    -- @secondarySourceVersions@ (at the project level).
    secondarySourceVersions :: Prelude.Maybe [ProjectSourceVersion],
    -- | Information about the build input source code for this build project.
    source :: Prelude.Maybe ProjectSource,
    -- | Information about logs for the build project. A project can create logs
    -- in CloudWatch Logs, an S3 bucket, or both.
    logsConfig :: Prelude.Maybe LogsConfig,
    -- | When the build project\'s settings were last modified, expressed in Unix
    -- time format.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | Contains the project identifier used with the public build APIs.
    publicProjectAlias :: Prelude.Maybe Prelude.Text,
    -- | A ProjectBuildBatchConfig object that defines the batch build options
    -- for the project.
    buildBatchConfig :: Prelude.Maybe ProjectBuildBatchConfig,
    -- | The Key Management Service customer master key (CMK) to be used for
    -- encrypting the build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    -- If you don\'t specify a value, CodeBuild uses the managed CMK for Amazon
    -- Simple Storage Service (Amazon S3).
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | Information about the build output artifacts for the build project.
    artifacts :: Prelude.Maybe ProjectArtifacts,
    -- | The ARN of the IAM role that enables CodeBuild to access the CloudWatch
    -- Logs and Amazon S3 artifacts for the project\'s builds.
    resourceAccessRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Project' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'project_tags' - A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild build project tags.
--
-- 'webhook', 'project_webhook' - Information about a webhook that connects repository events to a build
-- project in CodeBuild.
--
-- 'name', 'project_name' - The name of the build project.
--
-- 'environment', 'project_environment' - Information about the build environment for this build project.
--
-- 'secondarySources', 'project_secondarySources' - An array of @ProjectSource@ objects.
--
-- 'fileSystemLocations', 'project_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'timeoutInMinutes', 'project_timeoutInMinutes' - How long, in minutes, from 5 to 480 (8 hours), for CodeBuild to wait
-- before timing out any related build that did not get marked as
-- completed. The default is 60 minutes.
--
-- 'queuedTimeoutInMinutes', 'project_queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times
-- out.
--
-- 'vpcConfig', 'project_vpcConfig' - Information about the VPC configuration that CodeBuild accesses.
--
-- 'secondaryArtifacts', 'project_secondaryArtifacts' - An array of @ProjectArtifacts@ objects.
--
-- 'created', 'project_created' - When the build project was created, expressed in Unix time format.
--
-- 'sourceVersion', 'project_sourceVersion' - A version of the build input to be built for this project. If not
-- specified, the latest version is used. If specified, it must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- -   For Bitbucket: the commit ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a branch name is specified, the branch\'s HEAD commit ID is used. If
--     not specified, the default branch\'s HEAD commit ID is used.
--
-- -   For Amazon S3: the version ID of the object that represents the
--     build input ZIP file to use.
--
-- If @sourceVersion@ is specified at the build level, then that version
-- takes precedence over this @sourceVersion@ (at the project level).
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
--
-- 'arn', 'project_arn' - The Amazon Resource Name (ARN) of the build project.
--
-- 'concurrentBuildLimit', 'project_concurrentBuildLimit' - The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
--
-- 'projectVisibility', 'project_projectVisibility' - Undocumented member.
--
-- 'description', 'project_description' - A description that makes the build project easy to identify.
--
-- 'cache', 'project_cache' - Information about the cache for the build project.
--
-- 'serviceRole', 'project_serviceRole' - The ARN of the IAM role that enables CodeBuild to interact with
-- dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
--
-- 'badge', 'project_badge' - Information about the build badge for the build project.
--
-- 'secondarySourceVersions', 'project_secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take over these
-- @secondarySourceVersions@ (at the project level).
--
-- 'source', 'project_source' - Information about the build input source code for this build project.
--
-- 'logsConfig', 'project_logsConfig' - Information about logs for the build project. A project can create logs
-- in CloudWatch Logs, an S3 bucket, or both.
--
-- 'lastModified', 'project_lastModified' - When the build project\'s settings were last modified, expressed in Unix
-- time format.
--
-- 'publicProjectAlias', 'project_publicProjectAlias' - Contains the project identifier used with the public build APIs.
--
-- 'buildBatchConfig', 'project_buildBatchConfig' - A ProjectBuildBatchConfig object that defines the batch build options
-- for the project.
--
-- 'encryptionKey', 'project_encryptionKey' - The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
-- If you don\'t specify a value, CodeBuild uses the managed CMK for Amazon
-- Simple Storage Service (Amazon S3).
--
-- 'artifacts', 'project_artifacts' - Information about the build output artifacts for the build project.
--
-- 'resourceAccessRole', 'project_resourceAccessRole' - The ARN of the IAM role that enables CodeBuild to access the CloudWatch
-- Logs and Amazon S3 artifacts for the project\'s builds.
newProject ::
  Project
newProject =
  Project'
    { tags = Prelude.Nothing,
      webhook = Prelude.Nothing,
      name = Prelude.Nothing,
      environment = Prelude.Nothing,
      secondarySources = Prelude.Nothing,
      fileSystemLocations = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      queuedTimeoutInMinutes = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      secondaryArtifacts = Prelude.Nothing,
      created = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      concurrentBuildLimit = Prelude.Nothing,
      projectVisibility = Prelude.Nothing,
      description = Prelude.Nothing,
      cache = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      badge = Prelude.Nothing,
      secondarySourceVersions = Prelude.Nothing,
      source = Prelude.Nothing,
      logsConfig = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      publicProjectAlias = Prelude.Nothing,
      buildBatchConfig = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      artifacts = Prelude.Nothing,
      resourceAccessRole = Prelude.Nothing
    }

-- | A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild build project tags.
project_tags :: Lens.Lens' Project (Prelude.Maybe [Tag])
project_tags = Lens.lens (\Project' {tags} -> tags) (\s@Project' {} a -> s {tags = a} :: Project) Prelude.. Lens.mapping Lens.coerced

-- | Information about a webhook that connects repository events to a build
-- project in CodeBuild.
project_webhook :: Lens.Lens' Project (Prelude.Maybe Webhook)
project_webhook = Lens.lens (\Project' {webhook} -> webhook) (\s@Project' {} a -> s {webhook = a} :: Project)

-- | The name of the build project.
project_name :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_name = Lens.lens (\Project' {name} -> name) (\s@Project' {} a -> s {name = a} :: Project)

-- | Information about the build environment for this build project.
project_environment :: Lens.Lens' Project (Prelude.Maybe ProjectEnvironment)
project_environment = Lens.lens (\Project' {environment} -> environment) (\s@Project' {} a -> s {environment = a} :: Project)

-- | An array of @ProjectSource@ objects.
project_secondarySources :: Lens.Lens' Project (Prelude.Maybe [ProjectSource])
project_secondarySources = Lens.lens (\Project' {secondarySources} -> secondarySources) (\s@Project' {} a -> s {secondarySources = a} :: Project) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
project_fileSystemLocations :: Lens.Lens' Project (Prelude.Maybe [ProjectFileSystemLocation])
project_fileSystemLocations = Lens.lens (\Project' {fileSystemLocations} -> fileSystemLocations) (\s@Project' {} a -> s {fileSystemLocations = a} :: Project) Prelude.. Lens.mapping Lens.coerced

-- | How long, in minutes, from 5 to 480 (8 hours), for CodeBuild to wait
-- before timing out any related build that did not get marked as
-- completed. The default is 60 minutes.
project_timeoutInMinutes :: Lens.Lens' Project (Prelude.Maybe Prelude.Natural)
project_timeoutInMinutes = Lens.lens (\Project' {timeoutInMinutes} -> timeoutInMinutes) (\s@Project' {} a -> s {timeoutInMinutes = a} :: Project)

-- | The number of minutes a build is allowed to be queued before it times
-- out.
project_queuedTimeoutInMinutes :: Lens.Lens' Project (Prelude.Maybe Prelude.Natural)
project_queuedTimeoutInMinutes = Lens.lens (\Project' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@Project' {} a -> s {queuedTimeoutInMinutes = a} :: Project)

-- | Information about the VPC configuration that CodeBuild accesses.
project_vpcConfig :: Lens.Lens' Project (Prelude.Maybe VpcConfig)
project_vpcConfig = Lens.lens (\Project' {vpcConfig} -> vpcConfig) (\s@Project' {} a -> s {vpcConfig = a} :: Project)

-- | An array of @ProjectArtifacts@ objects.
project_secondaryArtifacts :: Lens.Lens' Project (Prelude.Maybe [ProjectArtifacts])
project_secondaryArtifacts = Lens.lens (\Project' {secondaryArtifacts} -> secondaryArtifacts) (\s@Project' {} a -> s {secondaryArtifacts = a} :: Project) Prelude.. Lens.mapping Lens.coerced

-- | When the build project was created, expressed in Unix time format.
project_created :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_created = Lens.lens (\Project' {created} -> created) (\s@Project' {} a -> s {created = a} :: Project) Prelude.. Lens.mapping Core._Time

-- | A version of the build input to be built for this project. If not
-- specified, the latest version is used. If specified, it must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- -   For Bitbucket: the commit ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a branch name is specified, the branch\'s HEAD commit ID is used. If
--     not specified, the default branch\'s HEAD commit ID is used.
--
-- -   For Amazon S3: the version ID of the object that represents the
--     build input ZIP file to use.
--
-- If @sourceVersion@ is specified at the build level, then that version
-- takes precedence over this @sourceVersion@ (at the project level).
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
project_sourceVersion :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_sourceVersion = Lens.lens (\Project' {sourceVersion} -> sourceVersion) (\s@Project' {} a -> s {sourceVersion = a} :: Project)

-- | The Amazon Resource Name (ARN) of the build project.
project_arn :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_arn = Lens.lens (\Project' {arn} -> arn) (\s@Project' {} a -> s {arn = a} :: Project)

-- | The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
project_concurrentBuildLimit :: Lens.Lens' Project (Prelude.Maybe Prelude.Int)
project_concurrentBuildLimit = Lens.lens (\Project' {concurrentBuildLimit} -> concurrentBuildLimit) (\s@Project' {} a -> s {concurrentBuildLimit = a} :: Project)

-- | Undocumented member.
project_projectVisibility :: Lens.Lens' Project (Prelude.Maybe ProjectVisibilityType)
project_projectVisibility = Lens.lens (\Project' {projectVisibility} -> projectVisibility) (\s@Project' {} a -> s {projectVisibility = a} :: Project)

-- | A description that makes the build project easy to identify.
project_description :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_description = Lens.lens (\Project' {description} -> description) (\s@Project' {} a -> s {description = a} :: Project)

-- | Information about the cache for the build project.
project_cache :: Lens.Lens' Project (Prelude.Maybe ProjectCache)
project_cache = Lens.lens (\Project' {cache} -> cache) (\s@Project' {} a -> s {cache = a} :: Project)

-- | The ARN of the IAM role that enables CodeBuild to interact with
-- dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
project_serviceRole :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_serviceRole = Lens.lens (\Project' {serviceRole} -> serviceRole) (\s@Project' {} a -> s {serviceRole = a} :: Project)

-- | Information about the build badge for the build project.
project_badge :: Lens.Lens' Project (Prelude.Maybe ProjectBadge)
project_badge = Lens.lens (\Project' {badge} -> badge) (\s@Project' {} a -> s {badge = a} :: Project)

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take over these
-- @secondarySourceVersions@ (at the project level).
project_secondarySourceVersions :: Lens.Lens' Project (Prelude.Maybe [ProjectSourceVersion])
project_secondarySourceVersions = Lens.lens (\Project' {secondarySourceVersions} -> secondarySourceVersions) (\s@Project' {} a -> s {secondarySourceVersions = a} :: Project) Prelude.. Lens.mapping Lens.coerced

-- | Information about the build input source code for this build project.
project_source :: Lens.Lens' Project (Prelude.Maybe ProjectSource)
project_source = Lens.lens (\Project' {source} -> source) (\s@Project' {} a -> s {source = a} :: Project)

-- | Information about logs for the build project. A project can create logs
-- in CloudWatch Logs, an S3 bucket, or both.
project_logsConfig :: Lens.Lens' Project (Prelude.Maybe LogsConfig)
project_logsConfig = Lens.lens (\Project' {logsConfig} -> logsConfig) (\s@Project' {} a -> s {logsConfig = a} :: Project)

-- | When the build project\'s settings were last modified, expressed in Unix
-- time format.
project_lastModified :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_lastModified = Lens.lens (\Project' {lastModified} -> lastModified) (\s@Project' {} a -> s {lastModified = a} :: Project) Prelude.. Lens.mapping Core._Time

-- | Contains the project identifier used with the public build APIs.
project_publicProjectAlias :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_publicProjectAlias = Lens.lens (\Project' {publicProjectAlias} -> publicProjectAlias) (\s@Project' {} a -> s {publicProjectAlias = a} :: Project)

-- | A ProjectBuildBatchConfig object that defines the batch build options
-- for the project.
project_buildBatchConfig :: Lens.Lens' Project (Prelude.Maybe ProjectBuildBatchConfig)
project_buildBatchConfig = Lens.lens (\Project' {buildBatchConfig} -> buildBatchConfig) (\s@Project' {} a -> s {buildBatchConfig = a} :: Project)

-- | The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
-- If you don\'t specify a value, CodeBuild uses the managed CMK for Amazon
-- Simple Storage Service (Amazon S3).
project_encryptionKey :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_encryptionKey = Lens.lens (\Project' {encryptionKey} -> encryptionKey) (\s@Project' {} a -> s {encryptionKey = a} :: Project)

-- | Information about the build output artifacts for the build project.
project_artifacts :: Lens.Lens' Project (Prelude.Maybe ProjectArtifacts)
project_artifacts = Lens.lens (\Project' {artifacts} -> artifacts) (\s@Project' {} a -> s {artifacts = a} :: Project)

-- | The ARN of the IAM role that enables CodeBuild to access the CloudWatch
-- Logs and Amazon S3 artifacts for the project\'s builds.
project_resourceAccessRole :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_resourceAccessRole = Lens.lens (\Project' {resourceAccessRole} -> resourceAccessRole) (\s@Project' {} a -> s {resourceAccessRole = a} :: Project)

instance Core.FromJSON Project where
  parseJSON =
    Core.withObject
      "Project"
      ( \x ->
          Project'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "webhook")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "environment")
            Prelude.<*> ( x Core..:? "secondarySources"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "fileSystemLocations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "timeoutInMinutes")
            Prelude.<*> (x Core..:? "queuedTimeoutInMinutes")
            Prelude.<*> (x Core..:? "vpcConfig")
            Prelude.<*> ( x Core..:? "secondaryArtifacts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "created")
            Prelude.<*> (x Core..:? "sourceVersion")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "concurrentBuildLimit")
            Prelude.<*> (x Core..:? "projectVisibility")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "cache")
            Prelude.<*> (x Core..:? "serviceRole")
            Prelude.<*> (x Core..:? "badge")
            Prelude.<*> ( x Core..:? "secondarySourceVersions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "source")
            Prelude.<*> (x Core..:? "logsConfig")
            Prelude.<*> (x Core..:? "lastModified")
            Prelude.<*> (x Core..:? "publicProjectAlias")
            Prelude.<*> (x Core..:? "buildBatchConfig")
            Prelude.<*> (x Core..:? "encryptionKey")
            Prelude.<*> (x Core..:? "artifacts")
            Prelude.<*> (x Core..:? "resourceAccessRole")
      )

instance Prelude.Hashable Project where
  hashWithSalt _salt Project' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` webhook
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` secondarySources
      `Prelude.hashWithSalt` fileSystemLocations
      `Prelude.hashWithSalt` timeoutInMinutes
      `Prelude.hashWithSalt` queuedTimeoutInMinutes
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` secondaryArtifacts
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` concurrentBuildLimit
      `Prelude.hashWithSalt` projectVisibility
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` cache
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` badge
      `Prelude.hashWithSalt` secondarySourceVersions
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` logsConfig
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` publicProjectAlias
      `Prelude.hashWithSalt` buildBatchConfig
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` artifacts
      `Prelude.hashWithSalt` resourceAccessRole

instance Prelude.NFData Project where
  rnf Project' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf webhook
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf secondarySources
      `Prelude.seq` Prelude.rnf fileSystemLocations
      `Prelude.seq` Prelude.rnf timeoutInMinutes
      `Prelude.seq` Prelude.rnf queuedTimeoutInMinutes
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf secondaryArtifacts
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf concurrentBuildLimit
      `Prelude.seq` Prelude.rnf projectVisibility
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf cache
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf badge
      `Prelude.seq` Prelude.rnf
        secondarySourceVersions
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf logsConfig
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf
        publicProjectAlias
      `Prelude.seq` Prelude.rnf
        buildBatchConfig
      `Prelude.seq` Prelude.rnf
        encryptionKey
      `Prelude.seq` Prelude.rnf
        artifacts
      `Prelude.seq` Prelude.rnf
        resourceAccessRole
