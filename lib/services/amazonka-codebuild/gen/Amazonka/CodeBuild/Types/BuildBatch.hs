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
-- Module      : Amazonka.CodeBuild.Types.BuildBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BuildBatch where

import Amazonka.CodeBuild.Types.BuildArtifacts
import Amazonka.CodeBuild.Types.BuildBatchPhase
import Amazonka.CodeBuild.Types.BuildGroup
import Amazonka.CodeBuild.Types.LogsConfig
import Amazonka.CodeBuild.Types.ProjectBuildBatchConfig
import Amazonka.CodeBuild.Types.ProjectCache
import Amazonka.CodeBuild.Types.ProjectEnvironment
import Amazonka.CodeBuild.Types.ProjectFileSystemLocation
import Amazonka.CodeBuild.Types.ProjectSource
import Amazonka.CodeBuild.Types.ProjectSourceVersion
import Amazonka.CodeBuild.Types.StatusType
import Amazonka.CodeBuild.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a batch build.
--
-- /See:/ 'newBuildBatch' smart constructor.
data BuildBatch = BuildBatch'
  { -- | The ARN of the batch build.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A @BuildArtifacts@ object the defines the build artifacts for this batch
    -- build.
    artifacts :: Prelude.Maybe BuildArtifacts,
    buildBatchConfig :: Prelude.Maybe ProjectBuildBatchConfig,
    -- | The number of the batch build. For each project, the @buildBatchNumber@
    -- of its first batch build is @1@. The @buildBatchNumber@ of each
    -- subsequent batch build is incremented by @1@. If a batch build is
    -- deleted, the @buildBatchNumber@ of other batch builds does not change.
    buildBatchNumber :: Prelude.Maybe Prelude.Integer,
    -- | The status of the batch build.
    buildBatchStatus :: Prelude.Maybe StatusType,
    -- | An array of @BuildGroup@ objects that define the build groups for the
    -- batch build.
    buildGroups :: Prelude.Maybe [BuildGroup],
    -- | Specifies the maximum amount of time, in minutes, that the build in a
    -- batch must be completed in.
    buildTimeoutInMinutes :: Prelude.Maybe Prelude.Int,
    cache :: Prelude.Maybe ProjectCache,
    -- | Indicates if the batch build is complete.
    complete :: Prelude.Maybe Prelude.Bool,
    -- | The current phase of the batch build.
    currentPhase :: Prelude.Maybe Prelude.Text,
    -- | Specifies if session debugging is enabled for this batch build. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
    -- Batch session debugging is not supported for matrix batch builds.
    debugSessionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Key Management Service customer master key (CMK) to be used for
    -- encrypting the batch build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the batch build ended.
    endTime :: Prelude.Maybe Data.POSIX,
    environment :: Prelude.Maybe ProjectEnvironment,
    -- | An array of @ProjectFileSystemLocation@ objects for the batch build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Prelude.Maybe [ProjectFileSystemLocation],
    -- | The identifier of the batch build.
    id :: Prelude.Maybe Prelude.Text,
    -- | The entity that started the batch build. Valid values include:
    --
    -- -   If CodePipeline started the build, the pipeline\'s name (for
    --     example, @codepipeline\/my-demo-pipeline@).
    --
    -- -   If an IAM user started the build, the user\'s name.
    --
    -- -   If the Jenkins plugin for CodeBuild started the build, the string
    --     @CodeBuild-Jenkins-Plugin@.
    initiator :: Prelude.Maybe Prelude.Text,
    logConfig :: Prelude.Maybe LogsConfig,
    -- | An array of @BuildBatchPhase@ objects the specify the phases of the
    -- batch build.
    phases :: Prelude.Maybe [BuildBatchPhase],
    -- | The name of the batch build project.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the amount of time, in minutes, that the batch build is
    -- allowed to be queued before it times out.
    queuedTimeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the resolved version of this batch build\'s source
    -- code.
    --
    -- -   For CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit
    --     ID.
    --
    -- -   For CodePipeline, the source revision provided by CodePipeline.
    --
    -- -   For Amazon S3, this does not apply.
    resolvedSourceVersion :: Prelude.Maybe Prelude.Text,
    -- | An array of @BuildArtifacts@ objects the define the build artifacts for
    -- this batch build.
    secondaryArtifacts :: Prelude.Maybe [BuildArtifacts],
    -- | An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
    -- must be one of:
    --
    -- -   For CodeCommit: the commit ID, branch, or Git tag to use.
    --
    -- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
    --     that corresponds to the version of the source code you want to
    --     build. If a pull request ID is specified, it must use the format
    --     @pr\/pull-request-ID@ (for example, @pr\/25@). If a branch name is
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
    secondarySourceVersions :: Prelude.Maybe [ProjectSourceVersion],
    -- | An array of @ProjectSource@ objects that define the sources for the
    -- batch build.
    secondarySources :: Prelude.Maybe [ProjectSource],
    -- | The name of a service role used for builds in the batch.
    serviceRole :: Prelude.Maybe Prelude.Text,
    source :: Prelude.Maybe ProjectSource,
    -- | The identifier of the version of the source code to be built.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the batch build started.
    startTime :: Prelude.Maybe Data.POSIX,
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuildBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'buildBatch_arn' - The ARN of the batch build.
--
-- 'artifacts', 'buildBatch_artifacts' - A @BuildArtifacts@ object the defines the build artifacts for this batch
-- build.
--
-- 'buildBatchConfig', 'buildBatch_buildBatchConfig' - Undocumented member.
--
-- 'buildBatchNumber', 'buildBatch_buildBatchNumber' - The number of the batch build. For each project, the @buildBatchNumber@
-- of its first batch build is @1@. The @buildBatchNumber@ of each
-- subsequent batch build is incremented by @1@. If a batch build is
-- deleted, the @buildBatchNumber@ of other batch builds does not change.
--
-- 'buildBatchStatus', 'buildBatch_buildBatchStatus' - The status of the batch build.
--
-- 'buildGroups', 'buildBatch_buildGroups' - An array of @BuildGroup@ objects that define the build groups for the
-- batch build.
--
-- 'buildTimeoutInMinutes', 'buildBatch_buildTimeoutInMinutes' - Specifies the maximum amount of time, in minutes, that the build in a
-- batch must be completed in.
--
-- 'cache', 'buildBatch_cache' - Undocumented member.
--
-- 'complete', 'buildBatch_complete' - Indicates if the batch build is complete.
--
-- 'currentPhase', 'buildBatch_currentPhase' - The current phase of the batch build.
--
-- 'debugSessionEnabled', 'buildBatch_debugSessionEnabled' - Specifies if session debugging is enabled for this batch build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
-- Batch session debugging is not supported for matrix batch builds.
--
-- 'encryptionKey', 'buildBatch_encryptionKey' - The Key Management Service customer master key (CMK) to be used for
-- encrypting the batch build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'endTime', 'buildBatch_endTime' - The date and time that the batch build ended.
--
-- 'environment', 'buildBatch_environment' - Undocumented member.
--
-- 'fileSystemLocations', 'buildBatch_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for the batch build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'id', 'buildBatch_id' - The identifier of the batch build.
--
-- 'initiator', 'buildBatch_initiator' - The entity that started the batch build. Valid values include:
--
-- -   If CodePipeline started the build, the pipeline\'s name (for
--     example, @codepipeline\/my-demo-pipeline@).
--
-- -   If an IAM user started the build, the user\'s name.
--
-- -   If the Jenkins plugin for CodeBuild started the build, the string
--     @CodeBuild-Jenkins-Plugin@.
--
-- 'logConfig', 'buildBatch_logConfig' - Undocumented member.
--
-- 'phases', 'buildBatch_phases' - An array of @BuildBatchPhase@ objects the specify the phases of the
-- batch build.
--
-- 'projectName', 'buildBatch_projectName' - The name of the batch build project.
--
-- 'queuedTimeoutInMinutes', 'buildBatch_queuedTimeoutInMinutes' - Specifies the amount of time, in minutes, that the batch build is
-- allowed to be queued before it times out.
--
-- 'resolvedSourceVersion', 'buildBatch_resolvedSourceVersion' - The identifier of the resolved version of this batch build\'s source
-- code.
--
-- -   For CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit
--     ID.
--
-- -   For CodePipeline, the source revision provided by CodePipeline.
--
-- -   For Amazon S3, this does not apply.
--
-- 'secondaryArtifacts', 'buildBatch_secondaryArtifacts' - An array of @BuildArtifacts@ objects the define the build artifacts for
-- this batch build.
--
-- 'secondarySourceVersions', 'buildBatch_secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
-- must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example, @pr\/25@). If a branch name is
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
-- 'secondarySources', 'buildBatch_secondarySources' - An array of @ProjectSource@ objects that define the sources for the
-- batch build.
--
-- 'serviceRole', 'buildBatch_serviceRole' - The name of a service role used for builds in the batch.
--
-- 'source', 'buildBatch_source' - Undocumented member.
--
-- 'sourceVersion', 'buildBatch_sourceVersion' - The identifier of the version of the source code to be built.
--
-- 'startTime', 'buildBatch_startTime' - The date and time that the batch build started.
--
-- 'vpcConfig', 'buildBatch_vpcConfig' - Undocumented member.
newBuildBatch ::
  BuildBatch
newBuildBatch =
  BuildBatch'
    { arn = Prelude.Nothing,
      artifacts = Prelude.Nothing,
      buildBatchConfig = Prelude.Nothing,
      buildBatchNumber = Prelude.Nothing,
      buildBatchStatus = Prelude.Nothing,
      buildGroups = Prelude.Nothing,
      buildTimeoutInMinutes = Prelude.Nothing,
      cache = Prelude.Nothing,
      complete = Prelude.Nothing,
      currentPhase = Prelude.Nothing,
      debugSessionEnabled = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      endTime = Prelude.Nothing,
      environment = Prelude.Nothing,
      fileSystemLocations = Prelude.Nothing,
      id = Prelude.Nothing,
      initiator = Prelude.Nothing,
      logConfig = Prelude.Nothing,
      phases = Prelude.Nothing,
      projectName = Prelude.Nothing,
      queuedTimeoutInMinutes = Prelude.Nothing,
      resolvedSourceVersion = Prelude.Nothing,
      secondaryArtifacts = Prelude.Nothing,
      secondarySourceVersions = Prelude.Nothing,
      secondarySources = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      source = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      startTime = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The ARN of the batch build.
buildBatch_arn :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Text)
buildBatch_arn = Lens.lens (\BuildBatch' {arn} -> arn) (\s@BuildBatch' {} a -> s {arn = a} :: BuildBatch)

-- | A @BuildArtifacts@ object the defines the build artifacts for this batch
-- build.
buildBatch_artifacts :: Lens.Lens' BuildBatch (Prelude.Maybe BuildArtifacts)
buildBatch_artifacts = Lens.lens (\BuildBatch' {artifacts} -> artifacts) (\s@BuildBatch' {} a -> s {artifacts = a} :: BuildBatch)

-- | Undocumented member.
buildBatch_buildBatchConfig :: Lens.Lens' BuildBatch (Prelude.Maybe ProjectBuildBatchConfig)
buildBatch_buildBatchConfig = Lens.lens (\BuildBatch' {buildBatchConfig} -> buildBatchConfig) (\s@BuildBatch' {} a -> s {buildBatchConfig = a} :: BuildBatch)

-- | The number of the batch build. For each project, the @buildBatchNumber@
-- of its first batch build is @1@. The @buildBatchNumber@ of each
-- subsequent batch build is incremented by @1@. If a batch build is
-- deleted, the @buildBatchNumber@ of other batch builds does not change.
buildBatch_buildBatchNumber :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Integer)
buildBatch_buildBatchNumber = Lens.lens (\BuildBatch' {buildBatchNumber} -> buildBatchNumber) (\s@BuildBatch' {} a -> s {buildBatchNumber = a} :: BuildBatch)

-- | The status of the batch build.
buildBatch_buildBatchStatus :: Lens.Lens' BuildBatch (Prelude.Maybe StatusType)
buildBatch_buildBatchStatus = Lens.lens (\BuildBatch' {buildBatchStatus} -> buildBatchStatus) (\s@BuildBatch' {} a -> s {buildBatchStatus = a} :: BuildBatch)

-- | An array of @BuildGroup@ objects that define the build groups for the
-- batch build.
buildBatch_buildGroups :: Lens.Lens' BuildBatch (Prelude.Maybe [BuildGroup])
buildBatch_buildGroups = Lens.lens (\BuildBatch' {buildGroups} -> buildGroups) (\s@BuildBatch' {} a -> s {buildGroups = a} :: BuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the maximum amount of time, in minutes, that the build in a
-- batch must be completed in.
buildBatch_buildTimeoutInMinutes :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Int)
buildBatch_buildTimeoutInMinutes = Lens.lens (\BuildBatch' {buildTimeoutInMinutes} -> buildTimeoutInMinutes) (\s@BuildBatch' {} a -> s {buildTimeoutInMinutes = a} :: BuildBatch)

-- | Undocumented member.
buildBatch_cache :: Lens.Lens' BuildBatch (Prelude.Maybe ProjectCache)
buildBatch_cache = Lens.lens (\BuildBatch' {cache} -> cache) (\s@BuildBatch' {} a -> s {cache = a} :: BuildBatch)

-- | Indicates if the batch build is complete.
buildBatch_complete :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Bool)
buildBatch_complete = Lens.lens (\BuildBatch' {complete} -> complete) (\s@BuildBatch' {} a -> s {complete = a} :: BuildBatch)

-- | The current phase of the batch build.
buildBatch_currentPhase :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Text)
buildBatch_currentPhase = Lens.lens (\BuildBatch' {currentPhase} -> currentPhase) (\s@BuildBatch' {} a -> s {currentPhase = a} :: BuildBatch)

-- | Specifies if session debugging is enabled for this batch build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
-- Batch session debugging is not supported for matrix batch builds.
buildBatch_debugSessionEnabled :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Bool)
buildBatch_debugSessionEnabled = Lens.lens (\BuildBatch' {debugSessionEnabled} -> debugSessionEnabled) (\s@BuildBatch' {} a -> s {debugSessionEnabled = a} :: BuildBatch)

-- | The Key Management Service customer master key (CMK) to be used for
-- encrypting the batch build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
buildBatch_encryptionKey :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Text)
buildBatch_encryptionKey = Lens.lens (\BuildBatch' {encryptionKey} -> encryptionKey) (\s@BuildBatch' {} a -> s {encryptionKey = a} :: BuildBatch)

-- | The date and time that the batch build ended.
buildBatch_endTime :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.UTCTime)
buildBatch_endTime = Lens.lens (\BuildBatch' {endTime} -> endTime) (\s@BuildBatch' {} a -> s {endTime = a} :: BuildBatch) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
buildBatch_environment :: Lens.Lens' BuildBatch (Prelude.Maybe ProjectEnvironment)
buildBatch_environment = Lens.lens (\BuildBatch' {environment} -> environment) (\s@BuildBatch' {} a -> s {environment = a} :: BuildBatch)

-- | An array of @ProjectFileSystemLocation@ objects for the batch build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
buildBatch_fileSystemLocations :: Lens.Lens' BuildBatch (Prelude.Maybe [ProjectFileSystemLocation])
buildBatch_fileSystemLocations = Lens.lens (\BuildBatch' {fileSystemLocations} -> fileSystemLocations) (\s@BuildBatch' {} a -> s {fileSystemLocations = a} :: BuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the batch build.
buildBatch_id :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Text)
buildBatch_id = Lens.lens (\BuildBatch' {id} -> id) (\s@BuildBatch' {} a -> s {id = a} :: BuildBatch)

-- | The entity that started the batch build. Valid values include:
--
-- -   If CodePipeline started the build, the pipeline\'s name (for
--     example, @codepipeline\/my-demo-pipeline@).
--
-- -   If an IAM user started the build, the user\'s name.
--
-- -   If the Jenkins plugin for CodeBuild started the build, the string
--     @CodeBuild-Jenkins-Plugin@.
buildBatch_initiator :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Text)
buildBatch_initiator = Lens.lens (\BuildBatch' {initiator} -> initiator) (\s@BuildBatch' {} a -> s {initiator = a} :: BuildBatch)

-- | Undocumented member.
buildBatch_logConfig :: Lens.Lens' BuildBatch (Prelude.Maybe LogsConfig)
buildBatch_logConfig = Lens.lens (\BuildBatch' {logConfig} -> logConfig) (\s@BuildBatch' {} a -> s {logConfig = a} :: BuildBatch)

-- | An array of @BuildBatchPhase@ objects the specify the phases of the
-- batch build.
buildBatch_phases :: Lens.Lens' BuildBatch (Prelude.Maybe [BuildBatchPhase])
buildBatch_phases = Lens.lens (\BuildBatch' {phases} -> phases) (\s@BuildBatch' {} a -> s {phases = a} :: BuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | The name of the batch build project.
buildBatch_projectName :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Text)
buildBatch_projectName = Lens.lens (\BuildBatch' {projectName} -> projectName) (\s@BuildBatch' {} a -> s {projectName = a} :: BuildBatch)

-- | Specifies the amount of time, in minutes, that the batch build is
-- allowed to be queued before it times out.
buildBatch_queuedTimeoutInMinutes :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Int)
buildBatch_queuedTimeoutInMinutes = Lens.lens (\BuildBatch' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@BuildBatch' {} a -> s {queuedTimeoutInMinutes = a} :: BuildBatch)

-- | The identifier of the resolved version of this batch build\'s source
-- code.
--
-- -   For CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit
--     ID.
--
-- -   For CodePipeline, the source revision provided by CodePipeline.
--
-- -   For Amazon S3, this does not apply.
buildBatch_resolvedSourceVersion :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Text)
buildBatch_resolvedSourceVersion = Lens.lens (\BuildBatch' {resolvedSourceVersion} -> resolvedSourceVersion) (\s@BuildBatch' {} a -> s {resolvedSourceVersion = a} :: BuildBatch)

-- | An array of @BuildArtifacts@ objects the define the build artifacts for
-- this batch build.
buildBatch_secondaryArtifacts :: Lens.Lens' BuildBatch (Prelude.Maybe [BuildArtifacts])
buildBatch_secondaryArtifacts = Lens.lens (\BuildBatch' {secondaryArtifacts} -> secondaryArtifacts) (\s@BuildBatch' {} a -> s {secondaryArtifacts = a} :: BuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
-- must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example, @pr\/25@). If a branch name is
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
buildBatch_secondarySourceVersions :: Lens.Lens' BuildBatch (Prelude.Maybe [ProjectSourceVersion])
buildBatch_secondarySourceVersions = Lens.lens (\BuildBatch' {secondarySourceVersions} -> secondarySourceVersions) (\s@BuildBatch' {} a -> s {secondarySourceVersions = a} :: BuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ProjectSource@ objects that define the sources for the
-- batch build.
buildBatch_secondarySources :: Lens.Lens' BuildBatch (Prelude.Maybe [ProjectSource])
buildBatch_secondarySources = Lens.lens (\BuildBatch' {secondarySources} -> secondarySources) (\s@BuildBatch' {} a -> s {secondarySources = a} :: BuildBatch) Prelude.. Lens.mapping Lens.coerced

-- | The name of a service role used for builds in the batch.
buildBatch_serviceRole :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Text)
buildBatch_serviceRole = Lens.lens (\BuildBatch' {serviceRole} -> serviceRole) (\s@BuildBatch' {} a -> s {serviceRole = a} :: BuildBatch)

-- | Undocumented member.
buildBatch_source :: Lens.Lens' BuildBatch (Prelude.Maybe ProjectSource)
buildBatch_source = Lens.lens (\BuildBatch' {source} -> source) (\s@BuildBatch' {} a -> s {source = a} :: BuildBatch)

-- | The identifier of the version of the source code to be built.
buildBatch_sourceVersion :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.Text)
buildBatch_sourceVersion = Lens.lens (\BuildBatch' {sourceVersion} -> sourceVersion) (\s@BuildBatch' {} a -> s {sourceVersion = a} :: BuildBatch)

-- | The date and time that the batch build started.
buildBatch_startTime :: Lens.Lens' BuildBatch (Prelude.Maybe Prelude.UTCTime)
buildBatch_startTime = Lens.lens (\BuildBatch' {startTime} -> startTime) (\s@BuildBatch' {} a -> s {startTime = a} :: BuildBatch) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
buildBatch_vpcConfig :: Lens.Lens' BuildBatch (Prelude.Maybe VpcConfig)
buildBatch_vpcConfig = Lens.lens (\BuildBatch' {vpcConfig} -> vpcConfig) (\s@BuildBatch' {} a -> s {vpcConfig = a} :: BuildBatch)

instance Data.FromJSON BuildBatch where
  parseJSON =
    Data.withObject
      "BuildBatch"
      ( \x ->
          BuildBatch'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "artifacts")
            Prelude.<*> (x Data..:? "buildBatchConfig")
            Prelude.<*> (x Data..:? "buildBatchNumber")
            Prelude.<*> (x Data..:? "buildBatchStatus")
            Prelude.<*> (x Data..:? "buildGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "buildTimeoutInMinutes")
            Prelude.<*> (x Data..:? "cache")
            Prelude.<*> (x Data..:? "complete")
            Prelude.<*> (x Data..:? "currentPhase")
            Prelude.<*> (x Data..:? "debugSessionEnabled")
            Prelude.<*> (x Data..:? "encryptionKey")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "environment")
            Prelude.<*> ( x
                            Data..:? "fileSystemLocations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "initiator")
            Prelude.<*> (x Data..:? "logConfig")
            Prelude.<*> (x Data..:? "phases" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "projectName")
            Prelude.<*> (x Data..:? "queuedTimeoutInMinutes")
            Prelude.<*> (x Data..:? "resolvedSourceVersion")
            Prelude.<*> ( x
                            Data..:? "secondaryArtifacts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "secondarySourceVersions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "secondarySources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "serviceRole")
            Prelude.<*> (x Data..:? "source")
            Prelude.<*> (x Data..:? "sourceVersion")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "vpcConfig")
      )

instance Prelude.Hashable BuildBatch where
  hashWithSalt _salt BuildBatch' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` artifacts
      `Prelude.hashWithSalt` buildBatchConfig
      `Prelude.hashWithSalt` buildBatchNumber
      `Prelude.hashWithSalt` buildBatchStatus
      `Prelude.hashWithSalt` buildGroups
      `Prelude.hashWithSalt` buildTimeoutInMinutes
      `Prelude.hashWithSalt` cache
      `Prelude.hashWithSalt` complete
      `Prelude.hashWithSalt` currentPhase
      `Prelude.hashWithSalt` debugSessionEnabled
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` fileSystemLocations
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` initiator
      `Prelude.hashWithSalt` logConfig
      `Prelude.hashWithSalt` phases
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` queuedTimeoutInMinutes
      `Prelude.hashWithSalt` resolvedSourceVersion
      `Prelude.hashWithSalt` secondaryArtifacts
      `Prelude.hashWithSalt` secondarySourceVersions
      `Prelude.hashWithSalt` secondarySources
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData BuildBatch where
  rnf BuildBatch' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf artifacts
      `Prelude.seq` Prelude.rnf buildBatchConfig
      `Prelude.seq` Prelude.rnf buildBatchNumber
      `Prelude.seq` Prelude.rnf buildBatchStatus
      `Prelude.seq` Prelude.rnf buildGroups
      `Prelude.seq` Prelude.rnf buildTimeoutInMinutes
      `Prelude.seq` Prelude.rnf cache
      `Prelude.seq` Prelude.rnf complete
      `Prelude.seq` Prelude.rnf currentPhase
      `Prelude.seq` Prelude.rnf debugSessionEnabled
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf fileSystemLocations
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf initiator
      `Prelude.seq` Prelude.rnf logConfig
      `Prelude.seq` Prelude.rnf phases
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf
        queuedTimeoutInMinutes
      `Prelude.seq` Prelude.rnf
        resolvedSourceVersion
      `Prelude.seq` Prelude.rnf
        secondaryArtifacts
      `Prelude.seq` Prelude.rnf
        secondarySourceVersions
      `Prelude.seq` Prelude.rnf
        secondarySources
      `Prelude.seq` Prelude.rnf
        serviceRole
      `Prelude.seq` Prelude.rnf
        source
      `Prelude.seq` Prelude.rnf
        sourceVersion
      `Prelude.seq` Prelude.rnf
        startTime
      `Prelude.seq` Prelude.rnf
        vpcConfig
