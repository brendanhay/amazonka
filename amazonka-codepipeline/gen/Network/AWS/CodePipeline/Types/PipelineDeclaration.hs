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
-- Module      : Network.AWS.CodePipeline.Types.PipelineDeclaration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineDeclaration where

import Network.AWS.CodePipeline.Types.ArtifactStore
import Network.AWS.CodePipeline.Types.StageDeclaration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the structure of actions and stages to be performed in the
-- pipeline.
--
-- /See:/ 'newPipelineDeclaration' smart constructor.
data PipelineDeclaration = PipelineDeclaration'
  { -- | The version number of the pipeline. A new pipeline always has a version
    -- number of 1. This number is incremented when a pipeline is updated.
    version :: Core.Maybe Core.Natural,
    -- | A mapping of @artifactStore@ objects and their corresponding AWS
    -- Regions. There must be an artifact store for the pipeline Region and for
    -- each cross-region action in the pipeline.
    --
    -- You must include either @artifactStore@ or @artifactStores@ in your
    -- pipeline, but you cannot use both. If you create a cross-region action
    -- in your pipeline, you must use @artifactStores@.
    artifactStores :: Core.Maybe (Core.HashMap Core.Text ArtifactStore),
    -- | Represents information about the S3 bucket where artifacts are stored
    -- for the pipeline.
    --
    -- You must include either @artifactStore@ or @artifactStores@ in your
    -- pipeline, but you cannot use both. If you create a cross-region action
    -- in your pipeline, you must use @artifactStores@.
    artifactStore :: Core.Maybe ArtifactStore,
    -- | The name of the pipeline.
    name :: Core.Text,
    -- | The Amazon Resource Name (ARN) for AWS CodePipeline to use to either
    -- perform actions with no @actionRoleArn@, or to use to assume roles for
    -- actions with an @actionRoleArn@.
    roleArn :: Core.Text,
    -- | The stage in which to perform the action.
    stages :: [StageDeclaration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'pipelineDeclaration_version' - The version number of the pipeline. A new pipeline always has a version
-- number of 1. This number is incremented when a pipeline is updated.
--
-- 'artifactStores', 'pipelineDeclaration_artifactStores' - A mapping of @artifactStore@ objects and their corresponding AWS
-- Regions. There must be an artifact store for the pipeline Region and for
-- each cross-region action in the pipeline.
--
-- You must include either @artifactStore@ or @artifactStores@ in your
-- pipeline, but you cannot use both. If you create a cross-region action
-- in your pipeline, you must use @artifactStores@.
--
-- 'artifactStore', 'pipelineDeclaration_artifactStore' - Represents information about the S3 bucket where artifacts are stored
-- for the pipeline.
--
-- You must include either @artifactStore@ or @artifactStores@ in your
-- pipeline, but you cannot use both. If you create a cross-region action
-- in your pipeline, you must use @artifactStores@.
--
-- 'name', 'pipelineDeclaration_name' - The name of the pipeline.
--
-- 'roleArn', 'pipelineDeclaration_roleArn' - The Amazon Resource Name (ARN) for AWS CodePipeline to use to either
-- perform actions with no @actionRoleArn@, or to use to assume roles for
-- actions with an @actionRoleArn@.
--
-- 'stages', 'pipelineDeclaration_stages' - The stage in which to perform the action.
newPipelineDeclaration ::
  -- | 'name'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  PipelineDeclaration
newPipelineDeclaration pName_ pRoleArn_ =
  PipelineDeclaration'
    { version = Core.Nothing,
      artifactStores = Core.Nothing,
      artifactStore = Core.Nothing,
      name = pName_,
      roleArn = pRoleArn_,
      stages = Core.mempty
    }

-- | The version number of the pipeline. A new pipeline always has a version
-- number of 1. This number is incremented when a pipeline is updated.
pipelineDeclaration_version :: Lens.Lens' PipelineDeclaration (Core.Maybe Core.Natural)
pipelineDeclaration_version = Lens.lens (\PipelineDeclaration' {version} -> version) (\s@PipelineDeclaration' {} a -> s {version = a} :: PipelineDeclaration)

-- | A mapping of @artifactStore@ objects and their corresponding AWS
-- Regions. There must be an artifact store for the pipeline Region and for
-- each cross-region action in the pipeline.
--
-- You must include either @artifactStore@ or @artifactStores@ in your
-- pipeline, but you cannot use both. If you create a cross-region action
-- in your pipeline, you must use @artifactStores@.
pipelineDeclaration_artifactStores :: Lens.Lens' PipelineDeclaration (Core.Maybe (Core.HashMap Core.Text ArtifactStore))
pipelineDeclaration_artifactStores = Lens.lens (\PipelineDeclaration' {artifactStores} -> artifactStores) (\s@PipelineDeclaration' {} a -> s {artifactStores = a} :: PipelineDeclaration) Core.. Lens.mapping Lens._Coerce

-- | Represents information about the S3 bucket where artifacts are stored
-- for the pipeline.
--
-- You must include either @artifactStore@ or @artifactStores@ in your
-- pipeline, but you cannot use both. If you create a cross-region action
-- in your pipeline, you must use @artifactStores@.
pipelineDeclaration_artifactStore :: Lens.Lens' PipelineDeclaration (Core.Maybe ArtifactStore)
pipelineDeclaration_artifactStore = Lens.lens (\PipelineDeclaration' {artifactStore} -> artifactStore) (\s@PipelineDeclaration' {} a -> s {artifactStore = a} :: PipelineDeclaration)

-- | The name of the pipeline.
pipelineDeclaration_name :: Lens.Lens' PipelineDeclaration Core.Text
pipelineDeclaration_name = Lens.lens (\PipelineDeclaration' {name} -> name) (\s@PipelineDeclaration' {} a -> s {name = a} :: PipelineDeclaration)

-- | The Amazon Resource Name (ARN) for AWS CodePipeline to use to either
-- perform actions with no @actionRoleArn@, or to use to assume roles for
-- actions with an @actionRoleArn@.
pipelineDeclaration_roleArn :: Lens.Lens' PipelineDeclaration Core.Text
pipelineDeclaration_roleArn = Lens.lens (\PipelineDeclaration' {roleArn} -> roleArn) (\s@PipelineDeclaration' {} a -> s {roleArn = a} :: PipelineDeclaration)

-- | The stage in which to perform the action.
pipelineDeclaration_stages :: Lens.Lens' PipelineDeclaration [StageDeclaration]
pipelineDeclaration_stages = Lens.lens (\PipelineDeclaration' {stages} -> stages) (\s@PipelineDeclaration' {} a -> s {stages = a} :: PipelineDeclaration) Core.. Lens._Coerce

instance Core.FromJSON PipelineDeclaration where
  parseJSON =
    Core.withObject
      "PipelineDeclaration"
      ( \x ->
          PipelineDeclaration'
            Core.<$> (x Core..:? "version")
            Core.<*> (x Core..:? "artifactStores" Core..!= Core.mempty)
            Core.<*> (x Core..:? "artifactStore")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "roleArn")
            Core.<*> (x Core..:? "stages" Core..!= Core.mempty)
      )

instance Core.Hashable PipelineDeclaration

instance Core.NFData PipelineDeclaration

instance Core.ToJSON PipelineDeclaration where
  toJSON PipelineDeclaration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("version" Core..=) Core.<$> version,
            ("artifactStores" Core..=) Core.<$> artifactStores,
            ("artifactStore" Core..=) Core.<$> artifactStore,
            Core.Just ("name" Core..= name),
            Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("stages" Core..= stages)
          ]
      )
