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
-- Module      : Amazonka.CodePipeline.Types.PipelineDeclaration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.PipelineDeclaration where

import Amazonka.CodePipeline.Types.ArtifactStore
import Amazonka.CodePipeline.Types.StageDeclaration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the structure of actions and stages to be performed in the
-- pipeline.
--
-- /See:/ 'newPipelineDeclaration' smart constructor.
data PipelineDeclaration = PipelineDeclaration'
  { -- | Represents information about the S3 bucket where artifacts are stored
    -- for the pipeline.
    --
    -- You must include either @artifactStore@ or @artifactStores@ in your
    -- pipeline, but you cannot use both. If you create a cross-region action
    -- in your pipeline, you must use @artifactStores@.
    artifactStore :: Prelude.Maybe ArtifactStore,
    -- | A mapping of @artifactStore@ objects and their corresponding AWS
    -- Regions. There must be an artifact store for the pipeline Region and for
    -- each cross-region action in the pipeline.
    --
    -- You must include either @artifactStore@ or @artifactStores@ in your
    -- pipeline, but you cannot use both. If you create a cross-region action
    -- in your pipeline, you must use @artifactStores@.
    artifactStores :: Prelude.Maybe (Prelude.HashMap Prelude.Text ArtifactStore),
    -- | The version number of the pipeline. A new pipeline always has a version
    -- number of 1. This number is incremented when a pipeline is updated.
    version :: Prelude.Maybe Prelude.Natural,
    -- | The name of the pipeline.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for AWS CodePipeline to use to either
    -- perform actions with no @actionRoleArn@, or to use to assume roles for
    -- actions with an @actionRoleArn@.
    roleArn :: Prelude.Text,
    -- | The stage in which to perform the action.
    stages :: [StageDeclaration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactStore', 'pipelineDeclaration_artifactStore' - Represents information about the S3 bucket where artifacts are stored
-- for the pipeline.
--
-- You must include either @artifactStore@ or @artifactStores@ in your
-- pipeline, but you cannot use both. If you create a cross-region action
-- in your pipeline, you must use @artifactStores@.
--
-- 'artifactStores', 'pipelineDeclaration_artifactStores' - A mapping of @artifactStore@ objects and their corresponding AWS
-- Regions. There must be an artifact store for the pipeline Region and for
-- each cross-region action in the pipeline.
--
-- You must include either @artifactStore@ or @artifactStores@ in your
-- pipeline, but you cannot use both. If you create a cross-region action
-- in your pipeline, you must use @artifactStores@.
--
-- 'version', 'pipelineDeclaration_version' - The version number of the pipeline. A new pipeline always has a version
-- number of 1. This number is incremented when a pipeline is updated.
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
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  PipelineDeclaration
newPipelineDeclaration pName_ pRoleArn_ =
  PipelineDeclaration'
    { artifactStore =
        Prelude.Nothing,
      artifactStores = Prelude.Nothing,
      version = Prelude.Nothing,
      name = pName_,
      roleArn = pRoleArn_,
      stages = Prelude.mempty
    }

-- | Represents information about the S3 bucket where artifacts are stored
-- for the pipeline.
--
-- You must include either @artifactStore@ or @artifactStores@ in your
-- pipeline, but you cannot use both. If you create a cross-region action
-- in your pipeline, you must use @artifactStores@.
pipelineDeclaration_artifactStore :: Lens.Lens' PipelineDeclaration (Prelude.Maybe ArtifactStore)
pipelineDeclaration_artifactStore = Lens.lens (\PipelineDeclaration' {artifactStore} -> artifactStore) (\s@PipelineDeclaration' {} a -> s {artifactStore = a} :: PipelineDeclaration)

-- | A mapping of @artifactStore@ objects and their corresponding AWS
-- Regions. There must be an artifact store for the pipeline Region and for
-- each cross-region action in the pipeline.
--
-- You must include either @artifactStore@ or @artifactStores@ in your
-- pipeline, but you cannot use both. If you create a cross-region action
-- in your pipeline, you must use @artifactStores@.
pipelineDeclaration_artifactStores :: Lens.Lens' PipelineDeclaration (Prelude.Maybe (Prelude.HashMap Prelude.Text ArtifactStore))
pipelineDeclaration_artifactStores = Lens.lens (\PipelineDeclaration' {artifactStores} -> artifactStores) (\s@PipelineDeclaration' {} a -> s {artifactStores = a} :: PipelineDeclaration) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the pipeline. A new pipeline always has a version
-- number of 1. This number is incremented when a pipeline is updated.
pipelineDeclaration_version :: Lens.Lens' PipelineDeclaration (Prelude.Maybe Prelude.Natural)
pipelineDeclaration_version = Lens.lens (\PipelineDeclaration' {version} -> version) (\s@PipelineDeclaration' {} a -> s {version = a} :: PipelineDeclaration)

-- | The name of the pipeline.
pipelineDeclaration_name :: Lens.Lens' PipelineDeclaration Prelude.Text
pipelineDeclaration_name = Lens.lens (\PipelineDeclaration' {name} -> name) (\s@PipelineDeclaration' {} a -> s {name = a} :: PipelineDeclaration)

-- | The Amazon Resource Name (ARN) for AWS CodePipeline to use to either
-- perform actions with no @actionRoleArn@, or to use to assume roles for
-- actions with an @actionRoleArn@.
pipelineDeclaration_roleArn :: Lens.Lens' PipelineDeclaration Prelude.Text
pipelineDeclaration_roleArn = Lens.lens (\PipelineDeclaration' {roleArn} -> roleArn) (\s@PipelineDeclaration' {} a -> s {roleArn = a} :: PipelineDeclaration)

-- | The stage in which to perform the action.
pipelineDeclaration_stages :: Lens.Lens' PipelineDeclaration [StageDeclaration]
pipelineDeclaration_stages = Lens.lens (\PipelineDeclaration' {stages} -> stages) (\s@PipelineDeclaration' {} a -> s {stages = a} :: PipelineDeclaration) Prelude.. Lens.coerced

instance Data.FromJSON PipelineDeclaration where
  parseJSON =
    Data.withObject
      "PipelineDeclaration"
      ( \x ->
          PipelineDeclaration'
            Prelude.<$> (x Data..:? "artifactStore")
            Prelude.<*> (x Data..:? "artifactStores" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "version")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..:? "stages" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PipelineDeclaration where
  hashWithSalt _salt PipelineDeclaration' {..} =
    _salt
      `Prelude.hashWithSalt` artifactStore
      `Prelude.hashWithSalt` artifactStores
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` stages

instance Prelude.NFData PipelineDeclaration where
  rnf PipelineDeclaration' {..} =
    Prelude.rnf artifactStore
      `Prelude.seq` Prelude.rnf artifactStores
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf stages

instance Data.ToJSON PipelineDeclaration where
  toJSON PipelineDeclaration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("artifactStore" Data..=) Prelude.<$> artifactStore,
            ("artifactStores" Data..=)
              Prelude.<$> artifactStores,
            ("version" Data..=) Prelude.<$> version,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("stages" Data..= stages)
          ]
      )
