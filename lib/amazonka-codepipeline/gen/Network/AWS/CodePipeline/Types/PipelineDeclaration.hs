{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineDeclaration
  ( PipelineDeclaration (..),

    -- * Smart constructor
    mkPipelineDeclaration,

    -- * Lenses
    pdName,
    pdRoleArn,
    pdStages,
    pdArtifactStore,
    pdArtifactStores,
    pdVersion,
  )
where

import qualified Network.AWS.CodePipeline.Types.AWSRegionName as Types
import qualified Network.AWS.CodePipeline.Types.ArtifactStore as Types
import qualified Network.AWS.CodePipeline.Types.PipelineName as Types
import qualified Network.AWS.CodePipeline.Types.RoleArn as Types
import qualified Network.AWS.CodePipeline.Types.StageDeclaration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the structure of actions and stages to be performed in the pipeline.
--
-- /See:/ 'mkPipelineDeclaration' smart constructor.
data PipelineDeclaration = PipelineDeclaration'
  { -- | The name of the pipeline.
    name :: Types.PipelineName,
    -- | The Amazon Resource Name (ARN) for AWS CodePipeline to use to either perform actions with no @actionRoleArn@ , or to use to assume roles for actions with an @actionRoleArn@ .
    roleArn :: Types.RoleArn,
    -- | The stage in which to perform the action.
    stages :: [Types.StageDeclaration],
    -- | Represents information about the S3 bucket where artifacts are stored for the pipeline.
    artifactStore :: Core.Maybe Types.ArtifactStore,
    -- | A mapping of @artifactStore@ objects and their corresponding AWS Regions. There must be an artifact store for the pipeline Region and for each cross-region action in the pipeline.
    artifactStores :: Core.Maybe (Core.HashMap Types.AWSRegionName Types.ArtifactStore),
    -- | The version number of the pipeline. A new pipeline always has a version number of 1. This number is incremented when a pipeline is updated.
    version :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineDeclaration' value with any optional fields omitted.
mkPipelineDeclaration ::
  -- | 'name'
  Types.PipelineName ->
  -- | 'roleArn'
  Types.RoleArn ->
  PipelineDeclaration
mkPipelineDeclaration name roleArn =
  PipelineDeclaration'
    { name,
      roleArn,
      stages = Core.mempty,
      artifactStore = Core.Nothing,
      artifactStores = Core.Nothing,
      version = Core.Nothing
    }

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' PipelineDeclaration Types.PipelineName
pdName = Lens.field @"name"
{-# DEPRECATED pdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) for AWS CodePipeline to use to either perform actions with no @actionRoleArn@ , or to use to assume roles for actions with an @actionRoleArn@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdRoleArn :: Lens.Lens' PipelineDeclaration Types.RoleArn
pdRoleArn = Lens.field @"roleArn"
{-# DEPRECATED pdRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The stage in which to perform the action.
--
-- /Note:/ Consider using 'stages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdStages :: Lens.Lens' PipelineDeclaration [Types.StageDeclaration]
pdStages = Lens.field @"stages"
{-# DEPRECATED pdStages "Use generic-lens or generic-optics with 'stages' instead." #-}

-- | Represents information about the S3 bucket where artifacts are stored for the pipeline.
--
-- /Note:/ Consider using 'artifactStore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdArtifactStore :: Lens.Lens' PipelineDeclaration (Core.Maybe Types.ArtifactStore)
pdArtifactStore = Lens.field @"artifactStore"
{-# DEPRECATED pdArtifactStore "Use generic-lens or generic-optics with 'artifactStore' instead." #-}

-- | A mapping of @artifactStore@ objects and their corresponding AWS Regions. There must be an artifact store for the pipeline Region and for each cross-region action in the pipeline.
--
-- /Note:/ Consider using 'artifactStores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdArtifactStores :: Lens.Lens' PipelineDeclaration (Core.Maybe (Core.HashMap Types.AWSRegionName Types.ArtifactStore))
pdArtifactStores = Lens.field @"artifactStores"
{-# DEPRECATED pdArtifactStores "Use generic-lens or generic-optics with 'artifactStores' instead." #-}

-- | The version number of the pipeline. A new pipeline always has a version number of 1. This number is incremented when a pipeline is updated.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdVersion :: Lens.Lens' PipelineDeclaration (Core.Maybe Core.Natural)
pdVersion = Lens.field @"version"
{-# DEPRECATED pdVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON PipelineDeclaration where
  toJSON PipelineDeclaration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("stages" Core..= stages),
            ("artifactStore" Core..=) Core.<$> artifactStore,
            ("artifactStores" Core..=) Core.<$> artifactStores,
            ("version" Core..=) Core.<$> version
          ]
      )

instance Core.FromJSON PipelineDeclaration where
  parseJSON =
    Core.withObject "PipelineDeclaration" Core.$
      \x ->
        PipelineDeclaration'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..: "roleArn")
          Core.<*> (x Core..:? "stages" Core..!= Core.mempty)
          Core.<*> (x Core..:? "artifactStore")
          Core.<*> (x Core..:? "artifactStores")
          Core.<*> (x Core..:? "version")
