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
    pdArtifactStores,
    pdArtifactStore,
    pdVersion,
    pdName,
    pdRoleARN,
    pdStages,
  )
where

import Network.AWS.CodePipeline.Types.ArtifactStore
import Network.AWS.CodePipeline.Types.StageDeclaration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the structure of actions and stages to be performed in the pipeline.
--
-- /See:/ 'mkPipelineDeclaration' smart constructor.
data PipelineDeclaration = PipelineDeclaration'
  { artifactStores ::
      Lude.Maybe (Lude.HashMap Lude.Text (ArtifactStore)),
    artifactStore :: Lude.Maybe ArtifactStore,
    version :: Lude.Maybe Lude.Natural,
    name :: Lude.Text,
    roleARN :: Lude.Text,
    stages :: [StageDeclaration]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineDeclaration' with the minimum fields required to make a request.
--
-- * 'artifactStore' - Represents information about the S3 bucket where artifacts are stored for the pipeline.
-- * 'artifactStores' - A mapping of @artifactStore@ objects and their corresponding AWS Regions. There must be an artifact store for the pipeline Region and for each cross-region action in the pipeline.
-- * 'name' - The name of the pipeline.
-- * 'roleARN' - The Amazon Resource Name (ARN) for AWS CodePipeline to use to either perform actions with no @actionRoleArn@ , or to use to assume roles for actions with an @actionRoleArn@ .
-- * 'stages' - The stage in which to perform the action.
-- * 'version' - The version number of the pipeline. A new pipeline always has a version number of 1. This number is incremented when a pipeline is updated.
mkPipelineDeclaration ::
  -- | 'name'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  PipelineDeclaration
mkPipelineDeclaration pName_ pRoleARN_ =
  PipelineDeclaration'
    { artifactStores = Lude.Nothing,
      artifactStore = Lude.Nothing,
      version = Lude.Nothing,
      name = pName_,
      roleARN = pRoleARN_,
      stages = Lude.mempty
    }

-- | A mapping of @artifactStore@ objects and their corresponding AWS Regions. There must be an artifact store for the pipeline Region and for each cross-region action in the pipeline.
--
-- /Note:/ Consider using 'artifactStores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdArtifactStores :: Lens.Lens' PipelineDeclaration (Lude.Maybe (Lude.HashMap Lude.Text (ArtifactStore)))
pdArtifactStores = Lens.lens (artifactStores :: PipelineDeclaration -> Lude.Maybe (Lude.HashMap Lude.Text (ArtifactStore))) (\s a -> s {artifactStores = a} :: PipelineDeclaration)
{-# DEPRECATED pdArtifactStores "Use generic-lens or generic-optics with 'artifactStores' instead." #-}

-- | Represents information about the S3 bucket where artifacts are stored for the pipeline.
--
-- /Note:/ Consider using 'artifactStore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdArtifactStore :: Lens.Lens' PipelineDeclaration (Lude.Maybe ArtifactStore)
pdArtifactStore = Lens.lens (artifactStore :: PipelineDeclaration -> Lude.Maybe ArtifactStore) (\s a -> s {artifactStore = a} :: PipelineDeclaration)
{-# DEPRECATED pdArtifactStore "Use generic-lens or generic-optics with 'artifactStore' instead." #-}

-- | The version number of the pipeline. A new pipeline always has a version number of 1. This number is incremented when a pipeline is updated.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdVersion :: Lens.Lens' PipelineDeclaration (Lude.Maybe Lude.Natural)
pdVersion = Lens.lens (version :: PipelineDeclaration -> Lude.Maybe Lude.Natural) (\s a -> s {version = a} :: PipelineDeclaration)
{-# DEPRECATED pdVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' PipelineDeclaration Lude.Text
pdName = Lens.lens (name :: PipelineDeclaration -> Lude.Text) (\s a -> s {name = a} :: PipelineDeclaration)
{-# DEPRECATED pdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) for AWS CodePipeline to use to either perform actions with no @actionRoleArn@ , or to use to assume roles for actions with an @actionRoleArn@ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdRoleARN :: Lens.Lens' PipelineDeclaration Lude.Text
pdRoleARN = Lens.lens (roleARN :: PipelineDeclaration -> Lude.Text) (\s a -> s {roleARN = a} :: PipelineDeclaration)
{-# DEPRECATED pdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The stage in which to perform the action.
--
-- /Note:/ Consider using 'stages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdStages :: Lens.Lens' PipelineDeclaration [StageDeclaration]
pdStages = Lens.lens (stages :: PipelineDeclaration -> [StageDeclaration]) (\s a -> s {stages = a} :: PipelineDeclaration)
{-# DEPRECATED pdStages "Use generic-lens or generic-optics with 'stages' instead." #-}

instance Lude.FromJSON PipelineDeclaration where
  parseJSON =
    Lude.withObject
      "PipelineDeclaration"
      ( \x ->
          PipelineDeclaration'
            Lude.<$> (x Lude..:? "artifactStores" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "artifactStore")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "roleArn")
            Lude.<*> (x Lude..:? "stages" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PipelineDeclaration where
  toJSON PipelineDeclaration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("artifactStores" Lude..=) Lude.<$> artifactStores,
            ("artifactStore" Lude..=) Lude.<$> artifactStore,
            ("version" Lude..=) Lude.<$> version,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("roleArn" Lude..= roleARN),
            Lude.Just ("stages" Lude..= stages)
          ]
      )
