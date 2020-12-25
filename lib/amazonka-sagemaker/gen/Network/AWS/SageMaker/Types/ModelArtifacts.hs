{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelArtifacts
  ( ModelArtifacts (..),

    -- * Smart constructor
    mkModelArtifacts,

    -- * Lenses
    maS3ModelArtifacts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.S3ModelArtifacts as Types

-- | Provides information about the location that is configured for storing model artifacts.
--
-- Model artifacts are the output that results from training a model, and typically consist of trained parameters, a model defintion that desribes how to compute inferences, and other metadata.
--
-- /See:/ 'mkModelArtifacts' smart constructor.
newtype ModelArtifacts = ModelArtifacts'
  { -- | The path of the S3 object that contains the model artifacts. For example, @s3://bucket-name/keynameprefix/model.tar.gz@ .
    s3ModelArtifacts :: Types.S3ModelArtifacts
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModelArtifacts' value with any optional fields omitted.
mkModelArtifacts ::
  -- | 's3ModelArtifacts'
  Types.S3ModelArtifacts ->
  ModelArtifacts
mkModelArtifacts s3ModelArtifacts =
  ModelArtifacts' {s3ModelArtifacts}

-- | The path of the S3 object that contains the model artifacts. For example, @s3://bucket-name/keynameprefix/model.tar.gz@ .
--
-- /Note:/ Consider using 's3ModelArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maS3ModelArtifacts :: Lens.Lens' ModelArtifacts Types.S3ModelArtifacts
maS3ModelArtifacts = Lens.field @"s3ModelArtifacts"
{-# DEPRECATED maS3ModelArtifacts "Use generic-lens or generic-optics with 's3ModelArtifacts' instead." #-}

instance Core.FromJSON ModelArtifacts where
  parseJSON =
    Core.withObject "ModelArtifacts" Core.$
      \x -> ModelArtifacts' Core.<$> (x Core..: "S3ModelArtifacts")
