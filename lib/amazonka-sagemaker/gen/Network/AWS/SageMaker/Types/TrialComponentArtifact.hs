{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrialComponentArtifact
  ( TrialComponentArtifact (..)
  -- * Smart constructor
  , mkTrialComponentArtifact
  -- * Lenses
  , tcaValue
  , tcaMediaType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MediaType as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentArtifactValue as Types

-- | Represents an input or output artifact of a trial component. You specify @TrialComponentArtifact@ as part of the @InputArtifacts@ and @OutputArtifacts@ parameters in the 'CreateTrialComponent' request.
--
-- Examples of input artifacts are datasets, algorithms, hyperparameters, source code, and instance types. Examples of output artifacts are metrics, snapshots, logs, and images.
--
-- /See:/ 'mkTrialComponentArtifact' smart constructor.
data TrialComponentArtifact = TrialComponentArtifact'
  { value :: Types.TrialComponentArtifactValue
    -- ^ The location of the artifact.
  , mediaType :: Core.Maybe Types.MediaType
    -- ^ The media type of the artifact, which indicates the type of data in the artifact file. The media type consists of a /type/ and a /subtype/ concatenated with a slash (/) character, for example, text/csv, image/jpeg, and s3/uri. The type specifies the category of the media. The subtype specifies the kind of data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrialComponentArtifact' value with any optional fields omitted.
mkTrialComponentArtifact
    :: Types.TrialComponentArtifactValue -- ^ 'value'
    -> TrialComponentArtifact
mkTrialComponentArtifact value
  = TrialComponentArtifact'{value, mediaType = Core.Nothing}

-- | The location of the artifact.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcaValue :: Lens.Lens' TrialComponentArtifact Types.TrialComponentArtifactValue
tcaValue = Lens.field @"value"
{-# INLINEABLE tcaValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | The media type of the artifact, which indicates the type of data in the artifact file. The media type consists of a /type/ and a /subtype/ concatenated with a slash (/) character, for example, text/csv, image/jpeg, and s3/uri. The type specifies the category of the media. The subtype specifies the kind of data.
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcaMediaType :: Lens.Lens' TrialComponentArtifact (Core.Maybe Types.MediaType)
tcaMediaType = Lens.field @"mediaType"
{-# INLINEABLE tcaMediaType #-}
{-# DEPRECATED mediaType "Use generic-lens or generic-optics with 'mediaType' instead"  #-}

instance Core.FromJSON TrialComponentArtifact where
        toJSON TrialComponentArtifact{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Value" Core..= value),
                  ("MediaType" Core..=) Core.<$> mediaType])

instance Core.FromJSON TrialComponentArtifact where
        parseJSON
          = Core.withObject "TrialComponentArtifact" Core.$
              \ x ->
                TrialComponentArtifact' Core.<$>
                  (x Core..: "Value") Core.<*> x Core..:? "MediaType"
