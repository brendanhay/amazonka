{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactDetails
  ( ArtifactDetails (..),

    -- * Smart constructor
    mkArtifactDetails,

    -- * Lenses
    adMinimumCount,
    adMaximumCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about the details of an artifact.
--
-- /See:/ 'mkArtifactDetails' smart constructor.
data ArtifactDetails = ArtifactDetails'
  { -- | The minimum number of artifacts allowed for the action type.
    minimumCount :: Core.Natural,
    -- | The maximum number of artifacts allowed for the action type.
    maximumCount :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArtifactDetails' value with any optional fields omitted.
mkArtifactDetails ::
  -- | 'minimumCount'
  Core.Natural ->
  -- | 'maximumCount'
  Core.Natural ->
  ArtifactDetails
mkArtifactDetails minimumCount maximumCount =
  ArtifactDetails' {minimumCount, maximumCount}

-- | The minimum number of artifacts allowed for the action type.
--
-- /Note:/ Consider using 'minimumCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMinimumCount :: Lens.Lens' ArtifactDetails Core.Natural
adMinimumCount = Lens.field @"minimumCount"
{-# DEPRECATED adMinimumCount "Use generic-lens or generic-optics with 'minimumCount' instead." #-}

-- | The maximum number of artifacts allowed for the action type.
--
-- /Note:/ Consider using 'maximumCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMaximumCount :: Lens.Lens' ArtifactDetails Core.Natural
adMaximumCount = Lens.field @"maximumCount"
{-# DEPRECATED adMaximumCount "Use generic-lens or generic-optics with 'maximumCount' instead." #-}

instance Core.FromJSON ArtifactDetails where
  toJSON ArtifactDetails {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("minimumCount" Core..= minimumCount),
            Core.Just ("maximumCount" Core..= maximumCount)
          ]
      )

instance Core.FromJSON ArtifactDetails where
  parseJSON =
    Core.withObject "ArtifactDetails" Core.$
      \x ->
        ArtifactDetails'
          Core.<$> (x Core..: "minimumCount") Core.<*> (x Core..: "maximumCount")
