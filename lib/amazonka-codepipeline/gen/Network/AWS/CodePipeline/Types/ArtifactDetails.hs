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
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the details of an artifact.
--
-- /See:/ 'mkArtifactDetails' smart constructor.
data ArtifactDetails = ArtifactDetails'
  { minimumCount ::
      Lude.Natural,
    maximumCount :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArtifactDetails' with the minimum fields required to make a request.
--
-- * 'maximumCount' - The maximum number of artifacts allowed for the action type.
-- * 'minimumCount' - The minimum number of artifacts allowed for the action type.
mkArtifactDetails ::
  -- | 'minimumCount'
  Lude.Natural ->
  -- | 'maximumCount'
  Lude.Natural ->
  ArtifactDetails
mkArtifactDetails pMinimumCount_ pMaximumCount_ =
  ArtifactDetails'
    { minimumCount = pMinimumCount_,
      maximumCount = pMaximumCount_
    }

-- | The minimum number of artifacts allowed for the action type.
--
-- /Note:/ Consider using 'minimumCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMinimumCount :: Lens.Lens' ArtifactDetails Lude.Natural
adMinimumCount = Lens.lens (minimumCount :: ArtifactDetails -> Lude.Natural) (\s a -> s {minimumCount = a} :: ArtifactDetails)
{-# DEPRECATED adMinimumCount "Use generic-lens or generic-optics with 'minimumCount' instead." #-}

-- | The maximum number of artifacts allowed for the action type.
--
-- /Note:/ Consider using 'maximumCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMaximumCount :: Lens.Lens' ArtifactDetails Lude.Natural
adMaximumCount = Lens.lens (maximumCount :: ArtifactDetails -> Lude.Natural) (\s a -> s {maximumCount = a} :: ArtifactDetails)
{-# DEPRECATED adMaximumCount "Use generic-lens or generic-optics with 'maximumCount' instead." #-}

instance Lude.FromJSON ArtifactDetails where
  parseJSON =
    Lude.withObject
      "ArtifactDetails"
      ( \x ->
          ArtifactDetails'
            Lude.<$> (x Lude..: "minimumCount") Lude.<*> (x Lude..: "maximumCount")
      )

instance Lude.ToJSON ArtifactDetails where
  toJSON ArtifactDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("minimumCount" Lude..= minimumCount),
            Lude.Just ("maximumCount" Lude..= maximumCount)
          ]
      )
