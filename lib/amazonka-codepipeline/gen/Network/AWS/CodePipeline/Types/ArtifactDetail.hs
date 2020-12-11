-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactDetail
  ( ArtifactDetail (..),

    -- * Smart constructor
    mkArtifactDetail,

    -- * Lenses
    aName,
    aS3location,
  )
where

import Network.AWS.CodePipeline.Types.S3Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Artifact details for the action execution, such as the artifact location.
--
-- /See:/ 'mkArtifactDetail' smart constructor.
data ArtifactDetail = ArtifactDetail'
  { name :: Lude.Maybe Lude.Text,
    s3location :: Lude.Maybe S3Location
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArtifactDetail' with the minimum fields required to make a request.
--
-- * 'name' - The artifact object name for the action execution.
-- * 's3location' - The Amazon S3 artifact location for the action execution.
mkArtifactDetail ::
  ArtifactDetail
mkArtifactDetail =
  ArtifactDetail' {name = Lude.Nothing, s3location = Lude.Nothing}

-- | The artifact object name for the action execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' ArtifactDetail (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: ArtifactDetail -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ArtifactDetail)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon S3 artifact location for the action execution.
--
-- /Note:/ Consider using 's3location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aS3location :: Lens.Lens' ArtifactDetail (Lude.Maybe S3Location)
aS3location = Lens.lens (s3location :: ArtifactDetail -> Lude.Maybe S3Location) (\s a -> s {s3location = a} :: ArtifactDetail)
{-# DEPRECATED aS3location "Use generic-lens or generic-optics with 's3location' instead." #-}

instance Lude.FromJSON ArtifactDetail where
  parseJSON =
    Lude.withObject
      "ArtifactDetail"
      ( \x ->
          ArtifactDetail'
            Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "s3location")
      )
