{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactLocation
  ( ArtifactLocation (..),

    -- * Smart constructor
    mkArtifactLocation,

    -- * Lenses
    alS3Location,
    alType,
  )
where

import Network.AWS.CodePipeline.Types.ArtifactLocationType
import Network.AWS.CodePipeline.Types.S3ArtifactLocation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the location of an artifact.
--
-- /See:/ 'mkArtifactLocation' smart constructor.
data ArtifactLocation = ArtifactLocation'
  { -- | The S3 bucket that contains the artifact.
    s3Location :: Lude.Maybe S3ArtifactLocation,
    -- | The type of artifact in the location.
    type' :: Lude.Maybe ArtifactLocationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArtifactLocation' with the minimum fields required to make a request.
--
-- * 's3Location' - The S3 bucket that contains the artifact.
-- * 'type'' - The type of artifact in the location.
mkArtifactLocation ::
  ArtifactLocation
mkArtifactLocation =
  ArtifactLocation'
    { s3Location = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The S3 bucket that contains the artifact.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alS3Location :: Lens.Lens' ArtifactLocation (Lude.Maybe S3ArtifactLocation)
alS3Location = Lens.lens (s3Location :: ArtifactLocation -> Lude.Maybe S3ArtifactLocation) (\s a -> s {s3Location = a} :: ArtifactLocation)
{-# DEPRECATED alS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The type of artifact in the location.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alType :: Lens.Lens' ArtifactLocation (Lude.Maybe ArtifactLocationType)
alType = Lens.lens (type' :: ArtifactLocation -> Lude.Maybe ArtifactLocationType) (\s a -> s {type' = a} :: ArtifactLocation)
{-# DEPRECATED alType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ArtifactLocation where
  parseJSON =
    Lude.withObject
      "ArtifactLocation"
      ( \x ->
          ArtifactLocation'
            Lude.<$> (x Lude..:? "s3Location") Lude.<*> (x Lude..:? "type")
      )
