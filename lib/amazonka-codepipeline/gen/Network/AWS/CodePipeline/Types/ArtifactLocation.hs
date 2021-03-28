{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ArtifactLocation
  ( ArtifactLocation (..)
  -- * Smart constructor
  , mkArtifactLocation
  -- * Lenses
  , alS3Location
  , alType
  ) where

import qualified Network.AWS.CodePipeline.Types.ArtifactLocationType as Types
import qualified Network.AWS.CodePipeline.Types.S3ArtifactLocation as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the location of an artifact.
--
-- /See:/ 'mkArtifactLocation' smart constructor.
data ArtifactLocation = ArtifactLocation'
  { s3Location :: Core.Maybe Types.S3ArtifactLocation
    -- ^ The S3 bucket that contains the artifact.
  , type' :: Core.Maybe Types.ArtifactLocationType
    -- ^ The type of artifact in the location.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArtifactLocation' value with any optional fields omitted.
mkArtifactLocation
    :: ArtifactLocation
mkArtifactLocation
  = ArtifactLocation'{s3Location = Core.Nothing,
                      type' = Core.Nothing}

-- | The S3 bucket that contains the artifact.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alS3Location :: Lens.Lens' ArtifactLocation (Core.Maybe Types.S3ArtifactLocation)
alS3Location = Lens.field @"s3Location"
{-# INLINEABLE alS3Location #-}
{-# DEPRECATED s3Location "Use generic-lens or generic-optics with 's3Location' instead"  #-}

-- | The type of artifact in the location.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alType :: Lens.Lens' ArtifactLocation (Core.Maybe Types.ArtifactLocationType)
alType = Lens.field @"type'"
{-# INLINEABLE alType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ArtifactLocation where
        parseJSON
          = Core.withObject "ArtifactLocation" Core.$
              \ x ->
                ArtifactLocation' Core.<$>
                  (x Core..:? "s3Location") Core.<*> x Core..:? "type"
