{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ArtifactDetail
  ( ArtifactDetail (..)
  -- * Smart constructor
  , mkArtifactDetail
  -- * Lenses
  , aName
  , aS3location
  ) where

import qualified Network.AWS.CodePipeline.Types.Name as Types
import qualified Network.AWS.CodePipeline.Types.S3Location as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Artifact details for the action execution, such as the artifact location.
--
-- /See:/ 'mkArtifactDetail' smart constructor.
data ArtifactDetail = ArtifactDetail'
  { name :: Core.Maybe Types.Name
    -- ^ The artifact object name for the action execution.
  , s3location :: Core.Maybe Types.S3Location
    -- ^ The Amazon S3 artifact location for the action execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArtifactDetail' value with any optional fields omitted.
mkArtifactDetail
    :: ArtifactDetail
mkArtifactDetail
  = ArtifactDetail'{name = Core.Nothing, s3location = Core.Nothing}

-- | The artifact object name for the action execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' ArtifactDetail (Core.Maybe Types.Name)
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Amazon S3 artifact location for the action execution.
--
-- /Note:/ Consider using 's3location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aS3location :: Lens.Lens' ArtifactDetail (Core.Maybe Types.S3Location)
aS3location = Lens.field @"s3location"
{-# INLINEABLE aS3location #-}
{-# DEPRECATED s3location "Use generic-lens or generic-optics with 's3location' instead"  #-}

instance Core.FromJSON ArtifactDetail where
        parseJSON
          = Core.withObject "ArtifactDetail" Core.$
              \ x ->
                ArtifactDetail' Core.<$>
                  (x Core..:? "name") Core.<*> x Core..:? "s3location"
