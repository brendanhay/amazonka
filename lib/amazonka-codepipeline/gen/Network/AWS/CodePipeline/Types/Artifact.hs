{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.Artifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.Artifact
  ( Artifact (..)
  -- * Smart constructor
  , mkArtifact
  -- * Lenses
  , afLocation
  , afName
  , afRevision
  ) where

import qualified Network.AWS.CodePipeline.Types.ArtifactLocation as Types
import qualified Network.AWS.CodePipeline.Types.Name as Types
import qualified Network.AWS.CodePipeline.Types.Revision as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about an artifact that is worked on by actions in the pipeline.
--
-- /See:/ 'mkArtifact' smart constructor.
data Artifact = Artifact'
  { location :: Core.Maybe Types.ArtifactLocation
    -- ^ The location of an artifact.
  , name :: Core.Maybe Types.Name
    -- ^ The artifact's name.
  , revision :: Core.Maybe Types.Revision
    -- ^ The artifact's revision ID. Depending on the type of object, this could be a commit ID (GitHub) or a revision ID (Amazon S3).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Artifact' value with any optional fields omitted.
mkArtifact
    :: Artifact
mkArtifact
  = Artifact'{location = Core.Nothing, name = Core.Nothing,
              revision = Core.Nothing}

-- | The location of an artifact.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afLocation :: Lens.Lens' Artifact (Core.Maybe Types.ArtifactLocation)
afLocation = Lens.field @"location"
{-# INLINEABLE afLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The artifact's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afName :: Lens.Lens' Artifact (Core.Maybe Types.Name)
afName = Lens.field @"name"
{-# INLINEABLE afName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The artifact's revision ID. Depending on the type of object, this could be a commit ID (GitHub) or a revision ID (Amazon S3).
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afRevision :: Lens.Lens' Artifact (Core.Maybe Types.Revision)
afRevision = Lens.field @"revision"
{-# INLINEABLE afRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

instance Core.FromJSON Artifact where
        parseJSON
          = Core.withObject "Artifact" Core.$
              \ x ->
                Artifact' Core.<$>
                  (x Core..:? "location") Core.<*> x Core..:? "name" Core.<*>
                    x Core..:? "revision"
