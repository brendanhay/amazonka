{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ResolvedArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ResolvedArtifact
  ( ResolvedArtifact (..)
  -- * Smart constructor
  , mkResolvedArtifact
  -- * Lenses
  , raIdentifier
  , raLocation
  , raType
  ) where

import qualified Network.AWS.CodeBuild.Types.ArtifactsType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a resolved build artifact. A resolve artifact is an artifact that is built and deployed to the destination, such as Amazon Simple Storage Service (Amazon S3).
--
-- /See:/ 'mkResolvedArtifact' smart constructor.
data ResolvedArtifact = ResolvedArtifact'
  { identifier :: Core.Maybe Core.Text
    -- ^ The identifier of the artifact.
  , location :: Core.Maybe Core.Text
    -- ^ The location of the artifact.
  , type' :: Core.Maybe Types.ArtifactsType
    -- ^ Specifies the type of artifact.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResolvedArtifact' value with any optional fields omitted.
mkResolvedArtifact
    :: ResolvedArtifact
mkResolvedArtifact
  = ResolvedArtifact'{identifier = Core.Nothing,
                      location = Core.Nothing, type' = Core.Nothing}

-- | The identifier of the artifact.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raIdentifier :: Lens.Lens' ResolvedArtifact (Core.Maybe Core.Text)
raIdentifier = Lens.field @"identifier"
{-# INLINEABLE raIdentifier #-}
{-# DEPRECATED identifier "Use generic-lens or generic-optics with 'identifier' instead"  #-}

-- | The location of the artifact.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raLocation :: Lens.Lens' ResolvedArtifact (Core.Maybe Core.Text)
raLocation = Lens.field @"location"
{-# INLINEABLE raLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | Specifies the type of artifact.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raType :: Lens.Lens' ResolvedArtifact (Core.Maybe Types.ArtifactsType)
raType = Lens.field @"type'"
{-# INLINEABLE raType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ResolvedArtifact where
        parseJSON
          = Core.withObject "ResolvedArtifact" Core.$
              \ x ->
                ResolvedArtifact' Core.<$>
                  (x Core..:? "identifier") Core.<*> x Core..:? "location" Core.<*>
                    x Core..:? "type"
