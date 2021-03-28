{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.Artifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ImportExport.Types.Artifact
  ( Artifact (..)
  -- * Smart constructor
  , mkArtifact
  -- * Lenses
  , aDescription
  , aURL
  ) where

import qualified Network.AWS.ImportExport.Types.Description as Types
import qualified Network.AWS.ImportExport.Types.URL as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A discrete item that contains the description and URL of an artifact (such as a PDF).
--
-- /See:/ 'mkArtifact' smart constructor.
data Artifact = Artifact'
  { description :: Core.Maybe Types.Description
  , url :: Core.Maybe Types.URL
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Artifact' value with any optional fields omitted.
mkArtifact
    :: Artifact
mkArtifact
  = Artifact'{description = Core.Nothing, url = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Artifact (Core.Maybe Types.Description)
aDescription = Lens.field @"description"
{-# INLINEABLE aDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aURL :: Lens.Lens' Artifact (Core.Maybe Types.URL)
aURL = Lens.field @"url"
{-# INLINEABLE aURL #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromXML Artifact where
        parseXML x
          = Artifact' Core.<$>
              (x Core..@? "Description") Core.<*> x Core..@? "URL"
