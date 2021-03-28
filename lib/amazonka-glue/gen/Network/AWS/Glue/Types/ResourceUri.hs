{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ResourceUri
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ResourceUri
  ( ResourceUri (..)
  -- * Smart constructor
  , mkResourceUri
  -- * Lenses
  , ruResourceType
  , ruUri
  ) where

import qualified Network.AWS.Glue.Types.ResourceType as Types
import qualified Network.AWS.Glue.Types.Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The URIs for function resources.
--
-- /See:/ 'mkResourceUri' smart constructor.
data ResourceUri = ResourceUri'
  { resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of the resource.
  , uri :: Core.Maybe Types.Uri
    -- ^ The URI for accessing the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceUri' value with any optional fields omitted.
mkResourceUri
    :: ResourceUri
mkResourceUri
  = ResourceUri'{resourceType = Core.Nothing, uri = Core.Nothing}

-- | The type of the resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruResourceType :: Lens.Lens' ResourceUri (Core.Maybe Types.ResourceType)
ruResourceType = Lens.field @"resourceType"
{-# INLINEABLE ruResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The URI for accessing the resource.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruUri :: Lens.Lens' ResourceUri (Core.Maybe Types.Uri)
ruUri = Lens.field @"uri"
{-# INLINEABLE ruUri #-}
{-# DEPRECATED uri "Use generic-lens or generic-optics with 'uri' instead"  #-}

instance Core.FromJSON ResourceUri where
        toJSON ResourceUri{..}
          = Core.object
              (Core.catMaybes
                 [("ResourceType" Core..=) Core.<$> resourceType,
                  ("Uri" Core..=) Core.<$> uri])

instance Core.FromJSON ResourceUri where
        parseJSON
          = Core.withObject "ResourceUri" Core.$
              \ x ->
                ResourceUri' Core.<$>
                  (x Core..:? "ResourceType") Core.<*> x Core..:? "Uri"
