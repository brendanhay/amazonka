{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.FacetAttributeUpdate
  ( FacetAttributeUpdate (..)
  -- * Smart constructor
  , mkFacetAttributeUpdate
  -- * Lenses
  , fauAction
  , fauAttribute
  ) where

import qualified Network.AWS.CloudDirectory.Types.FacetAttribute as Types
import qualified Network.AWS.CloudDirectory.Types.UpdateActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that contains information used to update an attribute.
--
-- /See:/ 'mkFacetAttributeUpdate' smart constructor.
data FacetAttributeUpdate = FacetAttributeUpdate'
  { action :: Core.Maybe Types.UpdateActionType
    -- ^ The action to perform when updating the attribute.
  , attribute :: Core.Maybe Types.FacetAttribute
    -- ^ The attribute to update.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'FacetAttributeUpdate' value with any optional fields omitted.
mkFacetAttributeUpdate
    :: FacetAttributeUpdate
mkFacetAttributeUpdate
  = FacetAttributeUpdate'{action = Core.Nothing,
                          attribute = Core.Nothing}

-- | The action to perform when updating the attribute.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fauAction :: Lens.Lens' FacetAttributeUpdate (Core.Maybe Types.UpdateActionType)
fauAction = Lens.field @"action"
{-# INLINEABLE fauAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The attribute to update.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fauAttribute :: Lens.Lens' FacetAttributeUpdate (Core.Maybe Types.FacetAttribute)
fauAttribute = Lens.field @"attribute"
{-# INLINEABLE fauAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

instance Core.FromJSON FacetAttributeUpdate where
        toJSON FacetAttributeUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("Action" Core..=) Core.<$> action,
                  ("Attribute" Core..=) Core.<$> attribute])
