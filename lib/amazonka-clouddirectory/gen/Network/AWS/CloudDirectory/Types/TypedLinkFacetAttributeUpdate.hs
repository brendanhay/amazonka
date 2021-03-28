{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkFacetAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.TypedLinkFacetAttributeUpdate
  ( TypedLinkFacetAttributeUpdate (..)
  -- * Smart constructor
  , mkTypedLinkFacetAttributeUpdate
  -- * Lenses
  , tlfauAttribute
  , tlfauAction
  ) where

import qualified Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition as Types
import qualified Network.AWS.CloudDirectory.Types.UpdateActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A typed link facet attribute update.
--
-- /See:/ 'mkTypedLinkFacetAttributeUpdate' smart constructor.
data TypedLinkFacetAttributeUpdate = TypedLinkFacetAttributeUpdate'
  { attribute :: Types.TypedLinkAttributeDefinition
    -- ^ The attribute to update.
  , action :: Types.UpdateActionType
    -- ^ The action to perform when updating the attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TypedLinkFacetAttributeUpdate' value with any optional fields omitted.
mkTypedLinkFacetAttributeUpdate
    :: Types.TypedLinkAttributeDefinition -- ^ 'attribute'
    -> Types.UpdateActionType -- ^ 'action'
    -> TypedLinkFacetAttributeUpdate
mkTypedLinkFacetAttributeUpdate attribute action
  = TypedLinkFacetAttributeUpdate'{attribute, action}

-- | The attribute to update.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfauAttribute :: Lens.Lens' TypedLinkFacetAttributeUpdate Types.TypedLinkAttributeDefinition
tlfauAttribute = Lens.field @"attribute"
{-# INLINEABLE tlfauAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The action to perform when updating the attribute.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfauAction :: Lens.Lens' TypedLinkFacetAttributeUpdate Types.UpdateActionType
tlfauAction = Lens.field @"action"
{-# INLINEABLE tlfauAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

instance Core.FromJSON TypedLinkFacetAttributeUpdate where
        toJSON TypedLinkFacetAttributeUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Attribute" Core..= attribute),
                  Core.Just ("Action" Core..= action)])
