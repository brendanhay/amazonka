{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.LinkAttributeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.LinkAttributeAction
  ( LinkAttributeAction (..)
  -- * Smart constructor
  , mkLinkAttributeAction
  -- * Lenses
  , laaAttributeActionType
  , laaAttributeUpdateValue
  ) where

import qualified Network.AWS.CloudDirectory.Types.TypedAttributeValue as Types
import qualified Network.AWS.CloudDirectory.Types.UpdateActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The action to take on a typed link attribute value. Updates are only supported for attributes which don’t contribute to link identity.
--
-- /See:/ 'mkLinkAttributeAction' smart constructor.
data LinkAttributeAction = LinkAttributeAction'
  { attributeActionType :: Core.Maybe Types.UpdateActionType
    -- ^ A type that can be either @UPDATE_OR_CREATE@ or @DELETE@ .
  , attributeUpdateValue :: Core.Maybe Types.TypedAttributeValue
    -- ^ The value that you want to update to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LinkAttributeAction' value with any optional fields omitted.
mkLinkAttributeAction
    :: LinkAttributeAction
mkLinkAttributeAction
  = LinkAttributeAction'{attributeActionType = Core.Nothing,
                         attributeUpdateValue = Core.Nothing}

-- | A type that can be either @UPDATE_OR_CREATE@ or @DELETE@ .
--
-- /Note:/ Consider using 'attributeActionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laaAttributeActionType :: Lens.Lens' LinkAttributeAction (Core.Maybe Types.UpdateActionType)
laaAttributeActionType = Lens.field @"attributeActionType"
{-# INLINEABLE laaAttributeActionType #-}
{-# DEPRECATED attributeActionType "Use generic-lens or generic-optics with 'attributeActionType' instead"  #-}

-- | The value that you want to update to.
--
-- /Note:/ Consider using 'attributeUpdateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laaAttributeUpdateValue :: Lens.Lens' LinkAttributeAction (Core.Maybe Types.TypedAttributeValue)
laaAttributeUpdateValue = Lens.field @"attributeUpdateValue"
{-# INLINEABLE laaAttributeUpdateValue #-}
{-# DEPRECATED attributeUpdateValue "Use generic-lens or generic-optics with 'attributeUpdateValue' instead"  #-}

instance Core.FromJSON LinkAttributeAction where
        toJSON LinkAttributeAction{..}
          = Core.object
              (Core.catMaybes
                 [("AttributeActionType" Core..=) Core.<$> attributeActionType,
                  ("AttributeUpdateValue" Core..=) Core.<$> attributeUpdateValue])
