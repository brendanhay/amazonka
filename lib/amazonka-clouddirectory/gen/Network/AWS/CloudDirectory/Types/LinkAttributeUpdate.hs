{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
  ( LinkAttributeUpdate (..)
  -- * Smart constructor
  , mkLinkAttributeUpdate
  -- * Lenses
  , lauAttributeAction
  , lauAttributeKey
  ) where

import qualified Network.AWS.CloudDirectory.Types.AttributeKey as Types
import qualified Network.AWS.CloudDirectory.Types.LinkAttributeAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Structure that contains attribute update information.
--
-- /See:/ 'mkLinkAttributeUpdate' smart constructor.
data LinkAttributeUpdate = LinkAttributeUpdate'
  { attributeAction :: Core.Maybe Types.LinkAttributeAction
    -- ^ The action to perform as part of the attribute update.
  , attributeKey :: Core.Maybe Types.AttributeKey
    -- ^ The key of the attribute being updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LinkAttributeUpdate' value with any optional fields omitted.
mkLinkAttributeUpdate
    :: LinkAttributeUpdate
mkLinkAttributeUpdate
  = LinkAttributeUpdate'{attributeAction = Core.Nothing,
                         attributeKey = Core.Nothing}

-- | The action to perform as part of the attribute update.
--
-- /Note:/ Consider using 'attributeAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauAttributeAction :: Lens.Lens' LinkAttributeUpdate (Core.Maybe Types.LinkAttributeAction)
lauAttributeAction = Lens.field @"attributeAction"
{-# INLINEABLE lauAttributeAction #-}
{-# DEPRECATED attributeAction "Use generic-lens or generic-optics with 'attributeAction' instead"  #-}

-- | The key of the attribute being updated.
--
-- /Note:/ Consider using 'attributeKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauAttributeKey :: Lens.Lens' LinkAttributeUpdate (Core.Maybe Types.AttributeKey)
lauAttributeKey = Lens.field @"attributeKey"
{-# INLINEABLE lauAttributeKey #-}
{-# DEPRECATED attributeKey "Use generic-lens or generic-optics with 'attributeKey' instead"  #-}

instance Core.FromJSON LinkAttributeUpdate where
        toJSON LinkAttributeUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("AttributeAction" Core..=) Core.<$> attributeAction,
                  ("AttributeKey" Core..=) Core.<$> attributeKey])
