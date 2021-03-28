{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
  ( ObjectAttributeUpdate (..)
  -- * Smart constructor
  , mkObjectAttributeUpdate
  -- * Lenses
  , oauObjectAttributeAction
  , oauObjectAttributeKey
  ) where

import qualified Network.AWS.CloudDirectory.Types.AttributeKey as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectAttributeAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Structure that contains attribute update information.
--
-- /See:/ 'mkObjectAttributeUpdate' smart constructor.
data ObjectAttributeUpdate = ObjectAttributeUpdate'
  { objectAttributeAction :: Core.Maybe Types.ObjectAttributeAction
    -- ^ The action to perform as part of the attribute update.
  , objectAttributeKey :: Core.Maybe Types.AttributeKey
    -- ^ The key of the attribute being updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ObjectAttributeUpdate' value with any optional fields omitted.
mkObjectAttributeUpdate
    :: ObjectAttributeUpdate
mkObjectAttributeUpdate
  = ObjectAttributeUpdate'{objectAttributeAction = Core.Nothing,
                           objectAttributeKey = Core.Nothing}

-- | The action to perform as part of the attribute update.
--
-- /Note:/ Consider using 'objectAttributeAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oauObjectAttributeAction :: Lens.Lens' ObjectAttributeUpdate (Core.Maybe Types.ObjectAttributeAction)
oauObjectAttributeAction = Lens.field @"objectAttributeAction"
{-# INLINEABLE oauObjectAttributeAction #-}
{-# DEPRECATED objectAttributeAction "Use generic-lens or generic-optics with 'objectAttributeAction' instead"  #-}

-- | The key of the attribute being updated.
--
-- /Note:/ Consider using 'objectAttributeKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oauObjectAttributeKey :: Lens.Lens' ObjectAttributeUpdate (Core.Maybe Types.AttributeKey)
oauObjectAttributeKey = Lens.field @"objectAttributeKey"
{-# INLINEABLE oauObjectAttributeKey #-}
{-# DEPRECATED objectAttributeKey "Use generic-lens or generic-optics with 'objectAttributeKey' instead"  #-}

instance Core.FromJSON ObjectAttributeUpdate where
        toJSON ObjectAttributeUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("ObjectAttributeAction" Core..=) Core.<$> objectAttributeAction,
                  ("ObjectAttributeKey" Core..=) Core.<$> objectAttributeKey])
