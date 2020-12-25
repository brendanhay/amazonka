{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeAction
  ( ObjectAttributeAction (..),

    -- * Smart constructor
    mkObjectAttributeAction,

    -- * Lenses
    oaaObjectAttributeActionType,
    oaaObjectAttributeUpdateValue,
  )
where

import qualified Network.AWS.CloudDirectory.Types.TypedAttributeValue as Types
import qualified Network.AWS.CloudDirectory.Types.UpdateActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The action to take on the object attribute.
--
-- /See:/ 'mkObjectAttributeAction' smart constructor.
data ObjectAttributeAction = ObjectAttributeAction'
  { -- | A type that can be either @Update@ or @Delete@ .
    objectAttributeActionType :: Core.Maybe Types.UpdateActionType,
    -- | The value that you want to update to.
    objectAttributeUpdateValue :: Core.Maybe Types.TypedAttributeValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ObjectAttributeAction' value with any optional fields omitted.
mkObjectAttributeAction ::
  ObjectAttributeAction
mkObjectAttributeAction =
  ObjectAttributeAction'
    { objectAttributeActionType = Core.Nothing,
      objectAttributeUpdateValue = Core.Nothing
    }

-- | A type that can be either @Update@ or @Delete@ .
--
-- /Note:/ Consider using 'objectAttributeActionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaaObjectAttributeActionType :: Lens.Lens' ObjectAttributeAction (Core.Maybe Types.UpdateActionType)
oaaObjectAttributeActionType = Lens.field @"objectAttributeActionType"
{-# DEPRECATED oaaObjectAttributeActionType "Use generic-lens or generic-optics with 'objectAttributeActionType' instead." #-}

-- | The value that you want to update to.
--
-- /Note:/ Consider using 'objectAttributeUpdateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaaObjectAttributeUpdateValue :: Lens.Lens' ObjectAttributeAction (Core.Maybe Types.TypedAttributeValue)
oaaObjectAttributeUpdateValue = Lens.field @"objectAttributeUpdateValue"
{-# DEPRECATED oaaObjectAttributeUpdateValue "Use generic-lens or generic-optics with 'objectAttributeUpdateValue' instead." #-}

instance Core.FromJSON ObjectAttributeAction where
  toJSON ObjectAttributeAction {..} =
    Core.object
      ( Core.catMaybes
          [ ("ObjectAttributeActionType" Core..=)
              Core.<$> objectAttributeActionType,
            ("ObjectAttributeUpdateValue" Core..=)
              Core.<$> objectAttributeUpdateValue
          ]
      )
