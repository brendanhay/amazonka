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

import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.CloudDirectory.Types.UpdateActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The action to take on the object attribute.
--
-- /See:/ 'mkObjectAttributeAction' smart constructor.
data ObjectAttributeAction = ObjectAttributeAction'
  { objectAttributeActionType ::
      Lude.Maybe UpdateActionType,
    objectAttributeUpdateValue ::
      Lude.Maybe TypedAttributeValue
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectAttributeAction' with the minimum fields required to make a request.
--
-- * 'objectAttributeActionType' - A type that can be either @Update@ or @Delete@ .
-- * 'objectAttributeUpdateValue' - The value that you want to update to.
mkObjectAttributeAction ::
  ObjectAttributeAction
mkObjectAttributeAction =
  ObjectAttributeAction'
    { objectAttributeActionType = Lude.Nothing,
      objectAttributeUpdateValue = Lude.Nothing
    }

-- | A type that can be either @Update@ or @Delete@ .
--
-- /Note:/ Consider using 'objectAttributeActionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaaObjectAttributeActionType :: Lens.Lens' ObjectAttributeAction (Lude.Maybe UpdateActionType)
oaaObjectAttributeActionType = Lens.lens (objectAttributeActionType :: ObjectAttributeAction -> Lude.Maybe UpdateActionType) (\s a -> s {objectAttributeActionType = a} :: ObjectAttributeAction)
{-# DEPRECATED oaaObjectAttributeActionType "Use generic-lens or generic-optics with 'objectAttributeActionType' instead." #-}

-- | The value that you want to update to.
--
-- /Note:/ Consider using 'objectAttributeUpdateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaaObjectAttributeUpdateValue :: Lens.Lens' ObjectAttributeAction (Lude.Maybe TypedAttributeValue)
oaaObjectAttributeUpdateValue = Lens.lens (objectAttributeUpdateValue :: ObjectAttributeAction -> Lude.Maybe TypedAttributeValue) (\s a -> s {objectAttributeUpdateValue = a} :: ObjectAttributeAction)
{-# DEPRECATED oaaObjectAttributeUpdateValue "Use generic-lens or generic-optics with 'objectAttributeUpdateValue' instead." #-}

instance Lude.ToJSON ObjectAttributeAction where
  toJSON ObjectAttributeAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ObjectAttributeActionType" Lude..=)
              Lude.<$> objectAttributeActionType,
            ("ObjectAttributeUpdateValue" Lude..=)
              Lude.<$> objectAttributeUpdateValue
          ]
      )
