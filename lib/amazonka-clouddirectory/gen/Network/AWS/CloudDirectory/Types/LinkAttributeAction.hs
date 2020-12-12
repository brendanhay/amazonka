{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.LinkAttributeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.LinkAttributeAction
  ( LinkAttributeAction (..),

    -- * Smart constructor
    mkLinkAttributeAction,

    -- * Lenses
    laaAttributeActionType,
    laaAttributeUpdateValue,
  )
where

import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.CloudDirectory.Types.UpdateActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The action to take on a typed link attribute value. Updates are only supported for attributes which don’t contribute to link identity.
--
-- /See:/ 'mkLinkAttributeAction' smart constructor.
data LinkAttributeAction = LinkAttributeAction'
  { attributeActionType ::
      Lude.Maybe UpdateActionType,
    attributeUpdateValue ::
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

-- | Creates a value of 'LinkAttributeAction' with the minimum fields required to make a request.
--
-- * 'attributeActionType' - A type that can be either @UPDATE_OR_CREATE@ or @DELETE@ .
-- * 'attributeUpdateValue' - The value that you want to update to.
mkLinkAttributeAction ::
  LinkAttributeAction
mkLinkAttributeAction =
  LinkAttributeAction'
    { attributeActionType = Lude.Nothing,
      attributeUpdateValue = Lude.Nothing
    }

-- | A type that can be either @UPDATE_OR_CREATE@ or @DELETE@ .
--
-- /Note:/ Consider using 'attributeActionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laaAttributeActionType :: Lens.Lens' LinkAttributeAction (Lude.Maybe UpdateActionType)
laaAttributeActionType = Lens.lens (attributeActionType :: LinkAttributeAction -> Lude.Maybe UpdateActionType) (\s a -> s {attributeActionType = a} :: LinkAttributeAction)
{-# DEPRECATED laaAttributeActionType "Use generic-lens or generic-optics with 'attributeActionType' instead." #-}

-- | The value that you want to update to.
--
-- /Note:/ Consider using 'attributeUpdateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laaAttributeUpdateValue :: Lens.Lens' LinkAttributeAction (Lude.Maybe TypedAttributeValue)
laaAttributeUpdateValue = Lens.lens (attributeUpdateValue :: LinkAttributeAction -> Lude.Maybe TypedAttributeValue) (\s a -> s {attributeUpdateValue = a} :: LinkAttributeAction)
{-# DEPRECATED laaAttributeUpdateValue "Use generic-lens or generic-optics with 'attributeUpdateValue' instead." #-}

instance Lude.ToJSON LinkAttributeAction where
  toJSON LinkAttributeAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AttributeActionType" Lude..=) Lude.<$> attributeActionType,
            ("AttributeUpdateValue" Lude..=) Lude.<$> attributeUpdateValue
          ]
      )
