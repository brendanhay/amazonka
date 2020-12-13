{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkFacetAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkFacetAttributeUpdate
  ( TypedLinkFacetAttributeUpdate (..),

    -- * Smart constructor
    mkTypedLinkFacetAttributeUpdate,

    -- * Lenses
    tlfauAttribute,
    tlfauAction,
  )
where

import Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
import Network.AWS.CloudDirectory.Types.UpdateActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A typed link facet attribute update.
--
-- /See:/ 'mkTypedLinkFacetAttributeUpdate' smart constructor.
data TypedLinkFacetAttributeUpdate = TypedLinkFacetAttributeUpdate'
  { -- | The attribute to update.
    attribute :: TypedLinkAttributeDefinition,
    -- | The action to perform when updating the attribute.
    action :: UpdateActionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypedLinkFacetAttributeUpdate' with the minimum fields required to make a request.
--
-- * 'attribute' - The attribute to update.
-- * 'action' - The action to perform when updating the attribute.
mkTypedLinkFacetAttributeUpdate ::
  -- | 'attribute'
  TypedLinkAttributeDefinition ->
  -- | 'action'
  UpdateActionType ->
  TypedLinkFacetAttributeUpdate
mkTypedLinkFacetAttributeUpdate pAttribute_ pAction_ =
  TypedLinkFacetAttributeUpdate'
    { attribute = pAttribute_,
      action = pAction_
    }

-- | The attribute to update.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfauAttribute :: Lens.Lens' TypedLinkFacetAttributeUpdate TypedLinkAttributeDefinition
tlfauAttribute = Lens.lens (attribute :: TypedLinkFacetAttributeUpdate -> TypedLinkAttributeDefinition) (\s a -> s {attribute = a} :: TypedLinkFacetAttributeUpdate)
{-# DEPRECATED tlfauAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The action to perform when updating the attribute.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlfauAction :: Lens.Lens' TypedLinkFacetAttributeUpdate UpdateActionType
tlfauAction = Lens.lens (action :: TypedLinkFacetAttributeUpdate -> UpdateActionType) (\s a -> s {action = a} :: TypedLinkFacetAttributeUpdate)
{-# DEPRECATED tlfauAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Lude.ToJSON TypedLinkFacetAttributeUpdate where
  toJSON TypedLinkFacetAttributeUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Attribute" Lude..= attribute),
            Lude.Just ("Action" Lude..= action)
          ]
      )
