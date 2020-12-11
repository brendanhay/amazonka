-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeUpdate
  ( FacetAttributeUpdate (..),

    -- * Smart constructor
    mkFacetAttributeUpdate,

    -- * Lenses
    fauAttribute,
    fauAction,
  )
where

import Network.AWS.CloudDirectory.Types.FacetAttribute
import Network.AWS.CloudDirectory.Types.UpdateActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that contains information used to update an attribute.
--
-- /See:/ 'mkFacetAttributeUpdate' smart constructor.
data FacetAttributeUpdate = FacetAttributeUpdate'
  { attribute ::
      Lude.Maybe FacetAttribute,
    action :: Lude.Maybe UpdateActionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FacetAttributeUpdate' with the minimum fields required to make a request.
--
-- * 'action' - The action to perform when updating the attribute.
-- * 'attribute' - The attribute to update.
mkFacetAttributeUpdate ::
  FacetAttributeUpdate
mkFacetAttributeUpdate =
  FacetAttributeUpdate'
    { attribute = Lude.Nothing,
      action = Lude.Nothing
    }

-- | The attribute to update.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fauAttribute :: Lens.Lens' FacetAttributeUpdate (Lude.Maybe FacetAttribute)
fauAttribute = Lens.lens (attribute :: FacetAttributeUpdate -> Lude.Maybe FacetAttribute) (\s a -> s {attribute = a} :: FacetAttributeUpdate)
{-# DEPRECATED fauAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The action to perform when updating the attribute.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fauAction :: Lens.Lens' FacetAttributeUpdate (Lude.Maybe UpdateActionType)
fauAction = Lens.lens (action :: FacetAttributeUpdate -> Lude.Maybe UpdateActionType) (\s a -> s {action = a} :: FacetAttributeUpdate)
{-# DEPRECATED fauAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Lude.ToJSON FacetAttributeUpdate where
  toJSON FacetAttributeUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Attribute" Lude..=) Lude.<$> attribute,
            ("Action" Lude..=) Lude.<$> action
          ]
      )
