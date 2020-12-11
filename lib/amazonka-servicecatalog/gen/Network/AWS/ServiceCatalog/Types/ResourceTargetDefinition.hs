-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
  ( ResourceTargetDefinition (..),

    -- * Smart constructor
    mkResourceTargetDefinition,

    -- * Lenses
    rtdAttribute,
    rtdRequiresRecreation,
    rtdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.RequiresRecreation
import Network.AWS.ServiceCatalog.Types.ResourceAttribute

-- | Information about a change to a resource attribute.
--
-- /See:/ 'mkResourceTargetDefinition' smart constructor.
data ResourceTargetDefinition = ResourceTargetDefinition'
  { attribute ::
      Lude.Maybe ResourceAttribute,
    requiresRecreation ::
      Lude.Maybe RequiresRecreation,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceTargetDefinition' with the minimum fields required to make a request.
--
-- * 'attribute' - The attribute to be changed.
-- * 'name' - If the attribute is @Properties@ , the value is the name of the property. Otherwise, the value is null.
-- * 'requiresRecreation' - If the attribute is @Properties@ , indicates whether a change to this property causes the resource to be re-created.
mkResourceTargetDefinition ::
  ResourceTargetDefinition
mkResourceTargetDefinition =
  ResourceTargetDefinition'
    { attribute = Lude.Nothing,
      requiresRecreation = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The attribute to be changed.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdAttribute :: Lens.Lens' ResourceTargetDefinition (Lude.Maybe ResourceAttribute)
rtdAttribute = Lens.lens (attribute :: ResourceTargetDefinition -> Lude.Maybe ResourceAttribute) (\s a -> s {attribute = a} :: ResourceTargetDefinition)
{-# DEPRECATED rtdAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | If the attribute is @Properties@ , indicates whether a change to this property causes the resource to be re-created.
--
-- /Note:/ Consider using 'requiresRecreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdRequiresRecreation :: Lens.Lens' ResourceTargetDefinition (Lude.Maybe RequiresRecreation)
rtdRequiresRecreation = Lens.lens (requiresRecreation :: ResourceTargetDefinition -> Lude.Maybe RequiresRecreation) (\s a -> s {requiresRecreation = a} :: ResourceTargetDefinition)
{-# DEPRECATED rtdRequiresRecreation "Use generic-lens or generic-optics with 'requiresRecreation' instead." #-}

-- | If the attribute is @Properties@ , the value is the name of the property. Otherwise, the value is null.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdName :: Lens.Lens' ResourceTargetDefinition (Lude.Maybe Lude.Text)
rtdName = Lens.lens (name :: ResourceTargetDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ResourceTargetDefinition)
{-# DEPRECATED rtdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ResourceTargetDefinition where
  parseJSON =
    Lude.withObject
      "ResourceTargetDefinition"
      ( \x ->
          ResourceTargetDefinition'
            Lude.<$> (x Lude..:? "Attribute")
            Lude.<*> (x Lude..:? "RequiresRecreation")
            Lude.<*> (x Lude..:? "Name")
      )
