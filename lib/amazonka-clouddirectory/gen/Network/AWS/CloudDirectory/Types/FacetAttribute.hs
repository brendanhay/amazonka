-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttribute
  ( FacetAttribute (..),

    -- * Smart constructor
    mkFacetAttribute,

    -- * Lenses
    faAttributeReference,
    faAttributeDefinition,
    faRequiredBehavior,
    faName,
  )
where

import Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
import Network.AWS.CloudDirectory.Types.FacetAttributeReference
import Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An attribute that is associated with the 'Facet' .
--
-- /See:/ 'mkFacetAttribute' smart constructor.
data FacetAttribute = FacetAttribute'
  { attributeReference ::
      Lude.Maybe FacetAttributeReference,
    attributeDefinition :: Lude.Maybe FacetAttributeDefinition,
    requiredBehavior :: Lude.Maybe RequiredAttributeBehavior,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FacetAttribute' with the minimum fields required to make a request.
--
-- * 'attributeDefinition' - A facet attribute consists of either a definition or a reference. This structure contains the attribute definition. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
-- * 'attributeReference' - An attribute reference that is associated with the attribute. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
-- * 'name' - The name of the facet attribute.
-- * 'requiredBehavior' - The required behavior of the @FacetAttribute@ .
mkFacetAttribute ::
  -- | 'name'
  Lude.Text ->
  FacetAttribute
mkFacetAttribute pName_ =
  FacetAttribute'
    { attributeReference = Lude.Nothing,
      attributeDefinition = Lude.Nothing,
      requiredBehavior = Lude.Nothing,
      name = pName_
    }

-- | An attribute reference that is associated with the attribute. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /Note:/ Consider using 'attributeReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faAttributeReference :: Lens.Lens' FacetAttribute (Lude.Maybe FacetAttributeReference)
faAttributeReference = Lens.lens (attributeReference :: FacetAttribute -> Lude.Maybe FacetAttributeReference) (\s a -> s {attributeReference = a} :: FacetAttribute)
{-# DEPRECATED faAttributeReference "Use generic-lens or generic-optics with 'attributeReference' instead." #-}

-- | A facet attribute consists of either a definition or a reference. This structure contains the attribute definition. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /Note:/ Consider using 'attributeDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faAttributeDefinition :: Lens.Lens' FacetAttribute (Lude.Maybe FacetAttributeDefinition)
faAttributeDefinition = Lens.lens (attributeDefinition :: FacetAttribute -> Lude.Maybe FacetAttributeDefinition) (\s a -> s {attributeDefinition = a} :: FacetAttribute)
{-# DEPRECATED faAttributeDefinition "Use generic-lens or generic-optics with 'attributeDefinition' instead." #-}

-- | The required behavior of the @FacetAttribute@ .
--
-- /Note:/ Consider using 'requiredBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faRequiredBehavior :: Lens.Lens' FacetAttribute (Lude.Maybe RequiredAttributeBehavior)
faRequiredBehavior = Lens.lens (requiredBehavior :: FacetAttribute -> Lude.Maybe RequiredAttributeBehavior) (\s a -> s {requiredBehavior = a} :: FacetAttribute)
{-# DEPRECATED faRequiredBehavior "Use generic-lens or generic-optics with 'requiredBehavior' instead." #-}

-- | The name of the facet attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faName :: Lens.Lens' FacetAttribute Lude.Text
faName = Lens.lens (name :: FacetAttribute -> Lude.Text) (\s a -> s {name = a} :: FacetAttribute)
{-# DEPRECATED faName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON FacetAttribute where
  parseJSON =
    Lude.withObject
      "FacetAttribute"
      ( \x ->
          FacetAttribute'
            Lude.<$> (x Lude..:? "AttributeReference")
            Lude.<*> (x Lude..:? "AttributeDefinition")
            Lude.<*> (x Lude..:? "RequiredBehavior")
            Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON FacetAttribute where
  toJSON FacetAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AttributeReference" Lude..=) Lude.<$> attributeReference,
            ("AttributeDefinition" Lude..=) Lude.<$> attributeDefinition,
            ("RequiredBehavior" Lude..=) Lude.<$> requiredBehavior,
            Lude.Just ("Name" Lude..= name)
          ]
      )
