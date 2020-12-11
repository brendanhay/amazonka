-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyAttributeTypeDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttributeTypeDescription
  ( PolicyAttributeTypeDescription (..),

    -- * Smart constructor
    mkPolicyAttributeTypeDescription,

    -- * Lenses
    patdAttributeType,
    patdCardinality,
    patdDefaultValue,
    patdAttributeName,
    patdDescription,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a policy attribute type.
--
-- /See:/ 'mkPolicyAttributeTypeDescription' smart constructor.
data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription'
  { attributeType ::
      Lude.Maybe Lude.Text,
    cardinality ::
      Lude.Maybe Lude.Text,
    defaultValue ::
      Lude.Maybe Lude.Text,
    attributeName ::
      Lude.Maybe Lude.Text,
    description ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyAttributeTypeDescription' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the attribute.
-- * 'attributeType' - The type of the attribute. For example, @Boolean@ or @Integer@ .
-- * 'cardinality' - The cardinality of the attribute.
--
-- Valid values:
--
--     * ONE(1) : Single value required
--
--
--     * ZERO_OR_ONE(0..1) : Up to one value is allowed
--
--
--     * ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
--
--
--     * ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
--
--
-- * 'defaultValue' - The default value of the attribute, if applicable.
-- * 'description' - A description of the attribute.
mkPolicyAttributeTypeDescription ::
  PolicyAttributeTypeDescription
mkPolicyAttributeTypeDescription =
  PolicyAttributeTypeDescription'
    { attributeType = Lude.Nothing,
      cardinality = Lude.Nothing,
      defaultValue = Lude.Nothing,
      attributeName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The type of the attribute. For example, @Boolean@ or @Integer@ .
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdAttributeType :: Lens.Lens' PolicyAttributeTypeDescription (Lude.Maybe Lude.Text)
patdAttributeType = Lens.lens (attributeType :: PolicyAttributeTypeDescription -> Lude.Maybe Lude.Text) (\s a -> s {attributeType = a} :: PolicyAttributeTypeDescription)
{-# DEPRECATED patdAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

-- | The cardinality of the attribute.
--
-- Valid values:
--
--     * ONE(1) : Single value required
--
--
--     * ZERO_OR_ONE(0..1) : Up to one value is allowed
--
--
--     * ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
--
--
--     * ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
--
--
--
-- /Note:/ Consider using 'cardinality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdCardinality :: Lens.Lens' PolicyAttributeTypeDescription (Lude.Maybe Lude.Text)
patdCardinality = Lens.lens (cardinality :: PolicyAttributeTypeDescription -> Lude.Maybe Lude.Text) (\s a -> s {cardinality = a} :: PolicyAttributeTypeDescription)
{-# DEPRECATED patdCardinality "Use generic-lens or generic-optics with 'cardinality' instead." #-}

-- | The default value of the attribute, if applicable.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdDefaultValue :: Lens.Lens' PolicyAttributeTypeDescription (Lude.Maybe Lude.Text)
patdDefaultValue = Lens.lens (defaultValue :: PolicyAttributeTypeDescription -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: PolicyAttributeTypeDescription)
{-# DEPRECATED patdDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdAttributeName :: Lens.Lens' PolicyAttributeTypeDescription (Lude.Maybe Lude.Text)
patdAttributeName = Lens.lens (attributeName :: PolicyAttributeTypeDescription -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: PolicyAttributeTypeDescription)
{-# DEPRECATED patdAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | A description of the attribute.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdDescription :: Lens.Lens' PolicyAttributeTypeDescription (Lude.Maybe Lude.Text)
patdDescription = Lens.lens (description :: PolicyAttributeTypeDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PolicyAttributeTypeDescription)
{-# DEPRECATED patdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML PolicyAttributeTypeDescription where
  parseXML x =
    PolicyAttributeTypeDescription'
      Lude.<$> (x Lude..@? "AttributeType")
      Lude.<*> (x Lude..@? "Cardinality")
      Lude.<*> (x Lude..@? "DefaultValue")
      Lude.<*> (x Lude..@? "AttributeName")
      Lude.<*> (x Lude..@? "Description")
