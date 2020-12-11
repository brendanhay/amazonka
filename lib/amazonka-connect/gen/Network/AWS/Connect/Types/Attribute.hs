-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aValue,
    aAttributeType,
  )
where

import Network.AWS.Connect.Types.InstanceAttributeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A toggle for an individual feature at the instance level.
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { value :: Lude.Maybe Lude.Text,
    attributeType :: Lude.Maybe InstanceAttributeType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- * 'attributeType' - The type of attribute.
-- * 'value' - The value of the attribute.
mkAttribute ::
  Attribute
mkAttribute =
  Attribute' {value = Lude.Nothing, attributeType = Lude.Nothing}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute (Lude.Maybe Lude.Text)
aValue = Lens.lens (value :: Attribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Attribute)
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of attribute.
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAttributeType :: Lens.Lens' Attribute (Lude.Maybe InstanceAttributeType)
aAttributeType = Lens.lens (attributeType :: Attribute -> Lude.Maybe InstanceAttributeType) (\s a -> s {attributeType = a} :: Attribute)
{-# DEPRECATED aAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

instance Lude.FromJSON Attribute where
  parseJSON =
    Lude.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "AttributeType")
      )
