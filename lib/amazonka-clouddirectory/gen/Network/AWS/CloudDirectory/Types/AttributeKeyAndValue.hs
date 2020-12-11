-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
  ( AttributeKeyAndValue (..),

    -- * Smart constructor
    mkAttributeKeyAndValue,

    -- * Lenses
    akavKey,
    akavValue,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The combination of an attribute key and an attribute value.
--
-- /See:/ 'mkAttributeKeyAndValue' smart constructor.
data AttributeKeyAndValue = AttributeKeyAndValue'
  { key ::
      AttributeKey,
    value :: TypedAttributeValue
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributeKeyAndValue' with the minimum fields required to make a request.
--
-- * 'key' - The key of the attribute.
-- * 'value' - The value of the attribute.
mkAttributeKeyAndValue ::
  -- | 'key'
  AttributeKey ->
  -- | 'value'
  TypedAttributeValue ->
  AttributeKeyAndValue
mkAttributeKeyAndValue pKey_ pValue_ =
  AttributeKeyAndValue' {key = pKey_, value = pValue_}

-- | The key of the attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akavKey :: Lens.Lens' AttributeKeyAndValue AttributeKey
akavKey = Lens.lens (key :: AttributeKeyAndValue -> AttributeKey) (\s a -> s {key = a} :: AttributeKeyAndValue)
{-# DEPRECATED akavKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akavValue :: Lens.Lens' AttributeKeyAndValue TypedAttributeValue
akavValue = Lens.lens (value :: AttributeKeyAndValue -> TypedAttributeValue) (\s a -> s {value = a} :: AttributeKeyAndValue)
{-# DEPRECATED akavValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON AttributeKeyAndValue where
  parseJSON =
    Lude.withObject
      "AttributeKeyAndValue"
      ( \x ->
          AttributeKeyAndValue'
            Lude.<$> (x Lude..: "Key") Lude.<*> (x Lude..: "Value")
      )

instance Lude.ToJSON AttributeKeyAndValue where
  toJSON AttributeKeyAndValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Key" Lude..= key), Lude.Just ("Value" Lude..= value)]
      )
