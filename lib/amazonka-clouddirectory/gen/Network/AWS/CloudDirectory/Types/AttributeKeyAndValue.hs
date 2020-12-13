{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    akavValue,
    akavKey,
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
  { -- | The value of the attribute.
    value :: TypedAttributeValue,
    -- | The key of the attribute.
    key :: AttributeKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributeKeyAndValue' with the minimum fields required to make a request.
--
-- * 'value' - The value of the attribute.
-- * 'key' - The key of the attribute.
mkAttributeKeyAndValue ::
  -- | 'value'
  TypedAttributeValue ->
  -- | 'key'
  AttributeKey ->
  AttributeKeyAndValue
mkAttributeKeyAndValue pValue_ pKey_ =
  AttributeKeyAndValue' {value = pValue_, key = pKey_}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akavValue :: Lens.Lens' AttributeKeyAndValue TypedAttributeValue
akavValue = Lens.lens (value :: AttributeKeyAndValue -> TypedAttributeValue) (\s a -> s {value = a} :: AttributeKeyAndValue)
{-# DEPRECATED akavValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key of the attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akavKey :: Lens.Lens' AttributeKeyAndValue AttributeKey
akavKey = Lens.lens (key :: AttributeKeyAndValue -> AttributeKey) (\s a -> s {key = a} :: AttributeKeyAndValue)
{-# DEPRECATED akavKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON AttributeKeyAndValue where
  parseJSON =
    Lude.withObject
      "AttributeKeyAndValue"
      ( \x ->
          AttributeKeyAndValue'
            Lude.<$> (x Lude..: "Value") Lude.<*> (x Lude..: "Key")
      )

instance Lude.ToJSON AttributeKeyAndValue where
  toJSON AttributeKeyAndValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Value" Lude..= value), Lude.Just ("Key" Lude..= key)]
      )
