-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
  ( NumberAttributeConstraintsType (..),

    -- * Smart constructor
    mkNumberAttributeConstraintsType,

    -- * Lenses
    nactMaxValue,
    nactMinValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The minimum and maximum value of an attribute that is of the number data type.
--
-- /See:/ 'mkNumberAttributeConstraintsType' smart constructor.
data NumberAttributeConstraintsType = NumberAttributeConstraintsType'
  { maxValue ::
      Lude.Maybe Lude.Text,
    minValue ::
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

-- | Creates a value of 'NumberAttributeConstraintsType' with the minimum fields required to make a request.
--
-- * 'maxValue' - The maximum value of an attribute that is of the number data type.
-- * 'minValue' - The minimum value of an attribute that is of the number data type.
mkNumberAttributeConstraintsType ::
  NumberAttributeConstraintsType
mkNumberAttributeConstraintsType =
  NumberAttributeConstraintsType'
    { maxValue = Lude.Nothing,
      minValue = Lude.Nothing
    }

-- | The maximum value of an attribute that is of the number data type.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nactMaxValue :: Lens.Lens' NumberAttributeConstraintsType (Lude.Maybe Lude.Text)
nactMaxValue = Lens.lens (maxValue :: NumberAttributeConstraintsType -> Lude.Maybe Lude.Text) (\s a -> s {maxValue = a} :: NumberAttributeConstraintsType)
{-# DEPRECATED nactMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

-- | The minimum value of an attribute that is of the number data type.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nactMinValue :: Lens.Lens' NumberAttributeConstraintsType (Lude.Maybe Lude.Text)
nactMinValue = Lens.lens (minValue :: NumberAttributeConstraintsType -> Lude.Maybe Lude.Text) (\s a -> s {minValue = a} :: NumberAttributeConstraintsType)
{-# DEPRECATED nactMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

instance Lude.FromJSON NumberAttributeConstraintsType where
  parseJSON =
    Lude.withObject
      "NumberAttributeConstraintsType"
      ( \x ->
          NumberAttributeConstraintsType'
            Lude.<$> (x Lude..:? "MaxValue") Lude.<*> (x Lude..:? "MinValue")
      )

instance Lude.ToJSON NumberAttributeConstraintsType where
  toJSON NumberAttributeConstraintsType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxValue" Lude..=) Lude.<$> maxValue,
            ("MinValue" Lude..=) Lude.<$> minValue
          ]
      )
