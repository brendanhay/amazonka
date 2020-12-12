{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.AttributeValue
  ( AttributeValue (..),

    -- * Smart constructor
    mkAttributeValue,

    -- * Lenses
    avSL,
    avSDM,
    avN,
    avS,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Values for use in 'Player' attribute key-value pairs. This object lets you specify an attribute value using any of the valid data types: string, number, string array, or data map. Each @AttributeValue@ object can use only one of the available properties.
--
-- /See:/ 'mkAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { sL :: Lude.Maybe [Lude.Text],
    sDM :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)),
    n :: Lude.Maybe Lude.Double,
    s :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- * 'n' - For number values, expressed as double.
-- * 's' - For single string values. Maximum string length is 100 characters.
-- * 'sDM' - For a map of up to 10 data type:value pairs. Maximum length for each string value is 100 characters.
-- * 'sL' - For a list of up to 10 strings. Maximum length for each string is 100 characters. Duplicate values are not recognized; all occurrences of the repeated value after the first of a repeated value are ignored.
mkAttributeValue ::
  AttributeValue
mkAttributeValue =
  AttributeValue'
    { sL = Lude.Nothing,
      sDM = Lude.Nothing,
      n = Lude.Nothing,
      s = Lude.Nothing
    }

-- | For a list of up to 10 strings. Maximum length for each string is 100 characters. Duplicate values are not recognized; all occurrences of the repeated value after the first of a repeated value are ignored.
--
-- /Note:/ Consider using 'sL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avSL :: Lens.Lens' AttributeValue (Lude.Maybe [Lude.Text])
avSL = Lens.lens (sL :: AttributeValue -> Lude.Maybe [Lude.Text]) (\s a -> s {sL = a} :: AttributeValue)
{-# DEPRECATED avSL "Use generic-lens or generic-optics with 'sL' instead." #-}

-- | For a map of up to 10 data type:value pairs. Maximum length for each string value is 100 characters.
--
-- /Note:/ Consider using 'sDM' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avSDM :: Lens.Lens' AttributeValue (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)))
avSDM = Lens.lens (sDM :: AttributeValue -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))) (\s a -> s {sDM = a} :: AttributeValue)
{-# DEPRECATED avSDM "Use generic-lens or generic-optics with 'sDM' instead." #-}

-- | For number values, expressed as double.
--
-- /Note:/ Consider using 'n' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avN :: Lens.Lens' AttributeValue (Lude.Maybe Lude.Double)
avN = Lens.lens (n :: AttributeValue -> Lude.Maybe Lude.Double) (\s a -> s {n = a} :: AttributeValue)
{-# DEPRECATED avN "Use generic-lens or generic-optics with 'n' instead." #-}

-- | For single string values. Maximum string length is 100 characters.
--
-- /Note:/ Consider using 's' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avS :: Lens.Lens' AttributeValue (Lude.Maybe Lude.Text)
avS = Lens.lens (s :: AttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {s = a} :: AttributeValue)
{-# DEPRECATED avS "Use generic-lens or generic-optics with 's' instead." #-}

instance Lude.FromJSON AttributeValue where
  parseJSON =
    Lude.withObject
      "AttributeValue"
      ( \x ->
          AttributeValue'
            Lude.<$> (x Lude..:? "SL" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SDM" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "N")
            Lude.<*> (x Lude..:? "S")
      )

instance Lude.ToJSON AttributeValue where
  toJSON AttributeValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SL" Lude..=) Lude.<$> sL,
            ("SDM" Lude..=) Lude.<$> sDM,
            ("N" Lude..=) Lude.<$> n,
            ("S" Lude..=) Lude.<$> s
          ]
      )
