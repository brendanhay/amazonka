-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DecimalNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DecimalNumber
  ( DecimalNumber (..),

    -- * Smart constructor
    mkDecimalNumber,

    -- * Lenses
    dnUnscaledValue,
    dnScale,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a numeric value in decimal format.
--
-- /See:/ 'mkDecimalNumber' smart constructor.
data DecimalNumber = DecimalNumber'
  { unscaledValue :: Lude.Base64,
    scale :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecimalNumber' with the minimum fields required to make a request.
--
-- * 'scale' - The scale that determines where the decimal point falls in the unscaled value.
-- * 'unscaledValue' - The unscaled numeric value.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
mkDecimalNumber ::
  -- | 'unscaledValue'
  Lude.Base64 ->
  -- | 'scale'
  Lude.Int ->
  DecimalNumber
mkDecimalNumber pUnscaledValue_ pScale_ =
  DecimalNumber' {unscaledValue = pUnscaledValue_, scale = pScale_}

-- | The unscaled numeric value.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'unscaledValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnUnscaledValue :: Lens.Lens' DecimalNumber Lude.Base64
dnUnscaledValue = Lens.lens (unscaledValue :: DecimalNumber -> Lude.Base64) (\s a -> s {unscaledValue = a} :: DecimalNumber)
{-# DEPRECATED dnUnscaledValue "Use generic-lens or generic-optics with 'unscaledValue' instead." #-}

-- | The scale that determines where the decimal point falls in the unscaled value.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnScale :: Lens.Lens' DecimalNumber Lude.Int
dnScale = Lens.lens (scale :: DecimalNumber -> Lude.Int) (\s a -> s {scale = a} :: DecimalNumber)
{-# DEPRECATED dnScale "Use generic-lens or generic-optics with 'scale' instead." #-}

instance Lude.FromJSON DecimalNumber where
  parseJSON =
    Lude.withObject
      "DecimalNumber"
      ( \x ->
          DecimalNumber'
            Lude.<$> (x Lude..: "UnscaledValue") Lude.<*> (x Lude..: "Scale")
      )

instance Lude.ToJSON DecimalNumber where
  toJSON DecimalNumber' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UnscaledValue" Lude..= unscaledValue),
            Lude.Just ("Scale" Lude..= scale)
          ]
      )
