{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.AnnotationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AnnotationValue
  ( AnnotationValue (..),

    -- * Smart constructor
    mkAnnotationValue,

    -- * Lenses
    avNumberValue,
    avStringValue,
    avBooleanValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Value of a segment annotation. Has one of three value types: Number, Boolean, or String.
--
-- /See:/ 'mkAnnotationValue' smart constructor.
data AnnotationValue = AnnotationValue'
  { -- | Value for a Number annotation.
    numberValue :: Lude.Maybe Lude.Double,
    -- | Value for a String annotation.
    stringValue :: Lude.Maybe Lude.Text,
    -- | Value for a Boolean annotation.
    booleanValue :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnnotationValue' with the minimum fields required to make a request.
--
-- * 'numberValue' - Value for a Number annotation.
-- * 'stringValue' - Value for a String annotation.
-- * 'booleanValue' - Value for a Boolean annotation.
mkAnnotationValue ::
  AnnotationValue
mkAnnotationValue =
  AnnotationValue'
    { numberValue = Lude.Nothing,
      stringValue = Lude.Nothing,
      booleanValue = Lude.Nothing
    }

-- | Value for a Number annotation.
--
-- /Note:/ Consider using 'numberValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avNumberValue :: Lens.Lens' AnnotationValue (Lude.Maybe Lude.Double)
avNumberValue = Lens.lens (numberValue :: AnnotationValue -> Lude.Maybe Lude.Double) (\s a -> s {numberValue = a} :: AnnotationValue)
{-# DEPRECATED avNumberValue "Use generic-lens or generic-optics with 'numberValue' instead." #-}

-- | Value for a String annotation.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avStringValue :: Lens.Lens' AnnotationValue (Lude.Maybe Lude.Text)
avStringValue = Lens.lens (stringValue :: AnnotationValue -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: AnnotationValue)
{-# DEPRECATED avStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | Value for a Boolean annotation.
--
-- /Note:/ Consider using 'booleanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avBooleanValue :: Lens.Lens' AnnotationValue (Lude.Maybe Lude.Bool)
avBooleanValue = Lens.lens (booleanValue :: AnnotationValue -> Lude.Maybe Lude.Bool) (\s a -> s {booleanValue = a} :: AnnotationValue)
{-# DEPRECATED avBooleanValue "Use generic-lens or generic-optics with 'booleanValue' instead." #-}

instance Lude.FromJSON AnnotationValue where
  parseJSON =
    Lude.withObject
      "AnnotationValue"
      ( \x ->
          AnnotationValue'
            Lude.<$> (x Lude..:? "NumberValue")
            Lude.<*> (x Lude..:? "StringValue")
            Lude.<*> (x Lude..:? "BooleanValue")
      )
