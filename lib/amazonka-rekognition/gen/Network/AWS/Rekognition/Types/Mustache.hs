{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Mustache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Mustache
  ( Mustache (..),

    -- * Smart constructor
    mkMustache,

    -- * Lenses
    mValue,
    mConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
--
-- /See:/ 'mkMustache' smart constructor.
data Mustache = Mustache'
  { value :: Lude.Maybe Lude.Bool,
    confidence :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Mustache' with the minimum fields required to make a request.
--
-- * 'confidence' - Level of confidence in the determination.
-- * 'value' - Boolean value that indicates whether the face has mustache or not.
mkMustache ::
  Mustache
mkMustache =
  Mustache' {value = Lude.Nothing, confidence = Lude.Nothing}

-- | Boolean value that indicates whether the face has mustache or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mValue :: Lens.Lens' Mustache (Lude.Maybe Lude.Bool)
mValue = Lens.lens (value :: Mustache -> Lude.Maybe Lude.Bool) (\s a -> s {value = a} :: Mustache)
{-# DEPRECATED mValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mConfidence :: Lens.Lens' Mustache (Lude.Maybe Lude.Double)
mConfidence = Lens.lens (confidence :: Mustache -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Mustache)
{-# DEPRECATED mConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON Mustache where
  parseJSON =
    Lude.withObject
      "Mustache"
      ( \x ->
          Mustache'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Confidence")
      )
