-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.MathActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.MathActivity
  ( MathActivity (..),

    -- * Smart constructor
    mkMathActivity,

    -- * Lenses
    maNext,
    maName,
    maAttribute,
    maMath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An activity that computes an arithmetic expression using the message's attributes.
--
-- /See:/ 'mkMathActivity' smart constructor.
data MathActivity = MathActivity'
  { next :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    attribute :: Lude.Text,
    math :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MathActivity' with the minimum fields required to make a request.
--
-- * 'attribute' - The name of the attribute that contains the result of the math operation.
-- * 'math' - An expression that uses one or more existing attributes and must return an integer value.
-- * 'name' - The name of the math activity.
-- * 'next' - The next activity in the pipeline.
mkMathActivity ::
  -- | 'name'
  Lude.Text ->
  -- | 'attribute'
  Lude.Text ->
  -- | 'math'
  Lude.Text ->
  MathActivity
mkMathActivity pName_ pAttribute_ pMath_ =
  MathActivity'
    { next = Lude.Nothing,
      name = pName_,
      attribute = pAttribute_,
      math = pMath_
    }

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maNext :: Lens.Lens' MathActivity (Lude.Maybe Lude.Text)
maNext = Lens.lens (next :: MathActivity -> Lude.Maybe Lude.Text) (\s a -> s {next = a} :: MathActivity)
{-# DEPRECATED maNext "Use generic-lens or generic-optics with 'next' instead." #-}

-- | The name of the math activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maName :: Lens.Lens' MathActivity Lude.Text
maName = Lens.lens (name :: MathActivity -> Lude.Text) (\s a -> s {name = a} :: MathActivity)
{-# DEPRECATED maName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the attribute that contains the result of the math operation.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAttribute :: Lens.Lens' MathActivity Lude.Text
maAttribute = Lens.lens (attribute :: MathActivity -> Lude.Text) (\s a -> s {attribute = a} :: MathActivity)
{-# DEPRECATED maAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | An expression that uses one or more existing attributes and must return an integer value.
--
-- /Note:/ Consider using 'math' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maMath :: Lens.Lens' MathActivity Lude.Text
maMath = Lens.lens (math :: MathActivity -> Lude.Text) (\s a -> s {math = a} :: MathActivity)
{-# DEPRECATED maMath "Use generic-lens or generic-optics with 'math' instead." #-}

instance Lude.FromJSON MathActivity where
  parseJSON =
    Lude.withObject
      "MathActivity"
      ( \x ->
          MathActivity'
            Lude.<$> (x Lude..:? "next")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "attribute")
            Lude.<*> (x Lude..: "math")
      )

instance Lude.ToJSON MathActivity where
  toJSON MathActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("next" Lude..=) Lude.<$> next,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("attribute" Lude..= attribute),
            Lude.Just ("math" Lude..= math)
          ]
      )
