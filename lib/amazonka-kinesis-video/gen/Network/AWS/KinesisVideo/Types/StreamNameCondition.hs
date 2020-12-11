-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.StreamNameCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.StreamNameCondition
  ( StreamNameCondition (..),

    -- * Smart constructor
    mkStreamNameCondition,

    -- * Lenses
    sncComparisonOperator,
    sncComparisonValue,
  )
where

import Network.AWS.KinesisVideo.Types.ComparisonOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the condition that streams must satisfy to be returned when you list streams (see the @ListStreams@ API). A condition has a comparison operation and a value. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
--
-- /See:/ 'mkStreamNameCondition' smart constructor.
data StreamNameCondition = StreamNameCondition'
  { comparisonOperator ::
      Lude.Maybe ComparisonOperator,
    comparisonValue :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamNameCondition' with the minimum fields required to make a request.
--
-- * 'comparisonOperator' - A comparison operator. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
-- * 'comparisonValue' - A value to compare.
mkStreamNameCondition ::
  StreamNameCondition
mkStreamNameCondition =
  StreamNameCondition'
    { comparisonOperator = Lude.Nothing,
      comparisonValue = Lude.Nothing
    }

-- | A comparison operator. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sncComparisonOperator :: Lens.Lens' StreamNameCondition (Lude.Maybe ComparisonOperator)
sncComparisonOperator = Lens.lens (comparisonOperator :: StreamNameCondition -> Lude.Maybe ComparisonOperator) (\s a -> s {comparisonOperator = a} :: StreamNameCondition)
{-# DEPRECATED sncComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | A value to compare.
--
-- /Note:/ Consider using 'comparisonValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sncComparisonValue :: Lens.Lens' StreamNameCondition (Lude.Maybe Lude.Text)
sncComparisonValue = Lens.lens (comparisonValue :: StreamNameCondition -> Lude.Maybe Lude.Text) (\s a -> s {comparisonValue = a} :: StreamNameCondition)
{-# DEPRECATED sncComparisonValue "Use generic-lens or generic-optics with 'comparisonValue' instead." #-}

instance Lude.ToJSON StreamNameCondition where
  toJSON StreamNameCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ComparisonOperator" Lude..=) Lude.<$> comparisonOperator,
            ("ComparisonValue" Lude..=) Lude.<$> comparisonValue
          ]
      )
