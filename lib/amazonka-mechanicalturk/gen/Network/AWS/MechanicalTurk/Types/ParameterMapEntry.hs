{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ParameterMapEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ParameterMapEntry
  ( ParameterMapEntry (..),

    -- * Smart constructor
    mkParameterMapEntry,

    -- * Lenses
    pmeValues,
    pmeKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data structure is the data type for the AnswerKey parameter of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
--
-- /See:/ 'mkParameterMapEntry' smart constructor.
data ParameterMapEntry = ParameterMapEntry'
  { -- | The list of answers to the question specified in the MapEntry Key element. The Worker must match all values in order for the answer to be scored correctly.
    values :: Lude.Maybe [Lude.Text],
    -- | The QuestionID from the HIT that is used to identify which question requires Mechanical Turk to score as part of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterMapEntry' with the minimum fields required to make a request.
--
-- * 'values' - The list of answers to the question specified in the MapEntry Key element. The Worker must match all values in order for the answer to be scored correctly.
-- * 'key' - The QuestionID from the HIT that is used to identify which question requires Mechanical Turk to score as part of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
mkParameterMapEntry ::
  ParameterMapEntry
mkParameterMapEntry =
  ParameterMapEntry' {values = Lude.Nothing, key = Lude.Nothing}

-- | The list of answers to the question specified in the MapEntry Key element. The Worker must match all values in order for the answer to be scored correctly.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmeValues :: Lens.Lens' ParameterMapEntry (Lude.Maybe [Lude.Text])
pmeValues = Lens.lens (values :: ParameterMapEntry -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: ParameterMapEntry)
{-# DEPRECATED pmeValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The QuestionID from the HIT that is used to identify which question requires Mechanical Turk to score as part of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmeKey :: Lens.Lens' ParameterMapEntry (Lude.Maybe Lude.Text)
pmeKey = Lens.lens (key :: ParameterMapEntry -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: ParameterMapEntry)
{-# DEPRECATED pmeKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON ParameterMapEntry where
  parseJSON =
    Lude.withObject
      "ParameterMapEntry"
      ( \x ->
          ParameterMapEntry'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Key")
      )

instance Lude.ToJSON ParameterMapEntry where
  toJSON ParameterMapEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Values" Lude..=) Lude.<$> values, ("Key" Lude..=) Lude.<$> key]
      )
