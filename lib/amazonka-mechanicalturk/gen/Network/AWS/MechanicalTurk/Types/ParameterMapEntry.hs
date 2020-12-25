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
    pmeKey,
    pmeValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | This data structure is the data type for the AnswerKey parameter of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
--
-- /See:/ 'mkParameterMapEntry' smart constructor.
data ParameterMapEntry = ParameterMapEntry'
  { -- | The QuestionID from the HIT that is used to identify which question requires Mechanical Turk to score as part of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
    key :: Core.Maybe Types.String,
    -- | The list of answers to the question specified in the MapEntry Key element. The Worker must match all values in order for the answer to be scored correctly.
    values :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterMapEntry' value with any optional fields omitted.
mkParameterMapEntry ::
  ParameterMapEntry
mkParameterMapEntry =
  ParameterMapEntry' {key = Core.Nothing, values = Core.Nothing}

-- | The QuestionID from the HIT that is used to identify which question requires Mechanical Turk to score as part of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmeKey :: Lens.Lens' ParameterMapEntry (Core.Maybe Types.String)
pmeKey = Lens.field @"key"
{-# DEPRECATED pmeKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The list of answers to the question specified in the MapEntry Key element. The Worker must match all values in order for the answer to be scored correctly.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmeValues :: Lens.Lens' ParameterMapEntry (Core.Maybe [Types.String])
pmeValues = Lens.field @"values"
{-# DEPRECATED pmeValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON ParameterMapEntry where
  toJSON ParameterMapEntry {..} =
    Core.object
      ( Core.catMaybes
          [("Key" Core..=) Core.<$> key, ("Values" Core..=) Core.<$> values]
      )

instance Core.FromJSON ParameterMapEntry where
  parseJSON =
    Core.withObject "ParameterMapEntry" Core.$
      \x ->
        ParameterMapEntry'
          Core.<$> (x Core..:? "Key") Core.<*> (x Core..:? "Values")
