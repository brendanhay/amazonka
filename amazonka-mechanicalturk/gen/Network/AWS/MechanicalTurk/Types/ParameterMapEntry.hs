{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ParameterMapEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ParameterMapEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data structure is the data type for the AnswerKey parameter of the
-- ScoreMyKnownAnswers\/2011-09-01 Review Policy.
--
-- /See:/ 'newParameterMapEntry' smart constructor.
data ParameterMapEntry = ParameterMapEntry'
  { -- | The QuestionID from the HIT that is used to identify which question
    -- requires Mechanical Turk to score as part of the
    -- ScoreMyKnownAnswers\/2011-09-01 Review Policy.
    key :: Core.Maybe Core.Text,
    -- | The list of answers to the question specified in the MapEntry Key
    -- element. The Worker must match all values in order for the answer to be
    -- scored correctly.
    values :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterMapEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'parameterMapEntry_key' - The QuestionID from the HIT that is used to identify which question
-- requires Mechanical Turk to score as part of the
-- ScoreMyKnownAnswers\/2011-09-01 Review Policy.
--
-- 'values', 'parameterMapEntry_values' - The list of answers to the question specified in the MapEntry Key
-- element. The Worker must match all values in order for the answer to be
-- scored correctly.
newParameterMapEntry ::
  ParameterMapEntry
newParameterMapEntry =
  ParameterMapEntry'
    { key = Core.Nothing,
      values = Core.Nothing
    }

-- | The QuestionID from the HIT that is used to identify which question
-- requires Mechanical Turk to score as part of the
-- ScoreMyKnownAnswers\/2011-09-01 Review Policy.
parameterMapEntry_key :: Lens.Lens' ParameterMapEntry (Core.Maybe Core.Text)
parameterMapEntry_key = Lens.lens (\ParameterMapEntry' {key} -> key) (\s@ParameterMapEntry' {} a -> s {key = a} :: ParameterMapEntry)

-- | The list of answers to the question specified in the MapEntry Key
-- element. The Worker must match all values in order for the answer to be
-- scored correctly.
parameterMapEntry_values :: Lens.Lens' ParameterMapEntry (Core.Maybe [Core.Text])
parameterMapEntry_values = Lens.lens (\ParameterMapEntry' {values} -> values) (\s@ParameterMapEntry' {} a -> s {values = a} :: ParameterMapEntry) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ParameterMapEntry where
  parseJSON =
    Core.withObject
      "ParameterMapEntry"
      ( \x ->
          ParameterMapEntry'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Values" Core..!= Core.mempty)
      )

instance Core.Hashable ParameterMapEntry

instance Core.NFData ParameterMapEntry

instance Core.ToJSON ParameterMapEntry where
  toJSON ParameterMapEntry' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values
          ]
      )
