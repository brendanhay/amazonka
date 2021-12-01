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
-- Module      : Amazonka.MechanicalTurk.Types.ParameterMapEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.ParameterMapEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | This data structure is the data type for the AnswerKey parameter of the
-- ScoreMyKnownAnswers\/2011-09-01 Review Policy.
--
-- /See:/ 'newParameterMapEntry' smart constructor.
data ParameterMapEntry = ParameterMapEntry'
  { -- | The list of answers to the question specified in the MapEntry Key
    -- element. The Worker must match all values in order for the answer to be
    -- scored correctly.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The QuestionID from the HIT that is used to identify which question
    -- requires Mechanical Turk to score as part of the
    -- ScoreMyKnownAnswers\/2011-09-01 Review Policy.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterMapEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'parameterMapEntry_values' - The list of answers to the question specified in the MapEntry Key
-- element. The Worker must match all values in order for the answer to be
-- scored correctly.
--
-- 'key', 'parameterMapEntry_key' - The QuestionID from the HIT that is used to identify which question
-- requires Mechanical Turk to score as part of the
-- ScoreMyKnownAnswers\/2011-09-01 Review Policy.
newParameterMapEntry ::
  ParameterMapEntry
newParameterMapEntry =
  ParameterMapEntry'
    { values = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The list of answers to the question specified in the MapEntry Key
-- element. The Worker must match all values in order for the answer to be
-- scored correctly.
parameterMapEntry_values :: Lens.Lens' ParameterMapEntry (Prelude.Maybe [Prelude.Text])
parameterMapEntry_values = Lens.lens (\ParameterMapEntry' {values} -> values) (\s@ParameterMapEntry' {} a -> s {values = a} :: ParameterMapEntry) Prelude.. Lens.mapping Lens.coerced

-- | The QuestionID from the HIT that is used to identify which question
-- requires Mechanical Turk to score as part of the
-- ScoreMyKnownAnswers\/2011-09-01 Review Policy.
parameterMapEntry_key :: Lens.Lens' ParameterMapEntry (Prelude.Maybe Prelude.Text)
parameterMapEntry_key = Lens.lens (\ParameterMapEntry' {key} -> key) (\s@ParameterMapEntry' {} a -> s {key = a} :: ParameterMapEntry)

instance Core.FromJSON ParameterMapEntry where
  parseJSON =
    Core.withObject
      "ParameterMapEntry"
      ( \x ->
          ParameterMapEntry'
            Prelude.<$> (x Core..:? "Values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Key")
      )

instance Prelude.Hashable ParameterMapEntry where
  hashWithSalt salt' ParameterMapEntry' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData ParameterMapEntry where
  rnf ParameterMapEntry' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf key

instance Core.ToJSON ParameterMapEntry where
  toJSON ParameterMapEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("Key" Core..=) Prelude.<$> key
          ]
      )
