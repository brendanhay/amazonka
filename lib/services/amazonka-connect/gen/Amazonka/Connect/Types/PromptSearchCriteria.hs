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
-- Module      : Amazonka.Connect.Types.PromptSearchCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.PromptSearchCriteria where

import Amazonka.Connect.Types.StringCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to return prompts.
--
-- /See:/ 'newPromptSearchCriteria' smart constructor.
data PromptSearchCriteria = PromptSearchCriteria'
  { -- | A list of conditions which would be applied together with an AND
    -- condition.
    andConditions :: Prelude.Maybe [PromptSearchCriteria],
    -- | A list of conditions which would be applied together with an OR
    -- condition.
    orConditions :: Prelude.Maybe [PromptSearchCriteria],
    -- | A leaf node condition which can be used to specify a string condition.
    --
    -- The currently supported values for @FieldName@ are @name@,
    -- @description@, and @resourceID@.
    stringCondition :: Prelude.Maybe StringCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromptSearchCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'andConditions', 'promptSearchCriteria_andConditions' - A list of conditions which would be applied together with an AND
-- condition.
--
-- 'orConditions', 'promptSearchCriteria_orConditions' - A list of conditions which would be applied together with an OR
-- condition.
--
-- 'stringCondition', 'promptSearchCriteria_stringCondition' - A leaf node condition which can be used to specify a string condition.
--
-- The currently supported values for @FieldName@ are @name@,
-- @description@, and @resourceID@.
newPromptSearchCriteria ::
  PromptSearchCriteria
newPromptSearchCriteria =
  PromptSearchCriteria'
    { andConditions =
        Prelude.Nothing,
      orConditions = Prelude.Nothing,
      stringCondition = Prelude.Nothing
    }

-- | A list of conditions which would be applied together with an AND
-- condition.
promptSearchCriteria_andConditions :: Lens.Lens' PromptSearchCriteria (Prelude.Maybe [PromptSearchCriteria])
promptSearchCriteria_andConditions = Lens.lens (\PromptSearchCriteria' {andConditions} -> andConditions) (\s@PromptSearchCriteria' {} a -> s {andConditions = a} :: PromptSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions which would be applied together with an OR
-- condition.
promptSearchCriteria_orConditions :: Lens.Lens' PromptSearchCriteria (Prelude.Maybe [PromptSearchCriteria])
promptSearchCriteria_orConditions = Lens.lens (\PromptSearchCriteria' {orConditions} -> orConditions) (\s@PromptSearchCriteria' {} a -> s {orConditions = a} :: PromptSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A leaf node condition which can be used to specify a string condition.
--
-- The currently supported values for @FieldName@ are @name@,
-- @description@, and @resourceID@.
promptSearchCriteria_stringCondition :: Lens.Lens' PromptSearchCriteria (Prelude.Maybe StringCondition)
promptSearchCriteria_stringCondition = Lens.lens (\PromptSearchCriteria' {stringCondition} -> stringCondition) (\s@PromptSearchCriteria' {} a -> s {stringCondition = a} :: PromptSearchCriteria)

instance Prelude.Hashable PromptSearchCriteria where
  hashWithSalt _salt PromptSearchCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` andConditions
      `Prelude.hashWithSalt` orConditions
      `Prelude.hashWithSalt` stringCondition

instance Prelude.NFData PromptSearchCriteria where
  rnf PromptSearchCriteria' {..} =
    Prelude.rnf andConditions
      `Prelude.seq` Prelude.rnf orConditions
      `Prelude.seq` Prelude.rnf stringCondition

instance Data.ToJSON PromptSearchCriteria where
  toJSON PromptSearchCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AndConditions" Data..=) Prelude.<$> andConditions,
            ("OrConditions" Data..=) Prelude.<$> orConditions,
            ("StringCondition" Data..=)
              Prelude.<$> stringCondition
          ]
      )
