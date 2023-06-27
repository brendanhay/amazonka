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
-- Module      : Amazonka.Connect.Types.SingleSelectQuestionRuleCategoryAutomation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.SingleSelectQuestionRuleCategoryAutomation where

import Amazonka.Connect.Types.SingleSelectQuestionRuleCategoryAutomationCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the automation option based on a rule category for a
-- single select question.
--
-- /See:/ 'newSingleSelectQuestionRuleCategoryAutomation' smart constructor.
data SingleSelectQuestionRuleCategoryAutomation = SingleSelectQuestionRuleCategoryAutomation'
  { -- | The category name, as defined in Rules.
    category :: Prelude.Text,
    -- | The condition to apply for the automation option. If the condition is
    -- @PRESENT@, then the option is applied when the contact data includes the
    -- category. Similarly, if the condition is @NOT_PRESENT@, then the option
    -- is applied when the contact data does not include the category.
    condition :: SingleSelectQuestionRuleCategoryAutomationCondition,
    -- | The identifier of the answer option.
    optionRefId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingleSelectQuestionRuleCategoryAutomation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'singleSelectQuestionRuleCategoryAutomation_category' - The category name, as defined in Rules.
--
-- 'condition', 'singleSelectQuestionRuleCategoryAutomation_condition' - The condition to apply for the automation option. If the condition is
-- @PRESENT@, then the option is applied when the contact data includes the
-- category. Similarly, if the condition is @NOT_PRESENT@, then the option
-- is applied when the contact data does not include the category.
--
-- 'optionRefId', 'singleSelectQuestionRuleCategoryAutomation_optionRefId' - The identifier of the answer option.
newSingleSelectQuestionRuleCategoryAutomation ::
  -- | 'category'
  Prelude.Text ->
  -- | 'condition'
  SingleSelectQuestionRuleCategoryAutomationCondition ->
  -- | 'optionRefId'
  Prelude.Text ->
  SingleSelectQuestionRuleCategoryAutomation
newSingleSelectQuestionRuleCategoryAutomation
  pCategory_
  pCondition_
  pOptionRefId_ =
    SingleSelectQuestionRuleCategoryAutomation'
      { category =
          pCategory_,
        condition = pCondition_,
        optionRefId = pOptionRefId_
      }

-- | The category name, as defined in Rules.
singleSelectQuestionRuleCategoryAutomation_category :: Lens.Lens' SingleSelectQuestionRuleCategoryAutomation Prelude.Text
singleSelectQuestionRuleCategoryAutomation_category = Lens.lens (\SingleSelectQuestionRuleCategoryAutomation' {category} -> category) (\s@SingleSelectQuestionRuleCategoryAutomation' {} a -> s {category = a} :: SingleSelectQuestionRuleCategoryAutomation)

-- | The condition to apply for the automation option. If the condition is
-- @PRESENT@, then the option is applied when the contact data includes the
-- category. Similarly, if the condition is @NOT_PRESENT@, then the option
-- is applied when the contact data does not include the category.
singleSelectQuestionRuleCategoryAutomation_condition :: Lens.Lens' SingleSelectQuestionRuleCategoryAutomation SingleSelectQuestionRuleCategoryAutomationCondition
singleSelectQuestionRuleCategoryAutomation_condition = Lens.lens (\SingleSelectQuestionRuleCategoryAutomation' {condition} -> condition) (\s@SingleSelectQuestionRuleCategoryAutomation' {} a -> s {condition = a} :: SingleSelectQuestionRuleCategoryAutomation)

-- | The identifier of the answer option.
singleSelectQuestionRuleCategoryAutomation_optionRefId :: Lens.Lens' SingleSelectQuestionRuleCategoryAutomation Prelude.Text
singleSelectQuestionRuleCategoryAutomation_optionRefId = Lens.lens (\SingleSelectQuestionRuleCategoryAutomation' {optionRefId} -> optionRefId) (\s@SingleSelectQuestionRuleCategoryAutomation' {} a -> s {optionRefId = a} :: SingleSelectQuestionRuleCategoryAutomation)

instance
  Data.FromJSON
    SingleSelectQuestionRuleCategoryAutomation
  where
  parseJSON =
    Data.withObject
      "SingleSelectQuestionRuleCategoryAutomation"
      ( \x ->
          SingleSelectQuestionRuleCategoryAutomation'
            Prelude.<$> (x Data..: "Category")
            Prelude.<*> (x Data..: "Condition")
            Prelude.<*> (x Data..: "OptionRefId")
      )

instance
  Prelude.Hashable
    SingleSelectQuestionRuleCategoryAutomation
  where
  hashWithSalt
    _salt
    SingleSelectQuestionRuleCategoryAutomation' {..} =
      _salt
        `Prelude.hashWithSalt` category
        `Prelude.hashWithSalt` condition
        `Prelude.hashWithSalt` optionRefId

instance
  Prelude.NFData
    SingleSelectQuestionRuleCategoryAutomation
  where
  rnf SingleSelectQuestionRuleCategoryAutomation' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf optionRefId

instance
  Data.ToJSON
    SingleSelectQuestionRuleCategoryAutomation
  where
  toJSON
    SingleSelectQuestionRuleCategoryAutomation' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("Category" Data..= category),
              Prelude.Just ("Condition" Data..= condition),
              Prelude.Just ("OptionRefId" Data..= optionRefId)
            ]
        )
