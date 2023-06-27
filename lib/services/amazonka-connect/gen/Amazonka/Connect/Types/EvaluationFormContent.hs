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
-- Module      : Amazonka.Connect.Types.EvaluationFormContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormContent where

import Amazonka.Connect.Types.EvaluationFormItem
import Amazonka.Connect.Types.EvaluationFormScoringStrategy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an evaluation form used in a contact evaluation.
--
-- /See:/ 'newEvaluationFormContent' smart constructor.
data EvaluationFormContent = EvaluationFormContent'
  { -- | The description of the evaluation form.
    description :: Prelude.Maybe Prelude.Text,
    -- | A scoring strategy of the evaluation form.
    scoringStrategy :: Prelude.Maybe EvaluationFormScoringStrategy,
    -- | A version of the evaluation form.
    evaluationFormVersion :: Prelude.Natural,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the evaluation form resource.
    evaluationFormArn :: Prelude.Text,
    -- | A title of the evaluation form.
    title :: Prelude.Text,
    -- | Items that are part of the evaluation form. The total number of sections
    -- and questions must not exceed 100 each. Questions must be contained in a
    -- section.
    items :: Prelude.NonEmpty EvaluationFormItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'evaluationFormContent_description' - The description of the evaluation form.
--
-- 'scoringStrategy', 'evaluationFormContent_scoringStrategy' - A scoring strategy of the evaluation form.
--
-- 'evaluationFormVersion', 'evaluationFormContent_evaluationFormVersion' - A version of the evaluation form.
--
-- 'evaluationFormId', 'evaluationFormContent_evaluationFormId' - The unique identifier for the evaluation form.
--
-- 'evaluationFormArn', 'evaluationFormContent_evaluationFormArn' - The Amazon Resource Name (ARN) for the evaluation form resource.
--
-- 'title', 'evaluationFormContent_title' - A title of the evaluation form.
--
-- 'items', 'evaluationFormContent_items' - Items that are part of the evaluation form. The total number of sections
-- and questions must not exceed 100 each. Questions must be contained in a
-- section.
newEvaluationFormContent ::
  -- | 'evaluationFormVersion'
  Prelude.Natural ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  -- | 'evaluationFormArn'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty EvaluationFormItem ->
  EvaluationFormContent
newEvaluationFormContent
  pEvaluationFormVersion_
  pEvaluationFormId_
  pEvaluationFormArn_
  pTitle_
  pItems_ =
    EvaluationFormContent'
      { description =
          Prelude.Nothing,
        scoringStrategy = Prelude.Nothing,
        evaluationFormVersion = pEvaluationFormVersion_,
        evaluationFormId = pEvaluationFormId_,
        evaluationFormArn = pEvaluationFormArn_,
        title = pTitle_,
        items = Lens.coerced Lens.# pItems_
      }

-- | The description of the evaluation form.
evaluationFormContent_description :: Lens.Lens' EvaluationFormContent (Prelude.Maybe Prelude.Text)
evaluationFormContent_description = Lens.lens (\EvaluationFormContent' {description} -> description) (\s@EvaluationFormContent' {} a -> s {description = a} :: EvaluationFormContent)

-- | A scoring strategy of the evaluation form.
evaluationFormContent_scoringStrategy :: Lens.Lens' EvaluationFormContent (Prelude.Maybe EvaluationFormScoringStrategy)
evaluationFormContent_scoringStrategy = Lens.lens (\EvaluationFormContent' {scoringStrategy} -> scoringStrategy) (\s@EvaluationFormContent' {} a -> s {scoringStrategy = a} :: EvaluationFormContent)

-- | A version of the evaluation form.
evaluationFormContent_evaluationFormVersion :: Lens.Lens' EvaluationFormContent Prelude.Natural
evaluationFormContent_evaluationFormVersion = Lens.lens (\EvaluationFormContent' {evaluationFormVersion} -> evaluationFormVersion) (\s@EvaluationFormContent' {} a -> s {evaluationFormVersion = a} :: EvaluationFormContent)

-- | The unique identifier for the evaluation form.
evaluationFormContent_evaluationFormId :: Lens.Lens' EvaluationFormContent Prelude.Text
evaluationFormContent_evaluationFormId = Lens.lens (\EvaluationFormContent' {evaluationFormId} -> evaluationFormId) (\s@EvaluationFormContent' {} a -> s {evaluationFormId = a} :: EvaluationFormContent)

-- | The Amazon Resource Name (ARN) for the evaluation form resource.
evaluationFormContent_evaluationFormArn :: Lens.Lens' EvaluationFormContent Prelude.Text
evaluationFormContent_evaluationFormArn = Lens.lens (\EvaluationFormContent' {evaluationFormArn} -> evaluationFormArn) (\s@EvaluationFormContent' {} a -> s {evaluationFormArn = a} :: EvaluationFormContent)

-- | A title of the evaluation form.
evaluationFormContent_title :: Lens.Lens' EvaluationFormContent Prelude.Text
evaluationFormContent_title = Lens.lens (\EvaluationFormContent' {title} -> title) (\s@EvaluationFormContent' {} a -> s {title = a} :: EvaluationFormContent)

-- | Items that are part of the evaluation form. The total number of sections
-- and questions must not exceed 100 each. Questions must be contained in a
-- section.
evaluationFormContent_items :: Lens.Lens' EvaluationFormContent (Prelude.NonEmpty EvaluationFormItem)
evaluationFormContent_items = Lens.lens (\EvaluationFormContent' {items} -> items) (\s@EvaluationFormContent' {} a -> s {items = a} :: EvaluationFormContent) Prelude.. Lens.coerced

instance Data.FromJSON EvaluationFormContent where
  parseJSON =
    Data.withObject
      "EvaluationFormContent"
      ( \x ->
          EvaluationFormContent'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ScoringStrategy")
            Prelude.<*> (x Data..: "EvaluationFormVersion")
            Prelude.<*> (x Data..: "EvaluationFormId")
            Prelude.<*> (x Data..: "EvaluationFormArn")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "Items")
      )

instance Prelude.Hashable EvaluationFormContent where
  hashWithSalt _salt EvaluationFormContent' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` scoringStrategy
      `Prelude.hashWithSalt` evaluationFormVersion
      `Prelude.hashWithSalt` evaluationFormId
      `Prelude.hashWithSalt` evaluationFormArn
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` items

instance Prelude.NFData EvaluationFormContent where
  rnf EvaluationFormContent' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf scoringStrategy
      `Prelude.seq` Prelude.rnf evaluationFormVersion
      `Prelude.seq` Prelude.rnf evaluationFormId
      `Prelude.seq` Prelude.rnf evaluationFormArn
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf items
