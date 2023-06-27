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
-- Module      : Amazonka.Connect.Types.EvaluationFormSection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormSection where

import {-# SOURCE #-} Amazonka.Connect.Types.EvaluationFormItem
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a section from an evaluation form. A section can
-- contain sections and\/or questions. Evaluation forms can only contain
-- sections and subsections (two level nesting).
--
-- /See:/ 'newEvaluationFormSection' smart constructor.
data EvaluationFormSection = EvaluationFormSection'
  { -- | The instructions of the section.
    instructions :: Prelude.Maybe Prelude.Text,
    -- | The scoring weight of the section.
    weight :: Prelude.Maybe Prelude.Double,
    -- | The title of the section.
    title :: Prelude.Text,
    -- | The identifier of the section. An identifier must be unique within the
    -- evaluation form.
    refId :: Prelude.Text,
    -- | The items of the section.
    items :: Prelude.NonEmpty EvaluationFormItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormSection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instructions', 'evaluationFormSection_instructions' - The instructions of the section.
--
-- 'weight', 'evaluationFormSection_weight' - The scoring weight of the section.
--
-- 'title', 'evaluationFormSection_title' - The title of the section.
--
-- 'refId', 'evaluationFormSection_refId' - The identifier of the section. An identifier must be unique within the
-- evaluation form.
--
-- 'items', 'evaluationFormSection_items' - The items of the section.
newEvaluationFormSection ::
  -- | 'title'
  Prelude.Text ->
  -- | 'refId'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty EvaluationFormItem ->
  EvaluationFormSection
newEvaluationFormSection pTitle_ pRefId_ pItems_ =
  EvaluationFormSection'
    { instructions =
        Prelude.Nothing,
      weight = Prelude.Nothing,
      title = pTitle_,
      refId = pRefId_,
      items = Lens.coerced Lens.# pItems_
    }

-- | The instructions of the section.
evaluationFormSection_instructions :: Lens.Lens' EvaluationFormSection (Prelude.Maybe Prelude.Text)
evaluationFormSection_instructions = Lens.lens (\EvaluationFormSection' {instructions} -> instructions) (\s@EvaluationFormSection' {} a -> s {instructions = a} :: EvaluationFormSection)

-- | The scoring weight of the section.
evaluationFormSection_weight :: Lens.Lens' EvaluationFormSection (Prelude.Maybe Prelude.Double)
evaluationFormSection_weight = Lens.lens (\EvaluationFormSection' {weight} -> weight) (\s@EvaluationFormSection' {} a -> s {weight = a} :: EvaluationFormSection)

-- | The title of the section.
evaluationFormSection_title :: Lens.Lens' EvaluationFormSection Prelude.Text
evaluationFormSection_title = Lens.lens (\EvaluationFormSection' {title} -> title) (\s@EvaluationFormSection' {} a -> s {title = a} :: EvaluationFormSection)

-- | The identifier of the section. An identifier must be unique within the
-- evaluation form.
evaluationFormSection_refId :: Lens.Lens' EvaluationFormSection Prelude.Text
evaluationFormSection_refId = Lens.lens (\EvaluationFormSection' {refId} -> refId) (\s@EvaluationFormSection' {} a -> s {refId = a} :: EvaluationFormSection)

-- | The items of the section.
evaluationFormSection_items :: Lens.Lens' EvaluationFormSection (Prelude.NonEmpty EvaluationFormItem)
evaluationFormSection_items = Lens.lens (\EvaluationFormSection' {items} -> items) (\s@EvaluationFormSection' {} a -> s {items = a} :: EvaluationFormSection) Prelude.. Lens.coerced

instance Data.FromJSON EvaluationFormSection where
  parseJSON =
    Data.withObject
      "EvaluationFormSection"
      ( \x ->
          EvaluationFormSection'
            Prelude.<$> (x Data..:? "Instructions")
            Prelude.<*> (x Data..:? "Weight")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "RefId")
            Prelude.<*> (x Data..: "Items")
      )

instance Prelude.Hashable EvaluationFormSection where
  hashWithSalt _salt EvaluationFormSection' {..} =
    _salt
      `Prelude.hashWithSalt` instructions
      `Prelude.hashWithSalt` weight
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` refId
      `Prelude.hashWithSalt` items

instance Prelude.NFData EvaluationFormSection where
  rnf EvaluationFormSection' {..} =
    Prelude.rnf instructions
      `Prelude.seq` Prelude.rnf weight
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf refId
      `Prelude.seq` Prelude.rnf items

instance Data.ToJSON EvaluationFormSection where
  toJSON EvaluationFormSection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Instructions" Data..=) Prelude.<$> instructions,
            ("Weight" Data..=) Prelude.<$> weight,
            Prelude.Just ("Title" Data..= title),
            Prelude.Just ("RefId" Data..= refId),
            Prelude.Just ("Items" Data..= items)
          ]
      )
