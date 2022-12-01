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
-- Module      : Amazonka.ComprehendMedical.Types.ICD10CMEntity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ICD10CMEntity where

import Amazonka.ComprehendMedical.Types.ICD10CMAttribute
import Amazonka.ComprehendMedical.Types.ICD10CMConcept
import Amazonka.ComprehendMedical.Types.ICD10CMEntityCategory
import Amazonka.ComprehendMedical.Types.ICD10CMEntityType
import Amazonka.ComprehendMedical.Types.ICD10CMTrait
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The collection of medical entities extracted from the input text and
-- their associated information. For each entity, the response provides the
-- entity text, the entity category, where the entity text begins and ends,
-- and the level of confidence that Amazon Comprehend Medical has in the
-- detection and analysis. Attributes and traits of the entity are also
-- returned.
--
-- /See:/ 'newICD10CMEntity' smart constructor.
data ICD10CMEntity = ICD10CMEntity'
  { -- | The 0-based character offset in the input text that shows where the
    -- entity begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | Describes the specific type of entity with category of entities.
    -- InferICD10CM detects entities of the type @DX_NAME@ and
    -- @TIME_EXPRESSION@.
    type' :: Prelude.Maybe ICD10CMEntityType,
    -- | Provides Contextual information for the entity. The traits recognized by
    -- InferICD10CM are @DIAGNOSIS@, @SIGN@, @SYMPTOM@, and @NEGATION.@
    traits :: Prelude.Maybe [ICD10CMTrait],
    -- | The level of confidence that Amazon Comprehend Medical has in the
    -- accuracy of the detection.
    score :: Prelude.Maybe Prelude.Double,
    -- | The numeric identifier for the entity. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | The 0-based character offset in the input text that shows where the
    -- entity ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The category of the entity. InferICD10CM detects entities in the
    -- @MEDICAL_CONDITION@ category.
    category :: Prelude.Maybe ICD10CMEntityCategory,
    -- | The detected attributes that relate to the entity. An extracted segment
    -- of the text that is an attribute of an entity, or otherwise related to
    -- an entity, such as the nature of a medical condition.
    attributes :: Prelude.Maybe [ICD10CMAttribute],
    -- | The segment of input text that is matched to the detected entity.
    text :: Prelude.Maybe Prelude.Text,
    -- | The ICD-10-CM concepts that the entity could refer to, along with a
    -- score indicating the likelihood of the match.
    iCD10CMConcepts :: Prelude.Maybe [ICD10CMConcept]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ICD10CMEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'iCD10CMEntity_beginOffset' - The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
--
-- 'type'', 'iCD10CMEntity_type' - Describes the specific type of entity with category of entities.
-- InferICD10CM detects entities of the type @DX_NAME@ and
-- @TIME_EXPRESSION@.
--
-- 'traits', 'iCD10CMEntity_traits' - Provides Contextual information for the entity. The traits recognized by
-- InferICD10CM are @DIAGNOSIS@, @SIGN@, @SYMPTOM@, and @NEGATION.@
--
-- 'score', 'iCD10CMEntity_score' - The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detection.
--
-- 'id', 'iCD10CMEntity_id' - The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'endOffset', 'iCD10CMEntity_endOffset' - The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
--
-- 'category', 'iCD10CMEntity_category' - The category of the entity. InferICD10CM detects entities in the
-- @MEDICAL_CONDITION@ category.
--
-- 'attributes', 'iCD10CMEntity_attributes' - The detected attributes that relate to the entity. An extracted segment
-- of the text that is an attribute of an entity, or otherwise related to
-- an entity, such as the nature of a medical condition.
--
-- 'text', 'iCD10CMEntity_text' - The segment of input text that is matched to the detected entity.
--
-- 'iCD10CMConcepts', 'iCD10CMEntity_iCD10CMConcepts' - The ICD-10-CM concepts that the entity could refer to, along with a
-- score indicating the likelihood of the match.
newICD10CMEntity ::
  ICD10CMEntity
newICD10CMEntity =
  ICD10CMEntity'
    { beginOffset = Prelude.Nothing,
      type' = Prelude.Nothing,
      traits = Prelude.Nothing,
      score = Prelude.Nothing,
      id = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      category = Prelude.Nothing,
      attributes = Prelude.Nothing,
      text = Prelude.Nothing,
      iCD10CMConcepts = Prelude.Nothing
    }

-- | The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
iCD10CMEntity_beginOffset :: Lens.Lens' ICD10CMEntity (Prelude.Maybe Prelude.Int)
iCD10CMEntity_beginOffset = Lens.lens (\ICD10CMEntity' {beginOffset} -> beginOffset) (\s@ICD10CMEntity' {} a -> s {beginOffset = a} :: ICD10CMEntity)

-- | Describes the specific type of entity with category of entities.
-- InferICD10CM detects entities of the type @DX_NAME@ and
-- @TIME_EXPRESSION@.
iCD10CMEntity_type :: Lens.Lens' ICD10CMEntity (Prelude.Maybe ICD10CMEntityType)
iCD10CMEntity_type = Lens.lens (\ICD10CMEntity' {type'} -> type') (\s@ICD10CMEntity' {} a -> s {type' = a} :: ICD10CMEntity)

-- | Provides Contextual information for the entity. The traits recognized by
-- InferICD10CM are @DIAGNOSIS@, @SIGN@, @SYMPTOM@, and @NEGATION.@
iCD10CMEntity_traits :: Lens.Lens' ICD10CMEntity (Prelude.Maybe [ICD10CMTrait])
iCD10CMEntity_traits = Lens.lens (\ICD10CMEntity' {traits} -> traits) (\s@ICD10CMEntity' {} a -> s {traits = a} :: ICD10CMEntity) Prelude.. Lens.mapping Lens.coerced

-- | The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detection.
iCD10CMEntity_score :: Lens.Lens' ICD10CMEntity (Prelude.Maybe Prelude.Double)
iCD10CMEntity_score = Lens.lens (\ICD10CMEntity' {score} -> score) (\s@ICD10CMEntity' {} a -> s {score = a} :: ICD10CMEntity)

-- | The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
iCD10CMEntity_id :: Lens.Lens' ICD10CMEntity (Prelude.Maybe Prelude.Int)
iCD10CMEntity_id = Lens.lens (\ICD10CMEntity' {id} -> id) (\s@ICD10CMEntity' {} a -> s {id = a} :: ICD10CMEntity)

-- | The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
iCD10CMEntity_endOffset :: Lens.Lens' ICD10CMEntity (Prelude.Maybe Prelude.Int)
iCD10CMEntity_endOffset = Lens.lens (\ICD10CMEntity' {endOffset} -> endOffset) (\s@ICD10CMEntity' {} a -> s {endOffset = a} :: ICD10CMEntity)

-- | The category of the entity. InferICD10CM detects entities in the
-- @MEDICAL_CONDITION@ category.
iCD10CMEntity_category :: Lens.Lens' ICD10CMEntity (Prelude.Maybe ICD10CMEntityCategory)
iCD10CMEntity_category = Lens.lens (\ICD10CMEntity' {category} -> category) (\s@ICD10CMEntity' {} a -> s {category = a} :: ICD10CMEntity)

-- | The detected attributes that relate to the entity. An extracted segment
-- of the text that is an attribute of an entity, or otherwise related to
-- an entity, such as the nature of a medical condition.
iCD10CMEntity_attributes :: Lens.Lens' ICD10CMEntity (Prelude.Maybe [ICD10CMAttribute])
iCD10CMEntity_attributes = Lens.lens (\ICD10CMEntity' {attributes} -> attributes) (\s@ICD10CMEntity' {} a -> s {attributes = a} :: ICD10CMEntity) Prelude.. Lens.mapping Lens.coerced

-- | The segment of input text that is matched to the detected entity.
iCD10CMEntity_text :: Lens.Lens' ICD10CMEntity (Prelude.Maybe Prelude.Text)
iCD10CMEntity_text = Lens.lens (\ICD10CMEntity' {text} -> text) (\s@ICD10CMEntity' {} a -> s {text = a} :: ICD10CMEntity)

-- | The ICD-10-CM concepts that the entity could refer to, along with a
-- score indicating the likelihood of the match.
iCD10CMEntity_iCD10CMConcepts :: Lens.Lens' ICD10CMEntity (Prelude.Maybe [ICD10CMConcept])
iCD10CMEntity_iCD10CMConcepts = Lens.lens (\ICD10CMEntity' {iCD10CMConcepts} -> iCD10CMConcepts) (\s@ICD10CMEntity' {} a -> s {iCD10CMConcepts = a} :: ICD10CMEntity) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ICD10CMEntity where
  parseJSON =
    Core.withObject
      "ICD10CMEntity"
      ( \x ->
          ICD10CMEntity'
            Prelude.<$> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Traits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Score")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "EndOffset")
            Prelude.<*> (x Core..:? "Category")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> ( x Core..:? "ICD10CMConcepts"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ICD10CMEntity where
  hashWithSalt _salt ICD10CMEntity' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` iCD10CMConcepts

instance Prelude.NFData ICD10CMEntity where
  rnf ICD10CMEntity' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf traits
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf iCD10CMConcepts
