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
-- Module      : Amazonka.ComprehendMedical.Types.ICD10CMAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ICD10CMAttribute where

import Amazonka.ComprehendMedical.Types.ICD10CMAttributeType
import Amazonka.ComprehendMedical.Types.ICD10CMEntityType
import Amazonka.ComprehendMedical.Types.ICD10CMRelationshipType
import Amazonka.ComprehendMedical.Types.ICD10CMTrait
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The detected attributes that relate to an entity. This includes an
-- extracted segment of the text that is an attribute of an entity, or
-- otherwise related to an entity. InferICD10CM detects the following
-- attributes: @Direction@, @System, Organ or Site@, and @Acuity@.
--
-- /See:/ 'newICD10CMAttribute' smart constructor.
data ICD10CMAttribute = ICD10CMAttribute'
  { -- | The 0-based character offset in the input text that shows where the
    -- attribute begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The level of confidence that Amazon Comprehend Medical has that this
    -- attribute is correctly related to this entity.
    relationshipScore :: Prelude.Maybe Prelude.Double,
    -- | The type of attribute. InferICD10CM detects entities of the type
    -- @DX_NAME@.
    type' :: Prelude.Maybe ICD10CMAttributeType,
    -- | The contextual information for the attribute. The traits recognized by
    -- InferICD10CM are @DIAGNOSIS@, @SIGN@, @SYMPTOM@, and @NEGATION@.
    traits :: Prelude.Maybe [ICD10CMTrait],
    -- | The level of confidence that Amazon Comprehend Medical has that the
    -- segment of text is correctly recognized as an attribute.
    score :: Prelude.Maybe Prelude.Double,
    -- | The numeric identifier for this attribute. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | The 0-based character offset in the input text that shows where the
    -- attribute ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The type of relationship between the entity and attribute. Type for the
    -- relationship can be either of @OVERLAP@ or @SYSTEM_ORGAN_SITE@.
    relationshipType :: Prelude.Maybe ICD10CMRelationshipType,
    -- | The category of attribute. Can be either of @DX_NAME@ or
    -- @TIME_EXPRESSION@.
    category :: Prelude.Maybe ICD10CMEntityType,
    -- | The segment of input text which contains the detected attribute.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ICD10CMAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'iCD10CMAttribute_beginOffset' - The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
--
-- 'relationshipScore', 'iCD10CMAttribute_relationshipScore' - The level of confidence that Amazon Comprehend Medical has that this
-- attribute is correctly related to this entity.
--
-- 'type'', 'iCD10CMAttribute_type' - The type of attribute. InferICD10CM detects entities of the type
-- @DX_NAME@.
--
-- 'traits', 'iCD10CMAttribute_traits' - The contextual information for the attribute. The traits recognized by
-- InferICD10CM are @DIAGNOSIS@, @SIGN@, @SYMPTOM@, and @NEGATION@.
--
-- 'score', 'iCD10CMAttribute_score' - The level of confidence that Amazon Comprehend Medical has that the
-- segment of text is correctly recognized as an attribute.
--
-- 'id', 'iCD10CMAttribute_id' - The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'endOffset', 'iCD10CMAttribute_endOffset' - The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
--
-- 'relationshipType', 'iCD10CMAttribute_relationshipType' - The type of relationship between the entity and attribute. Type for the
-- relationship can be either of @OVERLAP@ or @SYSTEM_ORGAN_SITE@.
--
-- 'category', 'iCD10CMAttribute_category' - The category of attribute. Can be either of @DX_NAME@ or
-- @TIME_EXPRESSION@.
--
-- 'text', 'iCD10CMAttribute_text' - The segment of input text which contains the detected attribute.
newICD10CMAttribute ::
  ICD10CMAttribute
newICD10CMAttribute =
  ICD10CMAttribute'
    { beginOffset = Prelude.Nothing,
      relationshipScore = Prelude.Nothing,
      type' = Prelude.Nothing,
      traits = Prelude.Nothing,
      score = Prelude.Nothing,
      id = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      relationshipType = Prelude.Nothing,
      category = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
iCD10CMAttribute_beginOffset :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe Prelude.Int)
iCD10CMAttribute_beginOffset = Lens.lens (\ICD10CMAttribute' {beginOffset} -> beginOffset) (\s@ICD10CMAttribute' {} a -> s {beginOffset = a} :: ICD10CMAttribute)

-- | The level of confidence that Amazon Comprehend Medical has that this
-- attribute is correctly related to this entity.
iCD10CMAttribute_relationshipScore :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe Prelude.Double)
iCD10CMAttribute_relationshipScore = Lens.lens (\ICD10CMAttribute' {relationshipScore} -> relationshipScore) (\s@ICD10CMAttribute' {} a -> s {relationshipScore = a} :: ICD10CMAttribute)

-- | The type of attribute. InferICD10CM detects entities of the type
-- @DX_NAME@.
iCD10CMAttribute_type :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe ICD10CMAttributeType)
iCD10CMAttribute_type = Lens.lens (\ICD10CMAttribute' {type'} -> type') (\s@ICD10CMAttribute' {} a -> s {type' = a} :: ICD10CMAttribute)

-- | The contextual information for the attribute. The traits recognized by
-- InferICD10CM are @DIAGNOSIS@, @SIGN@, @SYMPTOM@, and @NEGATION@.
iCD10CMAttribute_traits :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe [ICD10CMTrait])
iCD10CMAttribute_traits = Lens.lens (\ICD10CMAttribute' {traits} -> traits) (\s@ICD10CMAttribute' {} a -> s {traits = a} :: ICD10CMAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The level of confidence that Amazon Comprehend Medical has that the
-- segment of text is correctly recognized as an attribute.
iCD10CMAttribute_score :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe Prelude.Double)
iCD10CMAttribute_score = Lens.lens (\ICD10CMAttribute' {score} -> score) (\s@ICD10CMAttribute' {} a -> s {score = a} :: ICD10CMAttribute)

-- | The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
iCD10CMAttribute_id :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe Prelude.Int)
iCD10CMAttribute_id = Lens.lens (\ICD10CMAttribute' {id} -> id) (\s@ICD10CMAttribute' {} a -> s {id = a} :: ICD10CMAttribute)

-- | The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
iCD10CMAttribute_endOffset :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe Prelude.Int)
iCD10CMAttribute_endOffset = Lens.lens (\ICD10CMAttribute' {endOffset} -> endOffset) (\s@ICD10CMAttribute' {} a -> s {endOffset = a} :: ICD10CMAttribute)

-- | The type of relationship between the entity and attribute. Type for the
-- relationship can be either of @OVERLAP@ or @SYSTEM_ORGAN_SITE@.
iCD10CMAttribute_relationshipType :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe ICD10CMRelationshipType)
iCD10CMAttribute_relationshipType = Lens.lens (\ICD10CMAttribute' {relationshipType} -> relationshipType) (\s@ICD10CMAttribute' {} a -> s {relationshipType = a} :: ICD10CMAttribute)

-- | The category of attribute. Can be either of @DX_NAME@ or
-- @TIME_EXPRESSION@.
iCD10CMAttribute_category :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe ICD10CMEntityType)
iCD10CMAttribute_category = Lens.lens (\ICD10CMAttribute' {category} -> category) (\s@ICD10CMAttribute' {} a -> s {category = a} :: ICD10CMAttribute)

-- | The segment of input text which contains the detected attribute.
iCD10CMAttribute_text :: Lens.Lens' ICD10CMAttribute (Prelude.Maybe Prelude.Text)
iCD10CMAttribute_text = Lens.lens (\ICD10CMAttribute' {text} -> text) (\s@ICD10CMAttribute' {} a -> s {text = a} :: ICD10CMAttribute)

instance Data.FromJSON ICD10CMAttribute where
  parseJSON =
    Data.withObject
      "ICD10CMAttribute"
      ( \x ->
          ICD10CMAttribute'
            Prelude.<$> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "RelationshipScore")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Traits" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "EndOffset")
            Prelude.<*> (x Data..:? "RelationshipType")
            Prelude.<*> (x Data..:? "Category")
            Prelude.<*> (x Data..:? "Text")
      )

instance Prelude.Hashable ICD10CMAttribute where
  hashWithSalt _salt ICD10CMAttribute' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` relationshipScore
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` relationshipType
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` text

instance Prelude.NFData ICD10CMAttribute where
  rnf ICD10CMAttribute' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf relationshipScore
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf traits
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf relationshipType
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf text
