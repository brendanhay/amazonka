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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTAttribute where

import Amazonka.ComprehendMedical.Types.SNOMEDCTAttributeType
import Amazonka.ComprehendMedical.Types.SNOMEDCTConcept
import Amazonka.ComprehendMedical.Types.SNOMEDCTEntityCategory
import Amazonka.ComprehendMedical.Types.SNOMEDCTRelationshipType
import Amazonka.ComprehendMedical.Types.SNOMEDCTTrait
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The extracted attributes that relate to an entity. An extracted segment
-- of the text that is an attribute of an entity, or otherwise related to
-- an entity, such as the dosage of a medication taken.
--
-- /See:/ 'newSNOMEDCTAttribute' smart constructor.
data SNOMEDCTAttribute = SNOMEDCTAttribute'
  { -- | The 0-based character offset in the input text that shows where the
    -- attribute begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The level of confidence that Comprehend Medical has that this attribute
    -- is correctly related to this entity.
    relationshipScore :: Prelude.Maybe Prelude.Double,
    -- | The type of attribute. Possible types include DX_NAME, ACUITY,
    -- DIRECTION, SYSTEM_ORGAN_SITE,TEST_NAME, TEST_VALUE, TEST_UNIT,
    -- PROCEDURE_NAME, and TREATMENT_NAME.
    type' :: Prelude.Maybe SNOMEDCTAttributeType,
    -- | Contextual information for an attribute. Examples include signs,
    -- symptoms, diagnosis, and negation.
    traits :: Prelude.Maybe [SNOMEDCTTrait],
    -- | The level of confidence that Comprehend Medical has that the segment of
    -- text is correctly recognized as an attribute.
    score :: Prelude.Maybe Prelude.Double,
    -- | The numeric identifier for this attribute. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | The 0-based character offset in the input text that shows where the
    -- attribute ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The type of relationship that exists between the entity and the related
    -- attribute.
    relationshipType :: Prelude.Maybe SNOMEDCTRelationshipType,
    -- | The category of the detected attribute. Possible categories include
    -- MEDICAL_CONDITION, ANATOMY, and TEST_TREATMENT_PROCEDURE.
    category :: Prelude.Maybe SNOMEDCTEntityCategory,
    -- | The segment of input text extracted as this attribute.
    text :: Prelude.Maybe Prelude.Text,
    -- | The SNOMED-CT concepts specific to an attribute, along with a score
    -- indicating the likelihood of the match.
    sNOMEDCTConcepts :: Prelude.Maybe [SNOMEDCTConcept]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SNOMEDCTAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'sNOMEDCTAttribute_beginOffset' - The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
--
-- 'relationshipScore', 'sNOMEDCTAttribute_relationshipScore' - The level of confidence that Comprehend Medical has that this attribute
-- is correctly related to this entity.
--
-- 'type'', 'sNOMEDCTAttribute_type' - The type of attribute. Possible types include DX_NAME, ACUITY,
-- DIRECTION, SYSTEM_ORGAN_SITE,TEST_NAME, TEST_VALUE, TEST_UNIT,
-- PROCEDURE_NAME, and TREATMENT_NAME.
--
-- 'traits', 'sNOMEDCTAttribute_traits' - Contextual information for an attribute. Examples include signs,
-- symptoms, diagnosis, and negation.
--
-- 'score', 'sNOMEDCTAttribute_score' - The level of confidence that Comprehend Medical has that the segment of
-- text is correctly recognized as an attribute.
--
-- 'id', 'sNOMEDCTAttribute_id' - The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'endOffset', 'sNOMEDCTAttribute_endOffset' - The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
--
-- 'relationshipType', 'sNOMEDCTAttribute_relationshipType' - The type of relationship that exists between the entity and the related
-- attribute.
--
-- 'category', 'sNOMEDCTAttribute_category' - The category of the detected attribute. Possible categories include
-- MEDICAL_CONDITION, ANATOMY, and TEST_TREATMENT_PROCEDURE.
--
-- 'text', 'sNOMEDCTAttribute_text' - The segment of input text extracted as this attribute.
--
-- 'sNOMEDCTConcepts', 'sNOMEDCTAttribute_sNOMEDCTConcepts' - The SNOMED-CT concepts specific to an attribute, along with a score
-- indicating the likelihood of the match.
newSNOMEDCTAttribute ::
  SNOMEDCTAttribute
newSNOMEDCTAttribute =
  SNOMEDCTAttribute'
    { beginOffset = Prelude.Nothing,
      relationshipScore = Prelude.Nothing,
      type' = Prelude.Nothing,
      traits = Prelude.Nothing,
      score = Prelude.Nothing,
      id = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      relationshipType = Prelude.Nothing,
      category = Prelude.Nothing,
      text = Prelude.Nothing,
      sNOMEDCTConcepts = Prelude.Nothing
    }

-- | The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
sNOMEDCTAttribute_beginOffset :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe Prelude.Int)
sNOMEDCTAttribute_beginOffset = Lens.lens (\SNOMEDCTAttribute' {beginOffset} -> beginOffset) (\s@SNOMEDCTAttribute' {} a -> s {beginOffset = a} :: SNOMEDCTAttribute)

-- | The level of confidence that Comprehend Medical has that this attribute
-- is correctly related to this entity.
sNOMEDCTAttribute_relationshipScore :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe Prelude.Double)
sNOMEDCTAttribute_relationshipScore = Lens.lens (\SNOMEDCTAttribute' {relationshipScore} -> relationshipScore) (\s@SNOMEDCTAttribute' {} a -> s {relationshipScore = a} :: SNOMEDCTAttribute)

-- | The type of attribute. Possible types include DX_NAME, ACUITY,
-- DIRECTION, SYSTEM_ORGAN_SITE,TEST_NAME, TEST_VALUE, TEST_UNIT,
-- PROCEDURE_NAME, and TREATMENT_NAME.
sNOMEDCTAttribute_type :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe SNOMEDCTAttributeType)
sNOMEDCTAttribute_type = Lens.lens (\SNOMEDCTAttribute' {type'} -> type') (\s@SNOMEDCTAttribute' {} a -> s {type' = a} :: SNOMEDCTAttribute)

-- | Contextual information for an attribute. Examples include signs,
-- symptoms, diagnosis, and negation.
sNOMEDCTAttribute_traits :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe [SNOMEDCTTrait])
sNOMEDCTAttribute_traits = Lens.lens (\SNOMEDCTAttribute' {traits} -> traits) (\s@SNOMEDCTAttribute' {} a -> s {traits = a} :: SNOMEDCTAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The level of confidence that Comprehend Medical has that the segment of
-- text is correctly recognized as an attribute.
sNOMEDCTAttribute_score :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe Prelude.Double)
sNOMEDCTAttribute_score = Lens.lens (\SNOMEDCTAttribute' {score} -> score) (\s@SNOMEDCTAttribute' {} a -> s {score = a} :: SNOMEDCTAttribute)

-- | The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
sNOMEDCTAttribute_id :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe Prelude.Int)
sNOMEDCTAttribute_id = Lens.lens (\SNOMEDCTAttribute' {id} -> id) (\s@SNOMEDCTAttribute' {} a -> s {id = a} :: SNOMEDCTAttribute)

-- | The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
sNOMEDCTAttribute_endOffset :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe Prelude.Int)
sNOMEDCTAttribute_endOffset = Lens.lens (\SNOMEDCTAttribute' {endOffset} -> endOffset) (\s@SNOMEDCTAttribute' {} a -> s {endOffset = a} :: SNOMEDCTAttribute)

-- | The type of relationship that exists between the entity and the related
-- attribute.
sNOMEDCTAttribute_relationshipType :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe SNOMEDCTRelationshipType)
sNOMEDCTAttribute_relationshipType = Lens.lens (\SNOMEDCTAttribute' {relationshipType} -> relationshipType) (\s@SNOMEDCTAttribute' {} a -> s {relationshipType = a} :: SNOMEDCTAttribute)

-- | The category of the detected attribute. Possible categories include
-- MEDICAL_CONDITION, ANATOMY, and TEST_TREATMENT_PROCEDURE.
sNOMEDCTAttribute_category :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe SNOMEDCTEntityCategory)
sNOMEDCTAttribute_category = Lens.lens (\SNOMEDCTAttribute' {category} -> category) (\s@SNOMEDCTAttribute' {} a -> s {category = a} :: SNOMEDCTAttribute)

-- | The segment of input text extracted as this attribute.
sNOMEDCTAttribute_text :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe Prelude.Text)
sNOMEDCTAttribute_text = Lens.lens (\SNOMEDCTAttribute' {text} -> text) (\s@SNOMEDCTAttribute' {} a -> s {text = a} :: SNOMEDCTAttribute)

-- | The SNOMED-CT concepts specific to an attribute, along with a score
-- indicating the likelihood of the match.
sNOMEDCTAttribute_sNOMEDCTConcepts :: Lens.Lens' SNOMEDCTAttribute (Prelude.Maybe [SNOMEDCTConcept])
sNOMEDCTAttribute_sNOMEDCTConcepts = Lens.lens (\SNOMEDCTAttribute' {sNOMEDCTConcepts} -> sNOMEDCTConcepts) (\s@SNOMEDCTAttribute' {} a -> s {sNOMEDCTConcepts = a} :: SNOMEDCTAttribute) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SNOMEDCTAttribute where
  parseJSON =
    Core.withObject
      "SNOMEDCTAttribute"
      ( \x ->
          SNOMEDCTAttribute'
            Prelude.<$> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "RelationshipScore")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Traits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Score")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "EndOffset")
            Prelude.<*> (x Core..:? "RelationshipType")
            Prelude.<*> (x Core..:? "Category")
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> ( x Core..:? "SNOMEDCTConcepts"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SNOMEDCTAttribute where
  hashWithSalt _salt SNOMEDCTAttribute' {..} =
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
      `Prelude.hashWithSalt` sNOMEDCTConcepts

instance Prelude.NFData SNOMEDCTAttribute where
  rnf SNOMEDCTAttribute' {..} =
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
      `Prelude.seq` Prelude.rnf sNOMEDCTConcepts
