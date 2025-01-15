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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTEntity where

import Amazonka.ComprehendMedical.Types.SNOMEDCTAttribute
import Amazonka.ComprehendMedical.Types.SNOMEDCTConcept
import Amazonka.ComprehendMedical.Types.SNOMEDCTEntityCategory
import Amazonka.ComprehendMedical.Types.SNOMEDCTEntityType
import Amazonka.ComprehendMedical.Types.SNOMEDCTTrait
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The collection of medical entities extracted from the input text and
-- their associated information. For each entity, the response provides the
-- entity text, the entity category, where the entity text begins and ends,
-- and the level of confidence that Comprehend Medical has in the detection
-- and analysis. Attributes and traits of the entity are also returned.
--
-- /See:/ 'newSNOMEDCTEntity' smart constructor.
data SNOMEDCTEntity = SNOMEDCTEntity'
  { -- | An extracted segment of the text that is an attribute of an entity, or
    -- otherwise related to an entity, such as the dosage of a medication
    -- taken.
    attributes :: Prelude.Maybe [SNOMEDCTAttribute],
    -- | The 0-based character offset in the input text that shows where the
    -- entity begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The category of the detected entity. Possible categories are
    -- MEDICAL_CONDITION, ANATOMY, or TEST_TREATMENT_PROCEDURE.
    category :: Prelude.Maybe SNOMEDCTEntityCategory,
    -- | The 0-based character offset in the input text that shows where the
    -- entity ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The numeric identifier for the entity. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | The SNOMED concepts that the entity could refer to, along with a score
    -- indicating the likelihood of the match.
    sNOMEDCTConcepts :: Prelude.Maybe [SNOMEDCTConcept],
    -- | The level of confidence that Comprehend Medical has in the accuracy of
    -- the detected entity.
    score :: Prelude.Maybe Prelude.Double,
    -- | The segment of input text extracted as this entity.
    text :: Prelude.Maybe Prelude.Text,
    -- | Contextual information for the entity.
    traits :: Prelude.Maybe [SNOMEDCTTrait],
    -- | Describes the specific type of entity with category of entities.
    -- Possible types include DX_NAME, ACUITY, DIRECTION, SYSTEM_ORGAN_SITE,
    -- TEST_NAME, TEST_VALUE, TEST_UNIT, PROCEDURE_NAME, or TREATMENT_NAME.
    type' :: Prelude.Maybe SNOMEDCTEntityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SNOMEDCTEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'sNOMEDCTEntity_attributes' - An extracted segment of the text that is an attribute of an entity, or
-- otherwise related to an entity, such as the dosage of a medication
-- taken.
--
-- 'beginOffset', 'sNOMEDCTEntity_beginOffset' - The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
--
-- 'category', 'sNOMEDCTEntity_category' - The category of the detected entity. Possible categories are
-- MEDICAL_CONDITION, ANATOMY, or TEST_TREATMENT_PROCEDURE.
--
-- 'endOffset', 'sNOMEDCTEntity_endOffset' - The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
--
-- 'id', 'sNOMEDCTEntity_id' - The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'sNOMEDCTConcepts', 'sNOMEDCTEntity_sNOMEDCTConcepts' - The SNOMED concepts that the entity could refer to, along with a score
-- indicating the likelihood of the match.
--
-- 'score', 'sNOMEDCTEntity_score' - The level of confidence that Comprehend Medical has in the accuracy of
-- the detected entity.
--
-- 'text', 'sNOMEDCTEntity_text' - The segment of input text extracted as this entity.
--
-- 'traits', 'sNOMEDCTEntity_traits' - Contextual information for the entity.
--
-- 'type'', 'sNOMEDCTEntity_type' - Describes the specific type of entity with category of entities.
-- Possible types include DX_NAME, ACUITY, DIRECTION, SYSTEM_ORGAN_SITE,
-- TEST_NAME, TEST_VALUE, TEST_UNIT, PROCEDURE_NAME, or TREATMENT_NAME.
newSNOMEDCTEntity ::
  SNOMEDCTEntity
newSNOMEDCTEntity =
  SNOMEDCTEntity'
    { attributes = Prelude.Nothing,
      beginOffset = Prelude.Nothing,
      category = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      id = Prelude.Nothing,
      sNOMEDCTConcepts = Prelude.Nothing,
      score = Prelude.Nothing,
      text = Prelude.Nothing,
      traits = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | An extracted segment of the text that is an attribute of an entity, or
-- otherwise related to an entity, such as the dosage of a medication
-- taken.
sNOMEDCTEntity_attributes :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe [SNOMEDCTAttribute])
sNOMEDCTEntity_attributes = Lens.lens (\SNOMEDCTEntity' {attributes} -> attributes) (\s@SNOMEDCTEntity' {} a -> s {attributes = a} :: SNOMEDCTEntity) Prelude.. Lens.mapping Lens.coerced

-- | The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
sNOMEDCTEntity_beginOffset :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Int)
sNOMEDCTEntity_beginOffset = Lens.lens (\SNOMEDCTEntity' {beginOffset} -> beginOffset) (\s@SNOMEDCTEntity' {} a -> s {beginOffset = a} :: SNOMEDCTEntity)

-- | The category of the detected entity. Possible categories are
-- MEDICAL_CONDITION, ANATOMY, or TEST_TREATMENT_PROCEDURE.
sNOMEDCTEntity_category :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe SNOMEDCTEntityCategory)
sNOMEDCTEntity_category = Lens.lens (\SNOMEDCTEntity' {category} -> category) (\s@SNOMEDCTEntity' {} a -> s {category = a} :: SNOMEDCTEntity)

-- | The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
sNOMEDCTEntity_endOffset :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Int)
sNOMEDCTEntity_endOffset = Lens.lens (\SNOMEDCTEntity' {endOffset} -> endOffset) (\s@SNOMEDCTEntity' {} a -> s {endOffset = a} :: SNOMEDCTEntity)

-- | The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
sNOMEDCTEntity_id :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Int)
sNOMEDCTEntity_id = Lens.lens (\SNOMEDCTEntity' {id} -> id) (\s@SNOMEDCTEntity' {} a -> s {id = a} :: SNOMEDCTEntity)

-- | The SNOMED concepts that the entity could refer to, along with a score
-- indicating the likelihood of the match.
sNOMEDCTEntity_sNOMEDCTConcepts :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe [SNOMEDCTConcept])
sNOMEDCTEntity_sNOMEDCTConcepts = Lens.lens (\SNOMEDCTEntity' {sNOMEDCTConcepts} -> sNOMEDCTConcepts) (\s@SNOMEDCTEntity' {} a -> s {sNOMEDCTConcepts = a} :: SNOMEDCTEntity) Prelude.. Lens.mapping Lens.coerced

-- | The level of confidence that Comprehend Medical has in the accuracy of
-- the detected entity.
sNOMEDCTEntity_score :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Double)
sNOMEDCTEntity_score = Lens.lens (\SNOMEDCTEntity' {score} -> score) (\s@SNOMEDCTEntity' {} a -> s {score = a} :: SNOMEDCTEntity)

-- | The segment of input text extracted as this entity.
sNOMEDCTEntity_text :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Text)
sNOMEDCTEntity_text = Lens.lens (\SNOMEDCTEntity' {text} -> text) (\s@SNOMEDCTEntity' {} a -> s {text = a} :: SNOMEDCTEntity)

-- | Contextual information for the entity.
sNOMEDCTEntity_traits :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe [SNOMEDCTTrait])
sNOMEDCTEntity_traits = Lens.lens (\SNOMEDCTEntity' {traits} -> traits) (\s@SNOMEDCTEntity' {} a -> s {traits = a} :: SNOMEDCTEntity) Prelude.. Lens.mapping Lens.coerced

-- | Describes the specific type of entity with category of entities.
-- Possible types include DX_NAME, ACUITY, DIRECTION, SYSTEM_ORGAN_SITE,
-- TEST_NAME, TEST_VALUE, TEST_UNIT, PROCEDURE_NAME, or TREATMENT_NAME.
sNOMEDCTEntity_type :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe SNOMEDCTEntityType)
sNOMEDCTEntity_type = Lens.lens (\SNOMEDCTEntity' {type'} -> type') (\s@SNOMEDCTEntity' {} a -> s {type' = a} :: SNOMEDCTEntity)

instance Data.FromJSON SNOMEDCTEntity where
  parseJSON =
    Data.withObject
      "SNOMEDCTEntity"
      ( \x ->
          SNOMEDCTEntity'
            Prelude.<$> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "Category")
            Prelude.<*> (x Data..:? "EndOffset")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> ( x
                            Data..:? "SNOMEDCTConcepts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..:? "Text")
            Prelude.<*> (x Data..:? "Traits" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable SNOMEDCTEntity where
  hashWithSalt _salt SNOMEDCTEntity' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sNOMEDCTConcepts
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SNOMEDCTEntity where
  rnf SNOMEDCTEntity' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf beginOffset `Prelude.seq`
        Prelude.rnf category `Prelude.seq`
          Prelude.rnf endOffset `Prelude.seq`
            Prelude.rnf id `Prelude.seq`
              Prelude.rnf sNOMEDCTConcepts `Prelude.seq`
                Prelude.rnf score `Prelude.seq`
                  Prelude.rnf text `Prelude.seq`
                    Prelude.rnf traits `Prelude.seq`
                      Prelude.rnf type'
