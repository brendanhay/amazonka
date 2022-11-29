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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

-- | The collection of medical entities extracted from the input text and
-- their associated information. For each entity, the response provides the
-- entity text, the entity category, where the entity text begins and ends,
-- and the level of confidence that Comprehend Medical has in the detection
-- and analysis. Attributes and traits of the entity are also returned.
--
-- /See:/ 'newSNOMEDCTEntity' smart constructor.
data SNOMEDCTEntity = SNOMEDCTEntity'
  { -- | The 0-based character offset in the input text that shows where the
    -- entity begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | Describes the specific type of entity with category of entities.
    -- Possible types include DX_NAME, ACUITY, DIRECTION, SYSTEM_ORGAN_SITE,
    -- TEST_NAME, TEST_VALUE, TEST_UNIT, PROCEDURE_NAME, or TREATMENT_NAME.
    type' :: Prelude.Maybe SNOMEDCTEntityType,
    -- | Contextual information for the entity.
    traits :: Prelude.Maybe [SNOMEDCTTrait],
    -- | The level of confidence that Comprehend Medical has in the accuracy of
    -- the detected entity.
    score :: Prelude.Maybe Prelude.Double,
    -- | The numeric identifier for the entity. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | The 0-based character offset in the input text that shows where the
    -- entity ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The category of the detected entity. Possible categories are
    -- MEDICAL_CONDITION, ANATOMY, or TEST_TREATMENT_PROCEDURE.
    category :: Prelude.Maybe SNOMEDCTEntityCategory,
    -- | An extracted segment of the text that is an attribute of an entity, or
    -- otherwise related to an entity, such as the dosage of a medication
    -- taken.
    attributes :: Prelude.Maybe [SNOMEDCTAttribute],
    -- | The segment of input text extracted as this entity.
    text :: Prelude.Maybe Prelude.Text,
    -- | The SNOMED concepts that the entity could refer to, along with a score
    -- indicating the likelihood of the match.
    sNOMEDCTConcepts :: Prelude.Maybe [SNOMEDCTConcept]
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
-- 'beginOffset', 'sNOMEDCTEntity_beginOffset' - The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
--
-- 'type'', 'sNOMEDCTEntity_type' - Describes the specific type of entity with category of entities.
-- Possible types include DX_NAME, ACUITY, DIRECTION, SYSTEM_ORGAN_SITE,
-- TEST_NAME, TEST_VALUE, TEST_UNIT, PROCEDURE_NAME, or TREATMENT_NAME.
--
-- 'traits', 'sNOMEDCTEntity_traits' - Contextual information for the entity.
--
-- 'score', 'sNOMEDCTEntity_score' - The level of confidence that Comprehend Medical has in the accuracy of
-- the detected entity.
--
-- 'id', 'sNOMEDCTEntity_id' - The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'endOffset', 'sNOMEDCTEntity_endOffset' - The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
--
-- 'category', 'sNOMEDCTEntity_category' - The category of the detected entity. Possible categories are
-- MEDICAL_CONDITION, ANATOMY, or TEST_TREATMENT_PROCEDURE.
--
-- 'attributes', 'sNOMEDCTEntity_attributes' - An extracted segment of the text that is an attribute of an entity, or
-- otherwise related to an entity, such as the dosage of a medication
-- taken.
--
-- 'text', 'sNOMEDCTEntity_text' - The segment of input text extracted as this entity.
--
-- 'sNOMEDCTConcepts', 'sNOMEDCTEntity_sNOMEDCTConcepts' - The SNOMED concepts that the entity could refer to, along with a score
-- indicating the likelihood of the match.
newSNOMEDCTEntity ::
  SNOMEDCTEntity
newSNOMEDCTEntity =
  SNOMEDCTEntity'
    { beginOffset = Prelude.Nothing,
      type' = Prelude.Nothing,
      traits = Prelude.Nothing,
      score = Prelude.Nothing,
      id = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      category = Prelude.Nothing,
      attributes = Prelude.Nothing,
      text = Prelude.Nothing,
      sNOMEDCTConcepts = Prelude.Nothing
    }

-- | The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
sNOMEDCTEntity_beginOffset :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Int)
sNOMEDCTEntity_beginOffset = Lens.lens (\SNOMEDCTEntity' {beginOffset} -> beginOffset) (\s@SNOMEDCTEntity' {} a -> s {beginOffset = a} :: SNOMEDCTEntity)

-- | Describes the specific type of entity with category of entities.
-- Possible types include DX_NAME, ACUITY, DIRECTION, SYSTEM_ORGAN_SITE,
-- TEST_NAME, TEST_VALUE, TEST_UNIT, PROCEDURE_NAME, or TREATMENT_NAME.
sNOMEDCTEntity_type :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe SNOMEDCTEntityType)
sNOMEDCTEntity_type = Lens.lens (\SNOMEDCTEntity' {type'} -> type') (\s@SNOMEDCTEntity' {} a -> s {type' = a} :: SNOMEDCTEntity)

-- | Contextual information for the entity.
sNOMEDCTEntity_traits :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe [SNOMEDCTTrait])
sNOMEDCTEntity_traits = Lens.lens (\SNOMEDCTEntity' {traits} -> traits) (\s@SNOMEDCTEntity' {} a -> s {traits = a} :: SNOMEDCTEntity) Prelude.. Lens.mapping Lens.coerced

-- | The level of confidence that Comprehend Medical has in the accuracy of
-- the detected entity.
sNOMEDCTEntity_score :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Double)
sNOMEDCTEntity_score = Lens.lens (\SNOMEDCTEntity' {score} -> score) (\s@SNOMEDCTEntity' {} a -> s {score = a} :: SNOMEDCTEntity)

-- | The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
sNOMEDCTEntity_id :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Int)
sNOMEDCTEntity_id = Lens.lens (\SNOMEDCTEntity' {id} -> id) (\s@SNOMEDCTEntity' {} a -> s {id = a} :: SNOMEDCTEntity)

-- | The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
sNOMEDCTEntity_endOffset :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Int)
sNOMEDCTEntity_endOffset = Lens.lens (\SNOMEDCTEntity' {endOffset} -> endOffset) (\s@SNOMEDCTEntity' {} a -> s {endOffset = a} :: SNOMEDCTEntity)

-- | The category of the detected entity. Possible categories are
-- MEDICAL_CONDITION, ANATOMY, or TEST_TREATMENT_PROCEDURE.
sNOMEDCTEntity_category :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe SNOMEDCTEntityCategory)
sNOMEDCTEntity_category = Lens.lens (\SNOMEDCTEntity' {category} -> category) (\s@SNOMEDCTEntity' {} a -> s {category = a} :: SNOMEDCTEntity)

-- | An extracted segment of the text that is an attribute of an entity, or
-- otherwise related to an entity, such as the dosage of a medication
-- taken.
sNOMEDCTEntity_attributes :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe [SNOMEDCTAttribute])
sNOMEDCTEntity_attributes = Lens.lens (\SNOMEDCTEntity' {attributes} -> attributes) (\s@SNOMEDCTEntity' {} a -> s {attributes = a} :: SNOMEDCTEntity) Prelude.. Lens.mapping Lens.coerced

-- | The segment of input text extracted as this entity.
sNOMEDCTEntity_text :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe Prelude.Text)
sNOMEDCTEntity_text = Lens.lens (\SNOMEDCTEntity' {text} -> text) (\s@SNOMEDCTEntity' {} a -> s {text = a} :: SNOMEDCTEntity)

-- | The SNOMED concepts that the entity could refer to, along with a score
-- indicating the likelihood of the match.
sNOMEDCTEntity_sNOMEDCTConcepts :: Lens.Lens' SNOMEDCTEntity (Prelude.Maybe [SNOMEDCTConcept])
sNOMEDCTEntity_sNOMEDCTConcepts = Lens.lens (\SNOMEDCTEntity' {sNOMEDCTConcepts} -> sNOMEDCTConcepts) (\s@SNOMEDCTEntity' {} a -> s {sNOMEDCTConcepts = a} :: SNOMEDCTEntity) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SNOMEDCTEntity where
  parseJSON =
    Core.withObject
      "SNOMEDCTEntity"
      ( \x ->
          SNOMEDCTEntity'
            Prelude.<$> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Traits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Score")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "EndOffset")
            Prelude.<*> (x Core..:? "Category")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> ( x Core..:? "SNOMEDCTConcepts"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SNOMEDCTEntity where
  hashWithSalt _salt SNOMEDCTEntity' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` sNOMEDCTConcepts

instance Prelude.NFData SNOMEDCTEntity where
  rnf SNOMEDCTEntity' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf traits
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf sNOMEDCTConcepts
