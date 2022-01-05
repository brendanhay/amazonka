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
-- Module      : Amazonka.ComprehendMedical.Types.Attribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.Attribute where

import Amazonka.ComprehendMedical.Types.EntitySubType
import Amazonka.ComprehendMedical.Types.EntityType
import Amazonka.ComprehendMedical.Types.RelationshipType
import Amazonka.ComprehendMedical.Types.Trait
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An extracted segment of the text that is an attribute of an entity, or
-- otherwise related to an entity, such as the dosage of a medication
-- taken. It contains information about the attribute such as id, begin and
-- end offset within the input text, and the segment of the input text.
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The level of confidence that Amazon Comprehend Medical has that this
    -- attribute is correctly related to this entity.
    relationshipScore :: Prelude.Maybe Prelude.Double,
    -- | The 0-based character offset in the input text that shows where the
    -- attribute begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The segment of input text extracted as this attribute.
    text :: Prelude.Maybe Prelude.Text,
    -- | The category of attribute.
    category :: Prelude.Maybe EntityType,
    -- | The level of confidence that Amazon Comprehend Medical has that the
    -- segment of text is correctly recognized as an attribute.
    score :: Prelude.Maybe Prelude.Double,
    -- | Contextual information for this attribute.
    traits :: Prelude.Maybe [Trait],
    -- | The type of relationship between the entity and attribute. Type for the
    -- relationship is @OVERLAP@, indicating that the entity occurred at the
    -- same time as the @Date_Expression@.
    relationshipType :: Prelude.Maybe RelationshipType,
    -- | The 0-based character offset in the input text that shows where the
    -- attribute ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The numeric identifier for this attribute. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | The type of attribute.
    type' :: Prelude.Maybe EntitySubType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationshipScore', 'attribute_relationshipScore' - The level of confidence that Amazon Comprehend Medical has that this
-- attribute is correctly related to this entity.
--
-- 'beginOffset', 'attribute_beginOffset' - The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
--
-- 'text', 'attribute_text' - The segment of input text extracted as this attribute.
--
-- 'category', 'attribute_category' - The category of attribute.
--
-- 'score', 'attribute_score' - The level of confidence that Amazon Comprehend Medical has that the
-- segment of text is correctly recognized as an attribute.
--
-- 'traits', 'attribute_traits' - Contextual information for this attribute.
--
-- 'relationshipType', 'attribute_relationshipType' - The type of relationship between the entity and attribute. Type for the
-- relationship is @OVERLAP@, indicating that the entity occurred at the
-- same time as the @Date_Expression@.
--
-- 'endOffset', 'attribute_endOffset' - The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
--
-- 'id', 'attribute_id' - The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'type'', 'attribute_type' - The type of attribute.
newAttribute ::
  Attribute
newAttribute =
  Attribute'
    { relationshipScore = Prelude.Nothing,
      beginOffset = Prelude.Nothing,
      text = Prelude.Nothing,
      category = Prelude.Nothing,
      score = Prelude.Nothing,
      traits = Prelude.Nothing,
      relationshipType = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The level of confidence that Amazon Comprehend Medical has that this
-- attribute is correctly related to this entity.
attribute_relationshipScore :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Double)
attribute_relationshipScore = Lens.lens (\Attribute' {relationshipScore} -> relationshipScore) (\s@Attribute' {} a -> s {relationshipScore = a} :: Attribute)

-- | The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
attribute_beginOffset :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Int)
attribute_beginOffset = Lens.lens (\Attribute' {beginOffset} -> beginOffset) (\s@Attribute' {} a -> s {beginOffset = a} :: Attribute)

-- | The segment of input text extracted as this attribute.
attribute_text :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Text)
attribute_text = Lens.lens (\Attribute' {text} -> text) (\s@Attribute' {} a -> s {text = a} :: Attribute)

-- | The category of attribute.
attribute_category :: Lens.Lens' Attribute (Prelude.Maybe EntityType)
attribute_category = Lens.lens (\Attribute' {category} -> category) (\s@Attribute' {} a -> s {category = a} :: Attribute)

-- | The level of confidence that Amazon Comprehend Medical has that the
-- segment of text is correctly recognized as an attribute.
attribute_score :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Double)
attribute_score = Lens.lens (\Attribute' {score} -> score) (\s@Attribute' {} a -> s {score = a} :: Attribute)

-- | Contextual information for this attribute.
attribute_traits :: Lens.Lens' Attribute (Prelude.Maybe [Trait])
attribute_traits = Lens.lens (\Attribute' {traits} -> traits) (\s@Attribute' {} a -> s {traits = a} :: Attribute) Prelude.. Lens.mapping Lens.coerced

-- | The type of relationship between the entity and attribute. Type for the
-- relationship is @OVERLAP@, indicating that the entity occurred at the
-- same time as the @Date_Expression@.
attribute_relationshipType :: Lens.Lens' Attribute (Prelude.Maybe RelationshipType)
attribute_relationshipType = Lens.lens (\Attribute' {relationshipType} -> relationshipType) (\s@Attribute' {} a -> s {relationshipType = a} :: Attribute)

-- | The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
attribute_endOffset :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Int)
attribute_endOffset = Lens.lens (\Attribute' {endOffset} -> endOffset) (\s@Attribute' {} a -> s {endOffset = a} :: Attribute)

-- | The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
attribute_id :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Int)
attribute_id = Lens.lens (\Attribute' {id} -> id) (\s@Attribute' {} a -> s {id = a} :: Attribute)

-- | The type of attribute.
attribute_type :: Lens.Lens' Attribute (Prelude.Maybe EntitySubType)
attribute_type = Lens.lens (\Attribute' {type'} -> type') (\s@Attribute' {} a -> s {type' = a} :: Attribute)

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Prelude.<$> (x Core..:? "RelationshipScore")
            Prelude.<*> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Category")
            Prelude.<*> (x Core..:? "Score")
            Prelude.<*> (x Core..:? "Traits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RelationshipType")
            Prelude.<*> (x Core..:? "EndOffset")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable Attribute where
  hashWithSalt _salt Attribute' {..} =
    _salt `Prelude.hashWithSalt` relationshipScore
      `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` relationshipType
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Attribute where
  rnf Attribute' {..} =
    Prelude.rnf relationshipScore
      `Prelude.seq` Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf traits
      `Prelude.seq` Prelude.rnf relationshipType
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf type'
