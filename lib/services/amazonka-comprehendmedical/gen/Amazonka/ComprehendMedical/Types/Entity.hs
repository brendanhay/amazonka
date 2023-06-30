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
-- Module      : Amazonka.ComprehendMedical.Types.Entity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.Entity where

import Amazonka.ComprehendMedical.Types.Attribute
import Amazonka.ComprehendMedical.Types.EntitySubType
import Amazonka.ComprehendMedical.Types.EntityType
import Amazonka.ComprehendMedical.Types.Trait
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an extracted medical entity.
--
-- /See:/ 'newEntity' smart constructor.
data Entity = Entity'
  { -- | The extracted attributes that relate to this entity.
    attributes :: Prelude.Maybe [Attribute],
    -- | The 0-based character offset in the input text that shows where the
    -- entity begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The category of the entity.
    category :: Prelude.Maybe EntityType,
    -- | The 0-based character offset in the input text that shows where the
    -- entity ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The numeric identifier for the entity. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | The level of confidence that Comprehend Medical; has in the accuracy of
    -- the detection.
    score :: Prelude.Maybe Prelude.Double,
    -- | The segment of input text extracted as this entity.
    text :: Prelude.Maybe Prelude.Text,
    -- | Contextual information for the entity.
    traits :: Prelude.Maybe [Trait],
    -- | Describes the specific type of entity with category of entities.
    type' :: Prelude.Maybe EntitySubType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Entity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'entity_attributes' - The extracted attributes that relate to this entity.
--
-- 'beginOffset', 'entity_beginOffset' - The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
--
-- 'category', 'entity_category' - The category of the entity.
--
-- 'endOffset', 'entity_endOffset' - The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
--
-- 'id', 'entity_id' - The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'score', 'entity_score' - The level of confidence that Comprehend Medical; has in the accuracy of
-- the detection.
--
-- 'text', 'entity_text' - The segment of input text extracted as this entity.
--
-- 'traits', 'entity_traits' - Contextual information for the entity.
--
-- 'type'', 'entity_type' - Describes the specific type of entity with category of entities.
newEntity ::
  Entity
newEntity =
  Entity'
    { attributes = Prelude.Nothing,
      beginOffset = Prelude.Nothing,
      category = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      id = Prelude.Nothing,
      score = Prelude.Nothing,
      text = Prelude.Nothing,
      traits = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The extracted attributes that relate to this entity.
entity_attributes :: Lens.Lens' Entity (Prelude.Maybe [Attribute])
entity_attributes = Lens.lens (\Entity' {attributes} -> attributes) (\s@Entity' {} a -> s {attributes = a} :: Entity) Prelude.. Lens.mapping Lens.coerced

-- | The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
entity_beginOffset :: Lens.Lens' Entity (Prelude.Maybe Prelude.Int)
entity_beginOffset = Lens.lens (\Entity' {beginOffset} -> beginOffset) (\s@Entity' {} a -> s {beginOffset = a} :: Entity)

-- | The category of the entity.
entity_category :: Lens.Lens' Entity (Prelude.Maybe EntityType)
entity_category = Lens.lens (\Entity' {category} -> category) (\s@Entity' {} a -> s {category = a} :: Entity)

-- | The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
entity_endOffset :: Lens.Lens' Entity (Prelude.Maybe Prelude.Int)
entity_endOffset = Lens.lens (\Entity' {endOffset} -> endOffset) (\s@Entity' {} a -> s {endOffset = a} :: Entity)

-- | The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
entity_id :: Lens.Lens' Entity (Prelude.Maybe Prelude.Int)
entity_id = Lens.lens (\Entity' {id} -> id) (\s@Entity' {} a -> s {id = a} :: Entity)

-- | The level of confidence that Comprehend Medical; has in the accuracy of
-- the detection.
entity_score :: Lens.Lens' Entity (Prelude.Maybe Prelude.Double)
entity_score = Lens.lens (\Entity' {score} -> score) (\s@Entity' {} a -> s {score = a} :: Entity)

-- | The segment of input text extracted as this entity.
entity_text :: Lens.Lens' Entity (Prelude.Maybe Prelude.Text)
entity_text = Lens.lens (\Entity' {text} -> text) (\s@Entity' {} a -> s {text = a} :: Entity)

-- | Contextual information for the entity.
entity_traits :: Lens.Lens' Entity (Prelude.Maybe [Trait])
entity_traits = Lens.lens (\Entity' {traits} -> traits) (\s@Entity' {} a -> s {traits = a} :: Entity) Prelude.. Lens.mapping Lens.coerced

-- | Describes the specific type of entity with category of entities.
entity_type :: Lens.Lens' Entity (Prelude.Maybe EntitySubType)
entity_type = Lens.lens (\Entity' {type'} -> type') (\s@Entity' {} a -> s {type' = a} :: Entity)

instance Data.FromJSON Entity where
  parseJSON =
    Data.withObject
      "Entity"
      ( \x ->
          Entity'
            Prelude.<$> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "Category")
            Prelude.<*> (x Data..:? "EndOffset")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..:? "Text")
            Prelude.<*> (x Data..:? "Traits" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Entity where
  hashWithSalt _salt Entity' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Entity where
  rnf Entity' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf traits
      `Prelude.seq` Prelude.rnf type'
