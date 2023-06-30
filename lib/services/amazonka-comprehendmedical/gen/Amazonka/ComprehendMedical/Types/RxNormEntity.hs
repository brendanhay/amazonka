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
-- Module      : Amazonka.ComprehendMedical.Types.RxNormEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.RxNormEntity where

import Amazonka.ComprehendMedical.Types.RxNormAttribute
import Amazonka.ComprehendMedical.Types.RxNormConcept
import Amazonka.ComprehendMedical.Types.RxNormEntityCategory
import Amazonka.ComprehendMedical.Types.RxNormEntityType
import Amazonka.ComprehendMedical.Types.RxNormTrait
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The collection of medical entities extracted from the input text and
-- their associated information. For each entity, the response provides the
-- entity text, the entity category, where the entity text begins and ends,
-- and the level of confidence that Amazon Comprehend Medical has in the
-- detection and analysis. Attributes and traits of the entity are also
-- returned.
--
-- /See:/ 'newRxNormEntity' smart constructor.
data RxNormEntity = RxNormEntity'
  { -- | The extracted attributes that relate to the entity. The attributes
    -- recognized by InferRxNorm are @DOSAGE@, @DURATION@, @FORM@, @FREQUENCY@,
    -- @RATE@, @ROUTE_OR_MODE@, and @STRENGTH@.
    attributes :: Prelude.Maybe [RxNormAttribute],
    -- | The 0-based character offset in the input text that shows where the
    -- entity begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The category of the entity. The recognized categories are @GENERIC@ or
    -- @BRAND_NAME@.
    category :: Prelude.Maybe RxNormEntityCategory,
    -- | The 0-based character offset in the input text that shows where the
    -- entity ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The numeric identifier for the entity. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | The RxNorm concepts that the entity could refer to, along with a score
    -- indicating the likelihood of the match.
    rxNormConcepts :: Prelude.Maybe [RxNormConcept],
    -- | The level of confidence that Amazon Comprehend Medical has in the
    -- accuracy of the detected entity.
    score :: Prelude.Maybe Prelude.Double,
    -- | The segment of input text extracted from which the entity was detected.
    text :: Prelude.Maybe Prelude.Text,
    -- | Contextual information for the entity.
    traits :: Prelude.Maybe [RxNormTrait],
    -- | Describes the specific type of entity. For InferRxNorm, the recognized
    -- entity type is @MEDICATION@.
    type' :: Prelude.Maybe RxNormEntityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RxNormEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'rxNormEntity_attributes' - The extracted attributes that relate to the entity. The attributes
-- recognized by InferRxNorm are @DOSAGE@, @DURATION@, @FORM@, @FREQUENCY@,
-- @RATE@, @ROUTE_OR_MODE@, and @STRENGTH@.
--
-- 'beginOffset', 'rxNormEntity_beginOffset' - The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
--
-- 'category', 'rxNormEntity_category' - The category of the entity. The recognized categories are @GENERIC@ or
-- @BRAND_NAME@.
--
-- 'endOffset', 'rxNormEntity_endOffset' - The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
--
-- 'id', 'rxNormEntity_id' - The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'rxNormConcepts', 'rxNormEntity_rxNormConcepts' - The RxNorm concepts that the entity could refer to, along with a score
-- indicating the likelihood of the match.
--
-- 'score', 'rxNormEntity_score' - The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detected entity.
--
-- 'text', 'rxNormEntity_text' - The segment of input text extracted from which the entity was detected.
--
-- 'traits', 'rxNormEntity_traits' - Contextual information for the entity.
--
-- 'type'', 'rxNormEntity_type' - Describes the specific type of entity. For InferRxNorm, the recognized
-- entity type is @MEDICATION@.
newRxNormEntity ::
  RxNormEntity
newRxNormEntity =
  RxNormEntity'
    { attributes = Prelude.Nothing,
      beginOffset = Prelude.Nothing,
      category = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      id = Prelude.Nothing,
      rxNormConcepts = Prelude.Nothing,
      score = Prelude.Nothing,
      text = Prelude.Nothing,
      traits = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The extracted attributes that relate to the entity. The attributes
-- recognized by InferRxNorm are @DOSAGE@, @DURATION@, @FORM@, @FREQUENCY@,
-- @RATE@, @ROUTE_OR_MODE@, and @STRENGTH@.
rxNormEntity_attributes :: Lens.Lens' RxNormEntity (Prelude.Maybe [RxNormAttribute])
rxNormEntity_attributes = Lens.lens (\RxNormEntity' {attributes} -> attributes) (\s@RxNormEntity' {} a -> s {attributes = a} :: RxNormEntity) Prelude.. Lens.mapping Lens.coerced

-- | The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
rxNormEntity_beginOffset :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Int)
rxNormEntity_beginOffset = Lens.lens (\RxNormEntity' {beginOffset} -> beginOffset) (\s@RxNormEntity' {} a -> s {beginOffset = a} :: RxNormEntity)

-- | The category of the entity. The recognized categories are @GENERIC@ or
-- @BRAND_NAME@.
rxNormEntity_category :: Lens.Lens' RxNormEntity (Prelude.Maybe RxNormEntityCategory)
rxNormEntity_category = Lens.lens (\RxNormEntity' {category} -> category) (\s@RxNormEntity' {} a -> s {category = a} :: RxNormEntity)

-- | The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
rxNormEntity_endOffset :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Int)
rxNormEntity_endOffset = Lens.lens (\RxNormEntity' {endOffset} -> endOffset) (\s@RxNormEntity' {} a -> s {endOffset = a} :: RxNormEntity)

-- | The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
rxNormEntity_id :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Int)
rxNormEntity_id = Lens.lens (\RxNormEntity' {id} -> id) (\s@RxNormEntity' {} a -> s {id = a} :: RxNormEntity)

-- | The RxNorm concepts that the entity could refer to, along with a score
-- indicating the likelihood of the match.
rxNormEntity_rxNormConcepts :: Lens.Lens' RxNormEntity (Prelude.Maybe [RxNormConcept])
rxNormEntity_rxNormConcepts = Lens.lens (\RxNormEntity' {rxNormConcepts} -> rxNormConcepts) (\s@RxNormEntity' {} a -> s {rxNormConcepts = a} :: RxNormEntity) Prelude.. Lens.mapping Lens.coerced

-- | The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detected entity.
rxNormEntity_score :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Double)
rxNormEntity_score = Lens.lens (\RxNormEntity' {score} -> score) (\s@RxNormEntity' {} a -> s {score = a} :: RxNormEntity)

-- | The segment of input text extracted from which the entity was detected.
rxNormEntity_text :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Text)
rxNormEntity_text = Lens.lens (\RxNormEntity' {text} -> text) (\s@RxNormEntity' {} a -> s {text = a} :: RxNormEntity)

-- | Contextual information for the entity.
rxNormEntity_traits :: Lens.Lens' RxNormEntity (Prelude.Maybe [RxNormTrait])
rxNormEntity_traits = Lens.lens (\RxNormEntity' {traits} -> traits) (\s@RxNormEntity' {} a -> s {traits = a} :: RxNormEntity) Prelude.. Lens.mapping Lens.coerced

-- | Describes the specific type of entity. For InferRxNorm, the recognized
-- entity type is @MEDICATION@.
rxNormEntity_type :: Lens.Lens' RxNormEntity (Prelude.Maybe RxNormEntityType)
rxNormEntity_type = Lens.lens (\RxNormEntity' {type'} -> type') (\s@RxNormEntity' {} a -> s {type' = a} :: RxNormEntity)

instance Data.FromJSON RxNormEntity where
  parseJSON =
    Data.withObject
      "RxNormEntity"
      ( \x ->
          RxNormEntity'
            Prelude.<$> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "Category")
            Prelude.<*> (x Data..:? "EndOffset")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "RxNormConcepts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..:? "Text")
            Prelude.<*> (x Data..:? "Traits" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable RxNormEntity where
  hashWithSalt _salt RxNormEntity' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` rxNormConcepts
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RxNormEntity where
  rnf RxNormEntity' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf rxNormConcepts
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf traits
      `Prelude.seq` Prelude.rnf type'
