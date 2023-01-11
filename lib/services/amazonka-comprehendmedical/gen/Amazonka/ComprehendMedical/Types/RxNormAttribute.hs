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
-- Module      : Amazonka.ComprehendMedical.Types.RxNormAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.RxNormAttribute where

import Amazonka.ComprehendMedical.Types.RxNormAttributeType
import Amazonka.ComprehendMedical.Types.RxNormTrait
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The extracted attributes that relate to this entity. The attributes
-- recognized by InferRxNorm are @DOSAGE@, @DURATION@, @FORM@, @FREQUENCY@,
-- @RATE@, @ROUTE_OR_MODE@.
--
-- /See:/ 'newRxNormAttribute' smart constructor.
data RxNormAttribute = RxNormAttribute'
  { -- | The 0-based character offset in the input text that shows where the
    -- attribute begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The 0-based character offset in the input text that shows where the
    -- attribute ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The numeric identifier for this attribute. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | The level of confidence that Amazon Comprehend Medical has that the
    -- attribute is accurately linked to an entity.
    relationshipScore :: Prelude.Maybe Prelude.Double,
    -- | The level of confidence that Comprehend Medical has that the segment of
    -- text is correctly recognized as an attribute.
    score :: Prelude.Maybe Prelude.Double,
    -- | The segment of input text which corresponds to the detected attribute.
    text :: Prelude.Maybe Prelude.Text,
    -- | Contextual information for the attribute. InferRxNorm recognizes the
    -- trait @NEGATION@ for attributes, i.e. that the patient is not taking a
    -- specific dose or form of a medication.
    traits :: Prelude.Maybe [RxNormTrait],
    -- | The type of attribute. The types of attributes recognized by InferRxNorm
    -- are @BRAND_NAME@ and @GENERIC_NAME@.
    type' :: Prelude.Maybe RxNormAttributeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RxNormAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'rxNormAttribute_beginOffset' - The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
--
-- 'endOffset', 'rxNormAttribute_endOffset' - The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
--
-- 'id', 'rxNormAttribute_id' - The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'relationshipScore', 'rxNormAttribute_relationshipScore' - The level of confidence that Amazon Comprehend Medical has that the
-- attribute is accurately linked to an entity.
--
-- 'score', 'rxNormAttribute_score' - The level of confidence that Comprehend Medical has that the segment of
-- text is correctly recognized as an attribute.
--
-- 'text', 'rxNormAttribute_text' - The segment of input text which corresponds to the detected attribute.
--
-- 'traits', 'rxNormAttribute_traits' - Contextual information for the attribute. InferRxNorm recognizes the
-- trait @NEGATION@ for attributes, i.e. that the patient is not taking a
-- specific dose or form of a medication.
--
-- 'type'', 'rxNormAttribute_type' - The type of attribute. The types of attributes recognized by InferRxNorm
-- are @BRAND_NAME@ and @GENERIC_NAME@.
newRxNormAttribute ::
  RxNormAttribute
newRxNormAttribute =
  RxNormAttribute'
    { beginOffset = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      id = Prelude.Nothing,
      relationshipScore = Prelude.Nothing,
      score = Prelude.Nothing,
      text = Prelude.Nothing,
      traits = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
rxNormAttribute_beginOffset :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Int)
rxNormAttribute_beginOffset = Lens.lens (\RxNormAttribute' {beginOffset} -> beginOffset) (\s@RxNormAttribute' {} a -> s {beginOffset = a} :: RxNormAttribute)

-- | The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
rxNormAttribute_endOffset :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Int)
rxNormAttribute_endOffset = Lens.lens (\RxNormAttribute' {endOffset} -> endOffset) (\s@RxNormAttribute' {} a -> s {endOffset = a} :: RxNormAttribute)

-- | The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
rxNormAttribute_id :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Int)
rxNormAttribute_id = Lens.lens (\RxNormAttribute' {id} -> id) (\s@RxNormAttribute' {} a -> s {id = a} :: RxNormAttribute)

-- | The level of confidence that Amazon Comprehend Medical has that the
-- attribute is accurately linked to an entity.
rxNormAttribute_relationshipScore :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Double)
rxNormAttribute_relationshipScore = Lens.lens (\RxNormAttribute' {relationshipScore} -> relationshipScore) (\s@RxNormAttribute' {} a -> s {relationshipScore = a} :: RxNormAttribute)

-- | The level of confidence that Comprehend Medical has that the segment of
-- text is correctly recognized as an attribute.
rxNormAttribute_score :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Double)
rxNormAttribute_score = Lens.lens (\RxNormAttribute' {score} -> score) (\s@RxNormAttribute' {} a -> s {score = a} :: RxNormAttribute)

-- | The segment of input text which corresponds to the detected attribute.
rxNormAttribute_text :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Text)
rxNormAttribute_text = Lens.lens (\RxNormAttribute' {text} -> text) (\s@RxNormAttribute' {} a -> s {text = a} :: RxNormAttribute)

-- | Contextual information for the attribute. InferRxNorm recognizes the
-- trait @NEGATION@ for attributes, i.e. that the patient is not taking a
-- specific dose or form of a medication.
rxNormAttribute_traits :: Lens.Lens' RxNormAttribute (Prelude.Maybe [RxNormTrait])
rxNormAttribute_traits = Lens.lens (\RxNormAttribute' {traits} -> traits) (\s@RxNormAttribute' {} a -> s {traits = a} :: RxNormAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The type of attribute. The types of attributes recognized by InferRxNorm
-- are @BRAND_NAME@ and @GENERIC_NAME@.
rxNormAttribute_type :: Lens.Lens' RxNormAttribute (Prelude.Maybe RxNormAttributeType)
rxNormAttribute_type = Lens.lens (\RxNormAttribute' {type'} -> type') (\s@RxNormAttribute' {} a -> s {type' = a} :: RxNormAttribute)

instance Data.FromJSON RxNormAttribute where
  parseJSON =
    Data.withObject
      "RxNormAttribute"
      ( \x ->
          RxNormAttribute'
            Prelude.<$> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "EndOffset")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "RelationshipScore")
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..:? "Text")
            Prelude.<*> (x Data..:? "Traits" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable RxNormAttribute where
  hashWithSalt _salt RxNormAttribute' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` relationshipScore
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RxNormAttribute where
  rnf RxNormAttribute' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf relationshipScore
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf traits
      `Prelude.seq` Prelude.rnf type'
