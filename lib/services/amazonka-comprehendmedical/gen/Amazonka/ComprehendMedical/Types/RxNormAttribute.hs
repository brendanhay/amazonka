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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.RxNormAttribute where

import Amazonka.ComprehendMedical.Types.RxNormAttributeType
import Amazonka.ComprehendMedical.Types.RxNormTrait
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The extracted attributes that relate to this entity. The attributes
-- recognized by InferRxNorm are @DOSAGE@, @DURATION@, @FORM@, @FREQUENCY@,
-- @RATE@, @ROUTE_OR_MODE@.
--
-- /See:/ 'newRxNormAttribute' smart constructor.
data RxNormAttribute = RxNormAttribute'
  { -- | The level of confidence that Amazon Comprehend Medical has that the
    -- attribute is accurately linked to an entity.
    relationshipScore :: Prelude.Maybe Prelude.Double,
    -- | The 0-based character offset in the input text that shows where the
    -- attribute begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The segment of input text which corresponds to the detected attribute.
    text :: Prelude.Maybe Prelude.Text,
    -- | The level of confidence that Comprehend Medical has that the segment of
    -- text is correctly recognized as an attribute.
    score :: Prelude.Maybe Prelude.Double,
    -- | Contextual information for the attribute. InferRxNorm recognizes the
    -- trait @NEGATION@ for attributes, i.e. that the patient is not taking a
    -- specific dose or form of a medication.
    traits :: Prelude.Maybe [RxNormTrait],
    -- | The 0-based character offset in the input text that shows where the
    -- attribute ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The numeric identifier for this attribute. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
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
-- 'relationshipScore', 'rxNormAttribute_relationshipScore' - The level of confidence that Amazon Comprehend Medical has that the
-- attribute is accurately linked to an entity.
--
-- 'beginOffset', 'rxNormAttribute_beginOffset' - The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
--
-- 'text', 'rxNormAttribute_text' - The segment of input text which corresponds to the detected attribute.
--
-- 'score', 'rxNormAttribute_score' - The level of confidence that Comprehend Medical has that the segment of
-- text is correctly recognized as an attribute.
--
-- 'traits', 'rxNormAttribute_traits' - Contextual information for the attribute. InferRxNorm recognizes the
-- trait @NEGATION@ for attributes, i.e. that the patient is not taking a
-- specific dose or form of a medication.
--
-- 'endOffset', 'rxNormAttribute_endOffset' - The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
--
-- 'id', 'rxNormAttribute_id' - The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'type'', 'rxNormAttribute_type' - The type of attribute. The types of attributes recognized by InferRxNorm
-- are @BRAND_NAME@ and @GENERIC_NAME@.
newRxNormAttribute ::
  RxNormAttribute
newRxNormAttribute =
  RxNormAttribute'
    { relationshipScore =
        Prelude.Nothing,
      beginOffset = Prelude.Nothing,
      text = Prelude.Nothing,
      score = Prelude.Nothing,
      traits = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The level of confidence that Amazon Comprehend Medical has that the
-- attribute is accurately linked to an entity.
rxNormAttribute_relationshipScore :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Double)
rxNormAttribute_relationshipScore = Lens.lens (\RxNormAttribute' {relationshipScore} -> relationshipScore) (\s@RxNormAttribute' {} a -> s {relationshipScore = a} :: RxNormAttribute)

-- | The 0-based character offset in the input text that shows where the
-- attribute begins. The offset returns the UTF-8 code point in the string.
rxNormAttribute_beginOffset :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Int)
rxNormAttribute_beginOffset = Lens.lens (\RxNormAttribute' {beginOffset} -> beginOffset) (\s@RxNormAttribute' {} a -> s {beginOffset = a} :: RxNormAttribute)

-- | The segment of input text which corresponds to the detected attribute.
rxNormAttribute_text :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Text)
rxNormAttribute_text = Lens.lens (\RxNormAttribute' {text} -> text) (\s@RxNormAttribute' {} a -> s {text = a} :: RxNormAttribute)

-- | The level of confidence that Comprehend Medical has that the segment of
-- text is correctly recognized as an attribute.
rxNormAttribute_score :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Double)
rxNormAttribute_score = Lens.lens (\RxNormAttribute' {score} -> score) (\s@RxNormAttribute' {} a -> s {score = a} :: RxNormAttribute)

-- | Contextual information for the attribute. InferRxNorm recognizes the
-- trait @NEGATION@ for attributes, i.e. that the patient is not taking a
-- specific dose or form of a medication.
rxNormAttribute_traits :: Lens.Lens' RxNormAttribute (Prelude.Maybe [RxNormTrait])
rxNormAttribute_traits = Lens.lens (\RxNormAttribute' {traits} -> traits) (\s@RxNormAttribute' {} a -> s {traits = a} :: RxNormAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The 0-based character offset in the input text that shows where the
-- attribute ends. The offset returns the UTF-8 code point in the string.
rxNormAttribute_endOffset :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Int)
rxNormAttribute_endOffset = Lens.lens (\RxNormAttribute' {endOffset} -> endOffset) (\s@RxNormAttribute' {} a -> s {endOffset = a} :: RxNormAttribute)

-- | The numeric identifier for this attribute. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
rxNormAttribute_id :: Lens.Lens' RxNormAttribute (Prelude.Maybe Prelude.Int)
rxNormAttribute_id = Lens.lens (\RxNormAttribute' {id} -> id) (\s@RxNormAttribute' {} a -> s {id = a} :: RxNormAttribute)

-- | The type of attribute. The types of attributes recognized by InferRxNorm
-- are @BRAND_NAME@ and @GENERIC_NAME@.
rxNormAttribute_type :: Lens.Lens' RxNormAttribute (Prelude.Maybe RxNormAttributeType)
rxNormAttribute_type = Lens.lens (\RxNormAttribute' {type'} -> type') (\s@RxNormAttribute' {} a -> s {type' = a} :: RxNormAttribute)

instance Core.FromJSON RxNormAttribute where
  parseJSON =
    Core.withObject
      "RxNormAttribute"
      ( \x ->
          RxNormAttribute'
            Prelude.<$> (x Core..:? "RelationshipScore")
            Prelude.<*> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Score")
            Prelude.<*> (x Core..:? "Traits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EndOffset")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable RxNormAttribute where
  hashWithSalt salt' RxNormAttribute' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` relationshipScore

instance Prelude.NFData RxNormAttribute where
  rnf RxNormAttribute' {..} =
    Prelude.rnf relationshipScore
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf traits
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf beginOffset
