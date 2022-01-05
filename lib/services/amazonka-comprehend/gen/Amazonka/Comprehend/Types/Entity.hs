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
-- Module      : Amazonka.Comprehend.Types.Entity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.Entity where

import Amazonka.Comprehend.Types.EntityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an entity.
--
-- /See:/ 'newEntity' smart constructor.
data Entity = Entity'
  { -- | A character offset in the input text that shows where the entity begins
    -- (the first character is at position 0). The offset returns the position
    -- of each UTF-8 code point in the string. A /code point/ is the abstract
    -- character from a particular graphical representation. For example, a
    -- multi-byte UTF-8 character maps to a single code point.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The text of the entity.
    text :: Prelude.Maybe Prelude.Text,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Prelude.Maybe Prelude.Double,
    -- | A character offset in the input text that shows where the entity ends.
    -- The offset returns the position of each UTF-8 code point in the string.
    -- A /code point/ is the abstract character from a particular graphical
    -- representation. For example, a multi-byte UTF-8 character maps to a
    -- single code point.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The entity\'s type.
    type' :: Prelude.Maybe EntityType
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
-- 'beginOffset', 'entity_beginOffset' - A character offset in the input text that shows where the entity begins
-- (the first character is at position 0). The offset returns the position
-- of each UTF-8 code point in the string. A /code point/ is the abstract
-- character from a particular graphical representation. For example, a
-- multi-byte UTF-8 character maps to a single code point.
--
-- 'text', 'entity_text' - The text of the entity.
--
-- 'score', 'entity_score' - The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
--
-- 'endOffset', 'entity_endOffset' - A character offset in the input text that shows where the entity ends.
-- The offset returns the position of each UTF-8 code point in the string.
-- A /code point/ is the abstract character from a particular graphical
-- representation. For example, a multi-byte UTF-8 character maps to a
-- single code point.
--
-- 'type'', 'entity_type' - The entity\'s type.
newEntity ::
  Entity
newEntity =
  Entity'
    { beginOffset = Prelude.Nothing,
      text = Prelude.Nothing,
      score = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A character offset in the input text that shows where the entity begins
-- (the first character is at position 0). The offset returns the position
-- of each UTF-8 code point in the string. A /code point/ is the abstract
-- character from a particular graphical representation. For example, a
-- multi-byte UTF-8 character maps to a single code point.
entity_beginOffset :: Lens.Lens' Entity (Prelude.Maybe Prelude.Int)
entity_beginOffset = Lens.lens (\Entity' {beginOffset} -> beginOffset) (\s@Entity' {} a -> s {beginOffset = a} :: Entity)

-- | The text of the entity.
entity_text :: Lens.Lens' Entity (Prelude.Maybe Prelude.Text)
entity_text = Lens.lens (\Entity' {text} -> text) (\s@Entity' {} a -> s {text = a} :: Entity)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
entity_score :: Lens.Lens' Entity (Prelude.Maybe Prelude.Double)
entity_score = Lens.lens (\Entity' {score} -> score) (\s@Entity' {} a -> s {score = a} :: Entity)

-- | A character offset in the input text that shows where the entity ends.
-- The offset returns the position of each UTF-8 code point in the string.
-- A /code point/ is the abstract character from a particular graphical
-- representation. For example, a multi-byte UTF-8 character maps to a
-- single code point.
entity_endOffset :: Lens.Lens' Entity (Prelude.Maybe Prelude.Int)
entity_endOffset = Lens.lens (\Entity' {endOffset} -> endOffset) (\s@Entity' {} a -> s {endOffset = a} :: Entity)

-- | The entity\'s type.
entity_type :: Lens.Lens' Entity (Prelude.Maybe EntityType)
entity_type = Lens.lens (\Entity' {type'} -> type') (\s@Entity' {} a -> s {type' = a} :: Entity)

instance Core.FromJSON Entity where
  parseJSON =
    Core.withObject
      "Entity"
      ( \x ->
          Entity'
            Prelude.<$> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Score")
            Prelude.<*> (x Core..:? "EndOffset")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable Entity where
  hashWithSalt _salt Entity' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Entity where
  rnf Entity' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf type'
