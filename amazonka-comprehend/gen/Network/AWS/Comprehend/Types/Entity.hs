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
-- Module      : Network.AWS.Comprehend.Types.Entity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.Entity where

import Network.AWS.Comprehend.Types.EntityType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about an entity.
--
-- /See:/ 'newEntity' smart constructor.
data Entity = Entity'
  { -- | A character offset in the input text that shows where the entity ends.
    -- The offset returns the position of each UTF-8 code point in the string.
    -- A /code point/ is the abstract character from a particular graphical
    -- representation. For example, a multi-byte UTF-8 character maps to a
    -- single code point.
    endOffset :: Core.Maybe Core.Int,
    -- | The entity\'s type.
    type' :: Core.Maybe EntityType,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Core.Maybe Core.Double,
    -- | The text of the entity.
    text :: Core.Maybe Core.Text,
    -- | A character offset in the input text that shows where the entity begins
    -- (the first character is at position 0). The offset returns the position
    -- of each UTF-8 code point in the string. A /code point/ is the abstract
    -- character from a particular graphical representation. For example, a
    -- multi-byte UTF-8 character maps to a single code point.
    beginOffset :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Entity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endOffset', 'entity_endOffset' - A character offset in the input text that shows where the entity ends.
-- The offset returns the position of each UTF-8 code point in the string.
-- A /code point/ is the abstract character from a particular graphical
-- representation. For example, a multi-byte UTF-8 character maps to a
-- single code point.
--
-- 'type'', 'entity_type' - The entity\'s type.
--
-- 'score', 'entity_score' - The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
--
-- 'text', 'entity_text' - The text of the entity.
--
-- 'beginOffset', 'entity_beginOffset' - A character offset in the input text that shows where the entity begins
-- (the first character is at position 0). The offset returns the position
-- of each UTF-8 code point in the string. A /code point/ is the abstract
-- character from a particular graphical representation. For example, a
-- multi-byte UTF-8 character maps to a single code point.
newEntity ::
  Entity
newEntity =
  Entity'
    { endOffset = Core.Nothing,
      type' = Core.Nothing,
      score = Core.Nothing,
      text = Core.Nothing,
      beginOffset = Core.Nothing
    }

-- | A character offset in the input text that shows where the entity ends.
-- The offset returns the position of each UTF-8 code point in the string.
-- A /code point/ is the abstract character from a particular graphical
-- representation. For example, a multi-byte UTF-8 character maps to a
-- single code point.
entity_endOffset :: Lens.Lens' Entity (Core.Maybe Core.Int)
entity_endOffset = Lens.lens (\Entity' {endOffset} -> endOffset) (\s@Entity' {} a -> s {endOffset = a} :: Entity)

-- | The entity\'s type.
entity_type :: Lens.Lens' Entity (Core.Maybe EntityType)
entity_type = Lens.lens (\Entity' {type'} -> type') (\s@Entity' {} a -> s {type' = a} :: Entity)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
entity_score :: Lens.Lens' Entity (Core.Maybe Core.Double)
entity_score = Lens.lens (\Entity' {score} -> score) (\s@Entity' {} a -> s {score = a} :: Entity)

-- | The text of the entity.
entity_text :: Lens.Lens' Entity (Core.Maybe Core.Text)
entity_text = Lens.lens (\Entity' {text} -> text) (\s@Entity' {} a -> s {text = a} :: Entity)

-- | A character offset in the input text that shows where the entity begins
-- (the first character is at position 0). The offset returns the position
-- of each UTF-8 code point in the string. A /code point/ is the abstract
-- character from a particular graphical representation. For example, a
-- multi-byte UTF-8 character maps to a single code point.
entity_beginOffset :: Lens.Lens' Entity (Core.Maybe Core.Int)
entity_beginOffset = Lens.lens (\Entity' {beginOffset} -> beginOffset) (\s@Entity' {} a -> s {beginOffset = a} :: Entity)

instance Core.FromJSON Entity where
  parseJSON =
    Core.withObject
      "Entity"
      ( \x ->
          Entity'
            Core.<$> (x Core..:? "EndOffset")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "Score")
            Core.<*> (x Core..:? "Text")
            Core.<*> (x Core..:? "BeginOffset")
      )

instance Core.Hashable Entity

instance Core.NFData Entity
