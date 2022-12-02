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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.Entity where

import Amazonka.Comprehend.Types.EntityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an entity.
--
-- /See:/ 'newEntity' smart constructor.
data Entity = Entity'
  { -- | The zero-based offset from the beginning of the source text to the first
    -- character in the entity.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The entity\'s type.
    type' :: Prelude.Maybe EntityType,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Prelude.Maybe Prelude.Double,
    -- | The zero-based offset from the beginning of the source text to the last
    -- character in the entity.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The text of the entity.
    text :: Prelude.Maybe Prelude.Text
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
-- 'beginOffset', 'entity_beginOffset' - The zero-based offset from the beginning of the source text to the first
-- character in the entity.
--
-- 'type'', 'entity_type' - The entity\'s type.
--
-- 'score', 'entity_score' - The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
--
-- 'endOffset', 'entity_endOffset' - The zero-based offset from the beginning of the source text to the last
-- character in the entity.
--
-- 'text', 'entity_text' - The text of the entity.
newEntity ::
  Entity
newEntity =
  Entity'
    { beginOffset = Prelude.Nothing,
      type' = Prelude.Nothing,
      score = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The zero-based offset from the beginning of the source text to the first
-- character in the entity.
entity_beginOffset :: Lens.Lens' Entity (Prelude.Maybe Prelude.Int)
entity_beginOffset = Lens.lens (\Entity' {beginOffset} -> beginOffset) (\s@Entity' {} a -> s {beginOffset = a} :: Entity)

-- | The entity\'s type.
entity_type :: Lens.Lens' Entity (Prelude.Maybe EntityType)
entity_type = Lens.lens (\Entity' {type'} -> type') (\s@Entity' {} a -> s {type' = a} :: Entity)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
entity_score :: Lens.Lens' Entity (Prelude.Maybe Prelude.Double)
entity_score = Lens.lens (\Entity' {score} -> score) (\s@Entity' {} a -> s {score = a} :: Entity)

-- | The zero-based offset from the beginning of the source text to the last
-- character in the entity.
entity_endOffset :: Lens.Lens' Entity (Prelude.Maybe Prelude.Int)
entity_endOffset = Lens.lens (\Entity' {endOffset} -> endOffset) (\s@Entity' {} a -> s {endOffset = a} :: Entity)

-- | The text of the entity.
entity_text :: Lens.Lens' Entity (Prelude.Maybe Prelude.Text)
entity_text = Lens.lens (\Entity' {text} -> text) (\s@Entity' {} a -> s {text = a} :: Entity)

instance Data.FromJSON Entity where
  parseJSON =
    Data.withObject
      "Entity"
      ( \x ->
          Entity'
            Prelude.<$> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..:? "EndOffset")
            Prelude.<*> (x Data..:? "Text")
      )

instance Prelude.Hashable Entity where
  hashWithSalt _salt Entity' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` text

instance Prelude.NFData Entity where
  rnf Entity' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf text
