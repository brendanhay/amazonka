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
-- Module      : Network.AWS.Comprehend.Types.PiiEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntity where

import Network.AWS.Comprehend.Types.PiiEntityType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about a PII entity.
--
-- /See:/ 'newPiiEntity' smart constructor.
data PiiEntity = PiiEntity'
  { -- | A character offset in the input text that shows where the PII entity
    -- ends. The offset returns the position of each UTF-8 code point in the
    -- string. A /code point/ is the abstract character from a particular
    -- graphical representation. For example, a multi-byte UTF-8 character maps
    -- to a single code point.
    endOffset :: Core.Maybe Core.Int,
    -- | The entity\'s type.
    type' :: Core.Maybe PiiEntityType,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Core.Maybe Core.Double,
    -- | A character offset in the input text that shows where the PII entity
    -- begins (the first character is at position 0). The offset returns the
    -- position of each UTF-8 code point in the string. A /code point/ is the
    -- abstract character from a particular graphical representation. For
    -- example, a multi-byte UTF-8 character maps to a single code point.
    beginOffset :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PiiEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endOffset', 'piiEntity_endOffset' - A character offset in the input text that shows where the PII entity
-- ends. The offset returns the position of each UTF-8 code point in the
-- string. A /code point/ is the abstract character from a particular
-- graphical representation. For example, a multi-byte UTF-8 character maps
-- to a single code point.
--
-- 'type'', 'piiEntity_type' - The entity\'s type.
--
-- 'score', 'piiEntity_score' - The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
--
-- 'beginOffset', 'piiEntity_beginOffset' - A character offset in the input text that shows where the PII entity
-- begins (the first character is at position 0). The offset returns the
-- position of each UTF-8 code point in the string. A /code point/ is the
-- abstract character from a particular graphical representation. For
-- example, a multi-byte UTF-8 character maps to a single code point.
newPiiEntity ::
  PiiEntity
newPiiEntity =
  PiiEntity'
    { endOffset = Core.Nothing,
      type' = Core.Nothing,
      score = Core.Nothing,
      beginOffset = Core.Nothing
    }

-- | A character offset in the input text that shows where the PII entity
-- ends. The offset returns the position of each UTF-8 code point in the
-- string. A /code point/ is the abstract character from a particular
-- graphical representation. For example, a multi-byte UTF-8 character maps
-- to a single code point.
piiEntity_endOffset :: Lens.Lens' PiiEntity (Core.Maybe Core.Int)
piiEntity_endOffset = Lens.lens (\PiiEntity' {endOffset} -> endOffset) (\s@PiiEntity' {} a -> s {endOffset = a} :: PiiEntity)

-- | The entity\'s type.
piiEntity_type :: Lens.Lens' PiiEntity (Core.Maybe PiiEntityType)
piiEntity_type = Lens.lens (\PiiEntity' {type'} -> type') (\s@PiiEntity' {} a -> s {type' = a} :: PiiEntity)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
piiEntity_score :: Lens.Lens' PiiEntity (Core.Maybe Core.Double)
piiEntity_score = Lens.lens (\PiiEntity' {score} -> score) (\s@PiiEntity' {} a -> s {score = a} :: PiiEntity)

-- | A character offset in the input text that shows where the PII entity
-- begins (the first character is at position 0). The offset returns the
-- position of each UTF-8 code point in the string. A /code point/ is the
-- abstract character from a particular graphical representation. For
-- example, a multi-byte UTF-8 character maps to a single code point.
piiEntity_beginOffset :: Lens.Lens' PiiEntity (Core.Maybe Core.Int)
piiEntity_beginOffset = Lens.lens (\PiiEntity' {beginOffset} -> beginOffset) (\s@PiiEntity' {} a -> s {beginOffset = a} :: PiiEntity)

instance Core.FromJSON PiiEntity where
  parseJSON =
    Core.withObject
      "PiiEntity"
      ( \x ->
          PiiEntity'
            Core.<$> (x Core..:? "EndOffset")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "Score")
            Core.<*> (x Core..:? "BeginOffset")
      )

instance Core.Hashable PiiEntity

instance Core.NFData PiiEntity
