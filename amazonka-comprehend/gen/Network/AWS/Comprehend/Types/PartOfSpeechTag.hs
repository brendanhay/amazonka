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
-- Module      : Network.AWS.Comprehend.Types.PartOfSpeechTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PartOfSpeechTag where

import Network.AWS.Comprehend.Types.PartOfSpeechTagType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifies the part of speech represented by the token and gives the
-- confidence that Amazon Comprehend has that the part of speech was
-- correctly identified. For more information about the parts of speech
-- that Amazon Comprehend can identify, see how-syntax.
--
-- /See:/ 'newPartOfSpeechTag' smart constructor.
data PartOfSpeechTag = PartOfSpeechTag'
  { -- | The confidence that Amazon Comprehend has that the part of speech was
    -- correctly identified.
    score :: Core.Maybe Core.Double,
    -- | Identifies the part of speech that the token represents.
    tag :: Core.Maybe PartOfSpeechTagType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PartOfSpeechTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'score', 'partOfSpeechTag_score' - The confidence that Amazon Comprehend has that the part of speech was
-- correctly identified.
--
-- 'tag', 'partOfSpeechTag_tag' - Identifies the part of speech that the token represents.
newPartOfSpeechTag ::
  PartOfSpeechTag
newPartOfSpeechTag =
  PartOfSpeechTag'
    { score = Core.Nothing,
      tag = Core.Nothing
    }

-- | The confidence that Amazon Comprehend has that the part of speech was
-- correctly identified.
partOfSpeechTag_score :: Lens.Lens' PartOfSpeechTag (Core.Maybe Core.Double)
partOfSpeechTag_score = Lens.lens (\PartOfSpeechTag' {score} -> score) (\s@PartOfSpeechTag' {} a -> s {score = a} :: PartOfSpeechTag)

-- | Identifies the part of speech that the token represents.
partOfSpeechTag_tag :: Lens.Lens' PartOfSpeechTag (Core.Maybe PartOfSpeechTagType)
partOfSpeechTag_tag = Lens.lens (\PartOfSpeechTag' {tag} -> tag) (\s@PartOfSpeechTag' {} a -> s {tag = a} :: PartOfSpeechTag)

instance Core.FromJSON PartOfSpeechTag where
  parseJSON =
    Core.withObject
      "PartOfSpeechTag"
      ( \x ->
          PartOfSpeechTag'
            Core.<$> (x Core..:? "Score") Core.<*> (x Core..:? "Tag")
      )

instance Core.Hashable PartOfSpeechTag

instance Core.NFData PartOfSpeechTag
