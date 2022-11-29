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
-- Module      : Amazonka.Comprehend.Types.TargetedSentimentMention
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.TargetedSentimentMention where

import Amazonka.Comprehend.Types.MentionSentiment
import Amazonka.Comprehend.Types.TargetedSentimentEntityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about one mention of an entity. The mention information
-- includes the location of the mention in the text and the sentiment of
-- the mention.
--
-- For more information about targeted sentiment, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html Targeted sentiment>.
--
-- /See:/ 'newTargetedSentimentMention' smart constructor.
data TargetedSentimentMention = TargetedSentimentMention'
  { -- | The offset into the document text where the mention begins.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The type of the entity. Amazon Comprehend supports a variety of
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html#how-targeted-sentiment-entities entity types>.
    type' :: Prelude.Maybe TargetedSentimentEntityType,
    -- | Model confidence that the entity is relevant. Value range is zero to
    -- one, where one is highest confidence.
    score :: Prelude.Maybe Prelude.Double,
    -- | The offset into the document text where the mention ends.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | Contains the sentiment and sentiment score for the mention.
    mentionSentiment :: Prelude.Maybe MentionSentiment,
    -- | The confidence that all the entities mentioned in the group relate to
    -- the same entity.
    groupScore :: Prelude.Maybe Prelude.Double,
    -- | The text in the document that identifies the entity.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetedSentimentMention' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'targetedSentimentMention_beginOffset' - The offset into the document text where the mention begins.
--
-- 'type'', 'targetedSentimentMention_type' - The type of the entity. Amazon Comprehend supports a variety of
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html#how-targeted-sentiment-entities entity types>.
--
-- 'score', 'targetedSentimentMention_score' - Model confidence that the entity is relevant. Value range is zero to
-- one, where one is highest confidence.
--
-- 'endOffset', 'targetedSentimentMention_endOffset' - The offset into the document text where the mention ends.
--
-- 'mentionSentiment', 'targetedSentimentMention_mentionSentiment' - Contains the sentiment and sentiment score for the mention.
--
-- 'groupScore', 'targetedSentimentMention_groupScore' - The confidence that all the entities mentioned in the group relate to
-- the same entity.
--
-- 'text', 'targetedSentimentMention_text' - The text in the document that identifies the entity.
newTargetedSentimentMention ::
  TargetedSentimentMention
newTargetedSentimentMention =
  TargetedSentimentMention'
    { beginOffset =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      score = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      mentionSentiment = Prelude.Nothing,
      groupScore = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The offset into the document text where the mention begins.
targetedSentimentMention_beginOffset :: Lens.Lens' TargetedSentimentMention (Prelude.Maybe Prelude.Int)
targetedSentimentMention_beginOffset = Lens.lens (\TargetedSentimentMention' {beginOffset} -> beginOffset) (\s@TargetedSentimentMention' {} a -> s {beginOffset = a} :: TargetedSentimentMention)

-- | The type of the entity. Amazon Comprehend supports a variety of
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html#how-targeted-sentiment-entities entity types>.
targetedSentimentMention_type :: Lens.Lens' TargetedSentimentMention (Prelude.Maybe TargetedSentimentEntityType)
targetedSentimentMention_type = Lens.lens (\TargetedSentimentMention' {type'} -> type') (\s@TargetedSentimentMention' {} a -> s {type' = a} :: TargetedSentimentMention)

-- | Model confidence that the entity is relevant. Value range is zero to
-- one, where one is highest confidence.
targetedSentimentMention_score :: Lens.Lens' TargetedSentimentMention (Prelude.Maybe Prelude.Double)
targetedSentimentMention_score = Lens.lens (\TargetedSentimentMention' {score} -> score) (\s@TargetedSentimentMention' {} a -> s {score = a} :: TargetedSentimentMention)

-- | The offset into the document text where the mention ends.
targetedSentimentMention_endOffset :: Lens.Lens' TargetedSentimentMention (Prelude.Maybe Prelude.Int)
targetedSentimentMention_endOffset = Lens.lens (\TargetedSentimentMention' {endOffset} -> endOffset) (\s@TargetedSentimentMention' {} a -> s {endOffset = a} :: TargetedSentimentMention)

-- | Contains the sentiment and sentiment score for the mention.
targetedSentimentMention_mentionSentiment :: Lens.Lens' TargetedSentimentMention (Prelude.Maybe MentionSentiment)
targetedSentimentMention_mentionSentiment = Lens.lens (\TargetedSentimentMention' {mentionSentiment} -> mentionSentiment) (\s@TargetedSentimentMention' {} a -> s {mentionSentiment = a} :: TargetedSentimentMention)

-- | The confidence that all the entities mentioned in the group relate to
-- the same entity.
targetedSentimentMention_groupScore :: Lens.Lens' TargetedSentimentMention (Prelude.Maybe Prelude.Double)
targetedSentimentMention_groupScore = Lens.lens (\TargetedSentimentMention' {groupScore} -> groupScore) (\s@TargetedSentimentMention' {} a -> s {groupScore = a} :: TargetedSentimentMention)

-- | The text in the document that identifies the entity.
targetedSentimentMention_text :: Lens.Lens' TargetedSentimentMention (Prelude.Maybe Prelude.Text)
targetedSentimentMention_text = Lens.lens (\TargetedSentimentMention' {text} -> text) (\s@TargetedSentimentMention' {} a -> s {text = a} :: TargetedSentimentMention)

instance Core.FromJSON TargetedSentimentMention where
  parseJSON =
    Core.withObject
      "TargetedSentimentMention"
      ( \x ->
          TargetedSentimentMention'
            Prelude.<$> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Score")
            Prelude.<*> (x Core..:? "EndOffset")
            Prelude.<*> (x Core..:? "MentionSentiment")
            Prelude.<*> (x Core..:? "GroupScore")
            Prelude.<*> (x Core..:? "Text")
      )

instance Prelude.Hashable TargetedSentimentMention where
  hashWithSalt _salt TargetedSentimentMention' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` mentionSentiment
      `Prelude.hashWithSalt` groupScore
      `Prelude.hashWithSalt` text

instance Prelude.NFData TargetedSentimentMention where
  rnf TargetedSentimentMention' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf mentionSentiment
      `Prelude.seq` Prelude.rnf groupScore
      `Prelude.seq` Prelude.rnf text
