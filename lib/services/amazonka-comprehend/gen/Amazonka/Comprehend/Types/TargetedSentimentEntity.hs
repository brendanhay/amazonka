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
-- Module      : Amazonka.Comprehend.Types.TargetedSentimentEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.TargetedSentimentEntity where

import Amazonka.Comprehend.Types.TargetedSentimentMention
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about one of the entities found by targeted sentiment
-- analysis.
--
-- For more information about targeted sentiment, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html Targeted sentiment>.
--
-- /See:/ 'newTargetedSentimentEntity' smart constructor.
data TargetedSentimentEntity = TargetedSentimentEntity'
  { -- | One or more index into the Mentions array that provides the best name
    -- for the entity group.
    descriptiveMentionIndex :: Prelude.Maybe [Prelude.Int],
    -- | An array of mentions of the entity in the document. The array represents
    -- a co-reference group. See
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html#how-targeted-sentiment-values Co-reference group>
    -- for an example.
    mentions :: Prelude.Maybe [TargetedSentimentMention]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetedSentimentEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'descriptiveMentionIndex', 'targetedSentimentEntity_descriptiveMentionIndex' - One or more index into the Mentions array that provides the best name
-- for the entity group.
--
-- 'mentions', 'targetedSentimentEntity_mentions' - An array of mentions of the entity in the document. The array represents
-- a co-reference group. See
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html#how-targeted-sentiment-values Co-reference group>
-- for an example.
newTargetedSentimentEntity ::
  TargetedSentimentEntity
newTargetedSentimentEntity =
  TargetedSentimentEntity'
    { descriptiveMentionIndex =
        Prelude.Nothing,
      mentions = Prelude.Nothing
    }

-- | One or more index into the Mentions array that provides the best name
-- for the entity group.
targetedSentimentEntity_descriptiveMentionIndex :: Lens.Lens' TargetedSentimentEntity (Prelude.Maybe [Prelude.Int])
targetedSentimentEntity_descriptiveMentionIndex = Lens.lens (\TargetedSentimentEntity' {descriptiveMentionIndex} -> descriptiveMentionIndex) (\s@TargetedSentimentEntity' {} a -> s {descriptiveMentionIndex = a} :: TargetedSentimentEntity) Prelude.. Lens.mapping Lens.coerced

-- | An array of mentions of the entity in the document. The array represents
-- a co-reference group. See
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html#how-targeted-sentiment-values Co-reference group>
-- for an example.
targetedSentimentEntity_mentions :: Lens.Lens' TargetedSentimentEntity (Prelude.Maybe [TargetedSentimentMention])
targetedSentimentEntity_mentions = Lens.lens (\TargetedSentimentEntity' {mentions} -> mentions) (\s@TargetedSentimentEntity' {} a -> s {mentions = a} :: TargetedSentimentEntity) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TargetedSentimentEntity where
  parseJSON =
    Data.withObject
      "TargetedSentimentEntity"
      ( \x ->
          TargetedSentimentEntity'
            Prelude.<$> ( x
                            Data..:? "DescriptiveMentionIndex"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Mentions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TargetedSentimentEntity where
  hashWithSalt _salt TargetedSentimentEntity' {..} =
    _salt
      `Prelude.hashWithSalt` descriptiveMentionIndex
      `Prelude.hashWithSalt` mentions

instance Prelude.NFData TargetedSentimentEntity where
  rnf TargetedSentimentEntity' {..} =
    Prelude.rnf descriptiveMentionIndex
      `Prelude.seq` Prelude.rnf mentions
