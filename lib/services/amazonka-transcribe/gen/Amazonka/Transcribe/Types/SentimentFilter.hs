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
-- Module      : Amazonka.Transcribe.Types.SentimentFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.SentimentFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.AbsoluteTimeRange
import Amazonka.Transcribe.Types.ParticipantRole
import Amazonka.Transcribe.Types.RelativeTimeRange
import Amazonka.Transcribe.Types.SentimentValue

-- | An object that enables you to specify a particular customer or agent
-- sentiment. If at least 50 percent of the conversation turns (the
-- back-and-forth between two speakers) in a specified time period match
-- the specified sentiment, Amazon Transcribe will consider the sentiment a
-- match.
--
-- /See:/ 'newSentimentFilter' smart constructor.
data SentimentFilter = SentimentFilter'
  { -- | A value that determines whether the sentiment belongs to the customer or
    -- the agent.
    participantRole :: Prelude.Maybe ParticipantRole,
    -- | The time range, set in percentages, that correspond to proportion of the
    -- call.
    relativeTimeRange :: Prelude.Maybe RelativeTimeRange,
    -- | Set to @TRUE@ to look for sentiments that weren\'t specified in the
    -- request.
    negate :: Prelude.Maybe Prelude.Bool,
    -- | The time range, measured in seconds, of the sentiment.
    absoluteTimeRange :: Prelude.Maybe AbsoluteTimeRange,
    -- | An array that enables you to specify sentiments for the customer or
    -- agent. You can specify one or more values.
    sentiments :: Prelude.NonEmpty SentimentValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SentimentFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participantRole', 'sentimentFilter_participantRole' - A value that determines whether the sentiment belongs to the customer or
-- the agent.
--
-- 'relativeTimeRange', 'sentimentFilter_relativeTimeRange' - The time range, set in percentages, that correspond to proportion of the
-- call.
--
-- 'negate', 'sentimentFilter_negate' - Set to @TRUE@ to look for sentiments that weren\'t specified in the
-- request.
--
-- 'absoluteTimeRange', 'sentimentFilter_absoluteTimeRange' - The time range, measured in seconds, of the sentiment.
--
-- 'sentiments', 'sentimentFilter_sentiments' - An array that enables you to specify sentiments for the customer or
-- agent. You can specify one or more values.
newSentimentFilter ::
  -- | 'sentiments'
  Prelude.NonEmpty SentimentValue ->
  SentimentFilter
newSentimentFilter pSentiments_ =
  SentimentFilter'
    { participantRole = Prelude.Nothing,
      relativeTimeRange = Prelude.Nothing,
      negate = Prelude.Nothing,
      absoluteTimeRange = Prelude.Nothing,
      sentiments = Lens.coerced Lens.# pSentiments_
    }

-- | A value that determines whether the sentiment belongs to the customer or
-- the agent.
sentimentFilter_participantRole :: Lens.Lens' SentimentFilter (Prelude.Maybe ParticipantRole)
sentimentFilter_participantRole = Lens.lens (\SentimentFilter' {participantRole} -> participantRole) (\s@SentimentFilter' {} a -> s {participantRole = a} :: SentimentFilter)

-- | The time range, set in percentages, that correspond to proportion of the
-- call.
sentimentFilter_relativeTimeRange :: Lens.Lens' SentimentFilter (Prelude.Maybe RelativeTimeRange)
sentimentFilter_relativeTimeRange = Lens.lens (\SentimentFilter' {relativeTimeRange} -> relativeTimeRange) (\s@SentimentFilter' {} a -> s {relativeTimeRange = a} :: SentimentFilter)

-- | Set to @TRUE@ to look for sentiments that weren\'t specified in the
-- request.
sentimentFilter_negate :: Lens.Lens' SentimentFilter (Prelude.Maybe Prelude.Bool)
sentimentFilter_negate = Lens.lens (\SentimentFilter' {negate} -> negate) (\s@SentimentFilter' {} a -> s {negate = a} :: SentimentFilter)

-- | The time range, measured in seconds, of the sentiment.
sentimentFilter_absoluteTimeRange :: Lens.Lens' SentimentFilter (Prelude.Maybe AbsoluteTimeRange)
sentimentFilter_absoluteTimeRange = Lens.lens (\SentimentFilter' {absoluteTimeRange} -> absoluteTimeRange) (\s@SentimentFilter' {} a -> s {absoluteTimeRange = a} :: SentimentFilter)

-- | An array that enables you to specify sentiments for the customer or
-- agent. You can specify one or more values.
sentimentFilter_sentiments :: Lens.Lens' SentimentFilter (Prelude.NonEmpty SentimentValue)
sentimentFilter_sentiments = Lens.lens (\SentimentFilter' {sentiments} -> sentiments) (\s@SentimentFilter' {} a -> s {sentiments = a} :: SentimentFilter) Prelude.. Lens.coerced

instance Core.FromJSON SentimentFilter where
  parseJSON =
    Core.withObject
      "SentimentFilter"
      ( \x ->
          SentimentFilter'
            Prelude.<$> (x Core..:? "ParticipantRole")
            Prelude.<*> (x Core..:? "RelativeTimeRange")
            Prelude.<*> (x Core..:? "Negate")
            Prelude.<*> (x Core..:? "AbsoluteTimeRange")
            Prelude.<*> (x Core..: "Sentiments")
      )

instance Prelude.Hashable SentimentFilter where
  hashWithSalt salt' SentimentFilter' {..} =
    salt' `Prelude.hashWithSalt` sentiments
      `Prelude.hashWithSalt` absoluteTimeRange
      `Prelude.hashWithSalt` negate
      `Prelude.hashWithSalt` relativeTimeRange
      `Prelude.hashWithSalt` participantRole

instance Prelude.NFData SentimentFilter where
  rnf SentimentFilter' {..} =
    Prelude.rnf participantRole
      `Prelude.seq` Prelude.rnf sentiments
      `Prelude.seq` Prelude.rnf absoluteTimeRange
      `Prelude.seq` Prelude.rnf negate
      `Prelude.seq` Prelude.rnf relativeTimeRange

instance Core.ToJSON SentimentFilter where
  toJSON SentimentFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ParticipantRole" Core..=)
              Prelude.<$> participantRole,
            ("RelativeTimeRange" Core..=)
              Prelude.<$> relativeTimeRange,
            ("Negate" Core..=) Prelude.<$> negate,
            ("AbsoluteTimeRange" Core..=)
              Prelude.<$> absoluteTimeRange,
            Prelude.Just ("Sentiments" Core..= sentiments)
          ]
      )
