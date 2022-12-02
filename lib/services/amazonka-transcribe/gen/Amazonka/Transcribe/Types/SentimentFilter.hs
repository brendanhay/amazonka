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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.SentimentFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.AbsoluteTimeRange
import Amazonka.Transcribe.Types.ParticipantRole
import Amazonka.Transcribe.Types.RelativeTimeRange
import Amazonka.Transcribe.Types.SentimentValue

-- | Flag the presence or absence of specific sentiments detected in your
-- Call Analytics transcription output.
--
-- Rules using @SentimentFilter@ are designed to match:
--
-- -   The presence or absence of a positive sentiment felt by the
--     customer, agent, or both at specified points in the call
--
-- -   The presence or absence of a negative sentiment felt by the
--     customer, agent, or both at specified points in the call
--
-- -   The presence or absence of a neutral sentiment felt by the customer,
--     agent, or both at specified points in the call
--
-- -   The presence or absence of a mixed sentiment felt by the customer,
--     the agent, or both at specified points in the call
--
-- See
-- <https://docs.aws.amazon.com/transcribe/latest/dg/call-analytics-create-categories.html#call-analytics-create-categories-rules Rule criteria>
-- for examples.
--
-- /See:/ 'newSentimentFilter' smart constructor.
data SentimentFilter = SentimentFilter'
  { -- | Set to @TRUE@ to flag the sentiments you didn\'t include in your
    -- request. Set to @FALSE@ to flag the sentiments you specified in your
    -- request.
    negate :: Prelude.Maybe Prelude.Bool,
    -- | Allows you to specify a time range (in milliseconds) in your audio,
    -- during which you want to search for the specified sentiments. See for
    -- more detail.
    absoluteTimeRange :: Prelude.Maybe AbsoluteTimeRange,
    -- | Specify the participant you want to flag. Omitting this parameter is
    -- equivalent to specifying both participants.
    participantRole :: Prelude.Maybe ParticipantRole,
    -- | Allows you to specify a time range (in percentage) in your media file,
    -- during which you want to search for the specified sentiments. See for
    -- more detail.
    relativeTimeRange :: Prelude.Maybe RelativeTimeRange,
    -- | Specify the sentiments you want to flag.
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
-- 'negate', 'sentimentFilter_negate' - Set to @TRUE@ to flag the sentiments you didn\'t include in your
-- request. Set to @FALSE@ to flag the sentiments you specified in your
-- request.
--
-- 'absoluteTimeRange', 'sentimentFilter_absoluteTimeRange' - Allows you to specify a time range (in milliseconds) in your audio,
-- during which you want to search for the specified sentiments. See for
-- more detail.
--
-- 'participantRole', 'sentimentFilter_participantRole' - Specify the participant you want to flag. Omitting this parameter is
-- equivalent to specifying both participants.
--
-- 'relativeTimeRange', 'sentimentFilter_relativeTimeRange' - Allows you to specify a time range (in percentage) in your media file,
-- during which you want to search for the specified sentiments. See for
-- more detail.
--
-- 'sentiments', 'sentimentFilter_sentiments' - Specify the sentiments you want to flag.
newSentimentFilter ::
  -- | 'sentiments'
  Prelude.NonEmpty SentimentValue ->
  SentimentFilter
newSentimentFilter pSentiments_ =
  SentimentFilter'
    { negate = Prelude.Nothing,
      absoluteTimeRange = Prelude.Nothing,
      participantRole = Prelude.Nothing,
      relativeTimeRange = Prelude.Nothing,
      sentiments = Lens.coerced Lens.# pSentiments_
    }

-- | Set to @TRUE@ to flag the sentiments you didn\'t include in your
-- request. Set to @FALSE@ to flag the sentiments you specified in your
-- request.
sentimentFilter_negate :: Lens.Lens' SentimentFilter (Prelude.Maybe Prelude.Bool)
sentimentFilter_negate = Lens.lens (\SentimentFilter' {negate} -> negate) (\s@SentimentFilter' {} a -> s {negate = a} :: SentimentFilter)

-- | Allows you to specify a time range (in milliseconds) in your audio,
-- during which you want to search for the specified sentiments. See for
-- more detail.
sentimentFilter_absoluteTimeRange :: Lens.Lens' SentimentFilter (Prelude.Maybe AbsoluteTimeRange)
sentimentFilter_absoluteTimeRange = Lens.lens (\SentimentFilter' {absoluteTimeRange} -> absoluteTimeRange) (\s@SentimentFilter' {} a -> s {absoluteTimeRange = a} :: SentimentFilter)

-- | Specify the participant you want to flag. Omitting this parameter is
-- equivalent to specifying both participants.
sentimentFilter_participantRole :: Lens.Lens' SentimentFilter (Prelude.Maybe ParticipantRole)
sentimentFilter_participantRole = Lens.lens (\SentimentFilter' {participantRole} -> participantRole) (\s@SentimentFilter' {} a -> s {participantRole = a} :: SentimentFilter)

-- | Allows you to specify a time range (in percentage) in your media file,
-- during which you want to search for the specified sentiments. See for
-- more detail.
sentimentFilter_relativeTimeRange :: Lens.Lens' SentimentFilter (Prelude.Maybe RelativeTimeRange)
sentimentFilter_relativeTimeRange = Lens.lens (\SentimentFilter' {relativeTimeRange} -> relativeTimeRange) (\s@SentimentFilter' {} a -> s {relativeTimeRange = a} :: SentimentFilter)

-- | Specify the sentiments you want to flag.
sentimentFilter_sentiments :: Lens.Lens' SentimentFilter (Prelude.NonEmpty SentimentValue)
sentimentFilter_sentiments = Lens.lens (\SentimentFilter' {sentiments} -> sentiments) (\s@SentimentFilter' {} a -> s {sentiments = a} :: SentimentFilter) Prelude.. Lens.coerced

instance Data.FromJSON SentimentFilter where
  parseJSON =
    Data.withObject
      "SentimentFilter"
      ( \x ->
          SentimentFilter'
            Prelude.<$> (x Data..:? "Negate")
            Prelude.<*> (x Data..:? "AbsoluteTimeRange")
            Prelude.<*> (x Data..:? "ParticipantRole")
            Prelude.<*> (x Data..:? "RelativeTimeRange")
            Prelude.<*> (x Data..: "Sentiments")
      )

instance Prelude.Hashable SentimentFilter where
  hashWithSalt _salt SentimentFilter' {..} =
    _salt `Prelude.hashWithSalt` negate
      `Prelude.hashWithSalt` absoluteTimeRange
      `Prelude.hashWithSalt` participantRole
      `Prelude.hashWithSalt` relativeTimeRange
      `Prelude.hashWithSalt` sentiments

instance Prelude.NFData SentimentFilter where
  rnf SentimentFilter' {..} =
    Prelude.rnf negate
      `Prelude.seq` Prelude.rnf absoluteTimeRange
      `Prelude.seq` Prelude.rnf participantRole
      `Prelude.seq` Prelude.rnf relativeTimeRange
      `Prelude.seq` Prelude.rnf sentiments

instance Data.ToJSON SentimentFilter where
  toJSON SentimentFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Negate" Data..=) Prelude.<$> negate,
            ("AbsoluteTimeRange" Data..=)
              Prelude.<$> absoluteTimeRange,
            ("ParticipantRole" Data..=)
              Prelude.<$> participantRole,
            ("RelativeTimeRange" Data..=)
              Prelude.<$> relativeTimeRange,
            Prelude.Just ("Sentiments" Data..= sentiments)
          ]
      )
