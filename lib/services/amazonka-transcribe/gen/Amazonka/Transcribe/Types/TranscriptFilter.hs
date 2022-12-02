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
-- Module      : Amazonka.Transcribe.Types.TranscriptFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.TranscriptFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.AbsoluteTimeRange
import Amazonka.Transcribe.Types.ParticipantRole
import Amazonka.Transcribe.Types.RelativeTimeRange
import Amazonka.Transcribe.Types.TranscriptFilterType

-- | Flag the presence or absence of specific words or phrases detected in
-- your Call Analytics transcription output.
--
-- Rules using @TranscriptFilter@ are designed to match:
--
-- -   Custom words or phrases spoken by the agent, the customer, or both
--
-- -   Custom words or phrases __not__ spoken by the agent, the customer,
--     or either
--
-- -   Custom words or phrases that occur at a specific time frame
--
-- See
-- <https://docs.aws.amazon.com/transcribe/latest/dg/call-analytics-create-categories.html#call-analytics-create-categories-rules Rule criteria>
-- for examples.
--
-- /See:/ 'newTranscriptFilter' smart constructor.
data TranscriptFilter = TranscriptFilter'
  { -- | Set to @TRUE@ to flag the absence of the phrase you specified in your
    -- request. Set to @FALSE@ to flag the presence of the phrase you specified
    -- in your request.
    negate :: Prelude.Maybe Prelude.Bool,
    -- | Allows you to specify a time range (in milliseconds) in your audio,
    -- during which you want to search for the specified key words or phrases.
    -- See for more detail.
    absoluteTimeRange :: Prelude.Maybe AbsoluteTimeRange,
    -- | Specify the participant you want to flag. Omitting this parameter is
    -- equivalent to specifying both participants.
    participantRole :: Prelude.Maybe ParticipantRole,
    -- | Allows you to specify a time range (in percentage) in your media file,
    -- during which you want to search for the specified key words or phrases.
    -- See for more detail.
    relativeTimeRange :: Prelude.Maybe RelativeTimeRange,
    -- | Flag the presence or absence of an exact match to the phrases you
    -- specify. For example, if you specify the phrase \"speak to a manager\"
    -- as your @Targets@ value, only that exact phrase is flagged.
    --
    -- Note that semantic matching is not supported. For example, if your
    -- customer says \"speak to /the/ manager\", instead of \"speak to /a/
    -- manager\", your content is not flagged.
    transcriptFilterType :: TranscriptFilterType,
    -- | Specify the phrases you want to flag.
    targets :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranscriptFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'negate', 'transcriptFilter_negate' - Set to @TRUE@ to flag the absence of the phrase you specified in your
-- request. Set to @FALSE@ to flag the presence of the phrase you specified
-- in your request.
--
-- 'absoluteTimeRange', 'transcriptFilter_absoluteTimeRange' - Allows you to specify a time range (in milliseconds) in your audio,
-- during which you want to search for the specified key words or phrases.
-- See for more detail.
--
-- 'participantRole', 'transcriptFilter_participantRole' - Specify the participant you want to flag. Omitting this parameter is
-- equivalent to specifying both participants.
--
-- 'relativeTimeRange', 'transcriptFilter_relativeTimeRange' - Allows you to specify a time range (in percentage) in your media file,
-- during which you want to search for the specified key words or phrases.
-- See for more detail.
--
-- 'transcriptFilterType', 'transcriptFilter_transcriptFilterType' - Flag the presence or absence of an exact match to the phrases you
-- specify. For example, if you specify the phrase \"speak to a manager\"
-- as your @Targets@ value, only that exact phrase is flagged.
--
-- Note that semantic matching is not supported. For example, if your
-- customer says \"speak to /the/ manager\", instead of \"speak to /a/
-- manager\", your content is not flagged.
--
-- 'targets', 'transcriptFilter_targets' - Specify the phrases you want to flag.
newTranscriptFilter ::
  -- | 'transcriptFilterType'
  TranscriptFilterType ->
  -- | 'targets'
  Prelude.NonEmpty Prelude.Text ->
  TranscriptFilter
newTranscriptFilter pTranscriptFilterType_ pTargets_ =
  TranscriptFilter'
    { negate = Prelude.Nothing,
      absoluteTimeRange = Prelude.Nothing,
      participantRole = Prelude.Nothing,
      relativeTimeRange = Prelude.Nothing,
      transcriptFilterType = pTranscriptFilterType_,
      targets = Lens.coerced Lens.# pTargets_
    }

-- | Set to @TRUE@ to flag the absence of the phrase you specified in your
-- request. Set to @FALSE@ to flag the presence of the phrase you specified
-- in your request.
transcriptFilter_negate :: Lens.Lens' TranscriptFilter (Prelude.Maybe Prelude.Bool)
transcriptFilter_negate = Lens.lens (\TranscriptFilter' {negate} -> negate) (\s@TranscriptFilter' {} a -> s {negate = a} :: TranscriptFilter)

-- | Allows you to specify a time range (in milliseconds) in your audio,
-- during which you want to search for the specified key words or phrases.
-- See for more detail.
transcriptFilter_absoluteTimeRange :: Lens.Lens' TranscriptFilter (Prelude.Maybe AbsoluteTimeRange)
transcriptFilter_absoluteTimeRange = Lens.lens (\TranscriptFilter' {absoluteTimeRange} -> absoluteTimeRange) (\s@TranscriptFilter' {} a -> s {absoluteTimeRange = a} :: TranscriptFilter)

-- | Specify the participant you want to flag. Omitting this parameter is
-- equivalent to specifying both participants.
transcriptFilter_participantRole :: Lens.Lens' TranscriptFilter (Prelude.Maybe ParticipantRole)
transcriptFilter_participantRole = Lens.lens (\TranscriptFilter' {participantRole} -> participantRole) (\s@TranscriptFilter' {} a -> s {participantRole = a} :: TranscriptFilter)

-- | Allows you to specify a time range (in percentage) in your media file,
-- during which you want to search for the specified key words or phrases.
-- See for more detail.
transcriptFilter_relativeTimeRange :: Lens.Lens' TranscriptFilter (Prelude.Maybe RelativeTimeRange)
transcriptFilter_relativeTimeRange = Lens.lens (\TranscriptFilter' {relativeTimeRange} -> relativeTimeRange) (\s@TranscriptFilter' {} a -> s {relativeTimeRange = a} :: TranscriptFilter)

-- | Flag the presence or absence of an exact match to the phrases you
-- specify. For example, if you specify the phrase \"speak to a manager\"
-- as your @Targets@ value, only that exact phrase is flagged.
--
-- Note that semantic matching is not supported. For example, if your
-- customer says \"speak to /the/ manager\", instead of \"speak to /a/
-- manager\", your content is not flagged.
transcriptFilter_transcriptFilterType :: Lens.Lens' TranscriptFilter TranscriptFilterType
transcriptFilter_transcriptFilterType = Lens.lens (\TranscriptFilter' {transcriptFilterType} -> transcriptFilterType) (\s@TranscriptFilter' {} a -> s {transcriptFilterType = a} :: TranscriptFilter)

-- | Specify the phrases you want to flag.
transcriptFilter_targets :: Lens.Lens' TranscriptFilter (Prelude.NonEmpty Prelude.Text)
transcriptFilter_targets = Lens.lens (\TranscriptFilter' {targets} -> targets) (\s@TranscriptFilter' {} a -> s {targets = a} :: TranscriptFilter) Prelude.. Lens.coerced

instance Data.FromJSON TranscriptFilter where
  parseJSON =
    Data.withObject
      "TranscriptFilter"
      ( \x ->
          TranscriptFilter'
            Prelude.<$> (x Data..:? "Negate")
            Prelude.<*> (x Data..:? "AbsoluteTimeRange")
            Prelude.<*> (x Data..:? "ParticipantRole")
            Prelude.<*> (x Data..:? "RelativeTimeRange")
            Prelude.<*> (x Data..: "TranscriptFilterType")
            Prelude.<*> (x Data..: "Targets")
      )

instance Prelude.Hashable TranscriptFilter where
  hashWithSalt _salt TranscriptFilter' {..} =
    _salt `Prelude.hashWithSalt` negate
      `Prelude.hashWithSalt` absoluteTimeRange
      `Prelude.hashWithSalt` participantRole
      `Prelude.hashWithSalt` relativeTimeRange
      `Prelude.hashWithSalt` transcriptFilterType
      `Prelude.hashWithSalt` targets

instance Prelude.NFData TranscriptFilter where
  rnf TranscriptFilter' {..} =
    Prelude.rnf negate
      `Prelude.seq` Prelude.rnf absoluteTimeRange
      `Prelude.seq` Prelude.rnf participantRole
      `Prelude.seq` Prelude.rnf relativeTimeRange
      `Prelude.seq` Prelude.rnf transcriptFilterType
      `Prelude.seq` Prelude.rnf targets

instance Data.ToJSON TranscriptFilter where
  toJSON TranscriptFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Negate" Data..=) Prelude.<$> negate,
            ("AbsoluteTimeRange" Data..=)
              Prelude.<$> absoluteTimeRange,
            ("ParticipantRole" Data..=)
              Prelude.<$> participantRole,
            ("RelativeTimeRange" Data..=)
              Prelude.<$> relativeTimeRange,
            Prelude.Just
              ( "TranscriptFilterType"
                  Data..= transcriptFilterType
              ),
            Prelude.Just ("Targets" Data..= targets)
          ]
      )
