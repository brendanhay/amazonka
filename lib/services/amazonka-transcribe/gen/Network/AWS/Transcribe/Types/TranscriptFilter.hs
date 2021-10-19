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
-- Module      : Network.AWS.Transcribe.Types.TranscriptFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transcribe.Types.AbsoluteTimeRange
import Network.AWS.Transcribe.Types.ParticipantRole
import Network.AWS.Transcribe.Types.RelativeTimeRange
import Network.AWS.Transcribe.Types.TranscriptFilterType

-- | Matches the output of the transcription to either the specific phrases
-- that you specify, or the intent of the phrases that you specify.
--
-- /See:/ 'newTranscriptFilter' smart constructor.
data TranscriptFilter = TranscriptFilter'
  { -- | Determines whether the customer or the agent is speaking the phrases
    -- that you\'ve specified.
    participantRole :: Prelude.Maybe ParticipantRole,
    -- | An object that allows percentages to specify the proportion of the call
    -- where you would like to apply a filter. For example, you can specify the
    -- first half of the call. You can also specify the period of time between
    -- halfway through to three-quarters of the way through the call. Because
    -- the length of conversation can vary between calls, you can apply
    -- relative time ranges across all calls.
    relativeTimeRange :: Prelude.Maybe RelativeTimeRange,
    -- | If @TRUE@, the rule that you specify is applied to everything except for
    -- the phrases that you specify.
    negate :: Prelude.Maybe Prelude.Bool,
    -- | A time range, set in seconds, between two points in the call.
    absoluteTimeRange :: Prelude.Maybe AbsoluteTimeRange,
    -- | Matches the phrase to the transcription output in a word for word
    -- fashion. For example, if you specify the phrase \"I want to speak to the
    -- manager.\" Amazon Transcribe attempts to match that specific phrase to
    -- the transcription.
    transcriptFilterType :: TranscriptFilterType,
    -- | The phrases that you\'re specifying for the transcript filter to match.
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
-- 'participantRole', 'transcriptFilter_participantRole' - Determines whether the customer or the agent is speaking the phrases
-- that you\'ve specified.
--
-- 'relativeTimeRange', 'transcriptFilter_relativeTimeRange' - An object that allows percentages to specify the proportion of the call
-- where you would like to apply a filter. For example, you can specify the
-- first half of the call. You can also specify the period of time between
-- halfway through to three-quarters of the way through the call. Because
-- the length of conversation can vary between calls, you can apply
-- relative time ranges across all calls.
--
-- 'negate', 'transcriptFilter_negate' - If @TRUE@, the rule that you specify is applied to everything except for
-- the phrases that you specify.
--
-- 'absoluteTimeRange', 'transcriptFilter_absoluteTimeRange' - A time range, set in seconds, between two points in the call.
--
-- 'transcriptFilterType', 'transcriptFilter_transcriptFilterType' - Matches the phrase to the transcription output in a word for word
-- fashion. For example, if you specify the phrase \"I want to speak to the
-- manager.\" Amazon Transcribe attempts to match that specific phrase to
-- the transcription.
--
-- 'targets', 'transcriptFilter_targets' - The phrases that you\'re specifying for the transcript filter to match.
newTranscriptFilter ::
  -- | 'transcriptFilterType'
  TranscriptFilterType ->
  -- | 'targets'
  Prelude.NonEmpty Prelude.Text ->
  TranscriptFilter
newTranscriptFilter pTranscriptFilterType_ pTargets_ =
  TranscriptFilter'
    { participantRole =
        Prelude.Nothing,
      relativeTimeRange = Prelude.Nothing,
      negate = Prelude.Nothing,
      absoluteTimeRange = Prelude.Nothing,
      transcriptFilterType = pTranscriptFilterType_,
      targets = Lens.coerced Lens.# pTargets_
    }

-- | Determines whether the customer or the agent is speaking the phrases
-- that you\'ve specified.
transcriptFilter_participantRole :: Lens.Lens' TranscriptFilter (Prelude.Maybe ParticipantRole)
transcriptFilter_participantRole = Lens.lens (\TranscriptFilter' {participantRole} -> participantRole) (\s@TranscriptFilter' {} a -> s {participantRole = a} :: TranscriptFilter)

-- | An object that allows percentages to specify the proportion of the call
-- where you would like to apply a filter. For example, you can specify the
-- first half of the call. You can also specify the period of time between
-- halfway through to three-quarters of the way through the call. Because
-- the length of conversation can vary between calls, you can apply
-- relative time ranges across all calls.
transcriptFilter_relativeTimeRange :: Lens.Lens' TranscriptFilter (Prelude.Maybe RelativeTimeRange)
transcriptFilter_relativeTimeRange = Lens.lens (\TranscriptFilter' {relativeTimeRange} -> relativeTimeRange) (\s@TranscriptFilter' {} a -> s {relativeTimeRange = a} :: TranscriptFilter)

-- | If @TRUE@, the rule that you specify is applied to everything except for
-- the phrases that you specify.
transcriptFilter_negate :: Lens.Lens' TranscriptFilter (Prelude.Maybe Prelude.Bool)
transcriptFilter_negate = Lens.lens (\TranscriptFilter' {negate} -> negate) (\s@TranscriptFilter' {} a -> s {negate = a} :: TranscriptFilter)

-- | A time range, set in seconds, between two points in the call.
transcriptFilter_absoluteTimeRange :: Lens.Lens' TranscriptFilter (Prelude.Maybe AbsoluteTimeRange)
transcriptFilter_absoluteTimeRange = Lens.lens (\TranscriptFilter' {absoluteTimeRange} -> absoluteTimeRange) (\s@TranscriptFilter' {} a -> s {absoluteTimeRange = a} :: TranscriptFilter)

-- | Matches the phrase to the transcription output in a word for word
-- fashion. For example, if you specify the phrase \"I want to speak to the
-- manager.\" Amazon Transcribe attempts to match that specific phrase to
-- the transcription.
transcriptFilter_transcriptFilterType :: Lens.Lens' TranscriptFilter TranscriptFilterType
transcriptFilter_transcriptFilterType = Lens.lens (\TranscriptFilter' {transcriptFilterType} -> transcriptFilterType) (\s@TranscriptFilter' {} a -> s {transcriptFilterType = a} :: TranscriptFilter)

-- | The phrases that you\'re specifying for the transcript filter to match.
transcriptFilter_targets :: Lens.Lens' TranscriptFilter (Prelude.NonEmpty Prelude.Text)
transcriptFilter_targets = Lens.lens (\TranscriptFilter' {targets} -> targets) (\s@TranscriptFilter' {} a -> s {targets = a} :: TranscriptFilter) Prelude.. Lens.coerced

instance Core.FromJSON TranscriptFilter where
  parseJSON =
    Core.withObject
      "TranscriptFilter"
      ( \x ->
          TranscriptFilter'
            Prelude.<$> (x Core..:? "ParticipantRole")
            Prelude.<*> (x Core..:? "RelativeTimeRange")
            Prelude.<*> (x Core..:? "Negate")
            Prelude.<*> (x Core..:? "AbsoluteTimeRange")
            Prelude.<*> (x Core..: "TranscriptFilterType")
            Prelude.<*> (x Core..: "Targets")
      )

instance Prelude.Hashable TranscriptFilter

instance Prelude.NFData TranscriptFilter

instance Core.ToJSON TranscriptFilter where
  toJSON TranscriptFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ParticipantRole" Core..=)
              Prelude.<$> participantRole,
            ("RelativeTimeRange" Core..=)
              Prelude.<$> relativeTimeRange,
            ("Negate" Core..=) Prelude.<$> negate,
            ("AbsoluteTimeRange" Core..=)
              Prelude.<$> absoluteTimeRange,
            Prelude.Just
              ( "TranscriptFilterType"
                  Core..= transcriptFilterType
              ),
            Prelude.Just ("Targets" Core..= targets)
          ]
      )
