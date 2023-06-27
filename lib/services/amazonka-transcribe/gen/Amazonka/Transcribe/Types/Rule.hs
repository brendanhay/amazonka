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
-- Module      : Amazonka.Transcribe.Types.Rule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.Rule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.InterruptionFilter
import Amazonka.Transcribe.Types.NonTalkTimeFilter
import Amazonka.Transcribe.Types.SentimentFilter
import Amazonka.Transcribe.Types.TranscriptFilter

-- | A rule is a set of criteria that you can specify to flag an attribute in
-- your Call Analytics output. Rules define a Call Analytics category.
--
-- Rules can include these parameters: , , , and .
--
-- To learn more about Call Analytics rules and categories, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tca-categories-batch.html Creating categories for post-call transcriptions>
-- and
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tca-categories-stream.html Creating categories for real-time transcriptions>.
--
-- To learn more about Call Analytics, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/call-analytics.html Analyzing call center audio with Call Analytics>.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | Flag the presence or absence of interruptions in your Call Analytics
    -- transcription output. Refer to for more detail.
    interruptionFilter :: Prelude.Maybe InterruptionFilter,
    -- | Flag the presence or absence of periods of silence in your Call
    -- Analytics transcription output. Refer to for more detail.
    nonTalkTimeFilter :: Prelude.Maybe NonTalkTimeFilter,
    -- | Flag the presence or absence of specific sentiments in your Call
    -- Analytics transcription output. Refer to for more detail.
    sentimentFilter :: Prelude.Maybe SentimentFilter,
    -- | Flag the presence or absence of specific words or phrases in your Call
    -- Analytics transcription output. Refer to for more detail.
    transcriptFilter :: Prelude.Maybe TranscriptFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interruptionFilter', 'rule_interruptionFilter' - Flag the presence or absence of interruptions in your Call Analytics
-- transcription output. Refer to for more detail.
--
-- 'nonTalkTimeFilter', 'rule_nonTalkTimeFilter' - Flag the presence or absence of periods of silence in your Call
-- Analytics transcription output. Refer to for more detail.
--
-- 'sentimentFilter', 'rule_sentimentFilter' - Flag the presence or absence of specific sentiments in your Call
-- Analytics transcription output. Refer to for more detail.
--
-- 'transcriptFilter', 'rule_transcriptFilter' - Flag the presence or absence of specific words or phrases in your Call
-- Analytics transcription output. Refer to for more detail.
newRule ::
  Rule
newRule =
  Rule'
    { interruptionFilter = Prelude.Nothing,
      nonTalkTimeFilter = Prelude.Nothing,
      sentimentFilter = Prelude.Nothing,
      transcriptFilter = Prelude.Nothing
    }

-- | Flag the presence or absence of interruptions in your Call Analytics
-- transcription output. Refer to for more detail.
rule_interruptionFilter :: Lens.Lens' Rule (Prelude.Maybe InterruptionFilter)
rule_interruptionFilter = Lens.lens (\Rule' {interruptionFilter} -> interruptionFilter) (\s@Rule' {} a -> s {interruptionFilter = a} :: Rule)

-- | Flag the presence or absence of periods of silence in your Call
-- Analytics transcription output. Refer to for more detail.
rule_nonTalkTimeFilter :: Lens.Lens' Rule (Prelude.Maybe NonTalkTimeFilter)
rule_nonTalkTimeFilter = Lens.lens (\Rule' {nonTalkTimeFilter} -> nonTalkTimeFilter) (\s@Rule' {} a -> s {nonTalkTimeFilter = a} :: Rule)

-- | Flag the presence or absence of specific sentiments in your Call
-- Analytics transcription output. Refer to for more detail.
rule_sentimentFilter :: Lens.Lens' Rule (Prelude.Maybe SentimentFilter)
rule_sentimentFilter = Lens.lens (\Rule' {sentimentFilter} -> sentimentFilter) (\s@Rule' {} a -> s {sentimentFilter = a} :: Rule)

-- | Flag the presence or absence of specific words or phrases in your Call
-- Analytics transcription output. Refer to for more detail.
rule_transcriptFilter :: Lens.Lens' Rule (Prelude.Maybe TranscriptFilter)
rule_transcriptFilter = Lens.lens (\Rule' {transcriptFilter} -> transcriptFilter) (\s@Rule' {} a -> s {transcriptFilter = a} :: Rule)

instance Data.FromJSON Rule where
  parseJSON =
    Data.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Data..:? "InterruptionFilter")
            Prelude.<*> (x Data..:? "NonTalkTimeFilter")
            Prelude.<*> (x Data..:? "SentimentFilter")
            Prelude.<*> (x Data..:? "TranscriptFilter")
      )

instance Prelude.Hashable Rule where
  hashWithSalt _salt Rule' {..} =
    _salt
      `Prelude.hashWithSalt` interruptionFilter
      `Prelude.hashWithSalt` nonTalkTimeFilter
      `Prelude.hashWithSalt` sentimentFilter
      `Prelude.hashWithSalt` transcriptFilter

instance Prelude.NFData Rule where
  rnf Rule' {..} =
    Prelude.rnf interruptionFilter
      `Prelude.seq` Prelude.rnf nonTalkTimeFilter
      `Prelude.seq` Prelude.rnf sentimentFilter
      `Prelude.seq` Prelude.rnf transcriptFilter

instance Data.ToJSON Rule where
  toJSON Rule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InterruptionFilter" Data..=)
              Prelude.<$> interruptionFilter,
            ("NonTalkTimeFilter" Data..=)
              Prelude.<$> nonTalkTimeFilter,
            ("SentimentFilter" Data..=)
              Prelude.<$> sentimentFilter,
            ("TranscriptFilter" Data..=)
              Prelude.<$> transcriptFilter
          ]
      )
