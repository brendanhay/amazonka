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
-- Module      : Amazonka.ConnectContactLens.Types.Transcript
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectContactLens.Types.Transcript where

import Amazonka.ConnectContactLens.Types.IssueDetected
import Amazonka.ConnectContactLens.Types.SentimentValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of messages in the session.
--
-- /See:/ 'newTranscript' smart constructor.
data Transcript = Transcript'
  { -- | List of positions where issues were detected on the transcript.
    issuesDetected :: Prelude.Maybe [IssueDetected],
    -- | The identifier of the transcript.
    id :: Prelude.Text,
    -- | The identifier of the participant.
    participantId :: Prelude.Text,
    -- | The role of participant. For example, is it a customer, agent, or
    -- system.
    participantRole :: Prelude.Text,
    -- | The content of the transcript.
    content :: Prelude.Text,
    -- | The beginning offset in the contact for this transcript.
    beginOffsetMillis :: Prelude.Natural,
    -- | The end offset in the contact for this transcript.
    endOffsetMillis :: Prelude.Natural,
    -- | The sentiment of the detected for this piece of transcript.
    sentiment :: SentimentValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Transcript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issuesDetected', 'transcript_issuesDetected' - List of positions where issues were detected on the transcript.
--
-- 'id', 'transcript_id' - The identifier of the transcript.
--
-- 'participantId', 'transcript_participantId' - The identifier of the participant.
--
-- 'participantRole', 'transcript_participantRole' - The role of participant. For example, is it a customer, agent, or
-- system.
--
-- 'content', 'transcript_content' - The content of the transcript.
--
-- 'beginOffsetMillis', 'transcript_beginOffsetMillis' - The beginning offset in the contact for this transcript.
--
-- 'endOffsetMillis', 'transcript_endOffsetMillis' - The end offset in the contact for this transcript.
--
-- 'sentiment', 'transcript_sentiment' - The sentiment of the detected for this piece of transcript.
newTranscript ::
  -- | 'id'
  Prelude.Text ->
  -- | 'participantId'
  Prelude.Text ->
  -- | 'participantRole'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  -- | 'beginOffsetMillis'
  Prelude.Natural ->
  -- | 'endOffsetMillis'
  Prelude.Natural ->
  -- | 'sentiment'
  SentimentValue ->
  Transcript
newTranscript
  pId_
  pParticipantId_
  pParticipantRole_
  pContent_
  pBeginOffsetMillis_
  pEndOffsetMillis_
  pSentiment_ =
    Transcript'
      { issuesDetected = Prelude.Nothing,
        id = pId_,
        participantId = pParticipantId_,
        participantRole = pParticipantRole_,
        content = pContent_,
        beginOffsetMillis = pBeginOffsetMillis_,
        endOffsetMillis = pEndOffsetMillis_,
        sentiment = pSentiment_
      }

-- | List of positions where issues were detected on the transcript.
transcript_issuesDetected :: Lens.Lens' Transcript (Prelude.Maybe [IssueDetected])
transcript_issuesDetected = Lens.lens (\Transcript' {issuesDetected} -> issuesDetected) (\s@Transcript' {} a -> s {issuesDetected = a} :: Transcript) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the transcript.
transcript_id :: Lens.Lens' Transcript Prelude.Text
transcript_id = Lens.lens (\Transcript' {id} -> id) (\s@Transcript' {} a -> s {id = a} :: Transcript)

-- | The identifier of the participant.
transcript_participantId :: Lens.Lens' Transcript Prelude.Text
transcript_participantId = Lens.lens (\Transcript' {participantId} -> participantId) (\s@Transcript' {} a -> s {participantId = a} :: Transcript)

-- | The role of participant. For example, is it a customer, agent, or
-- system.
transcript_participantRole :: Lens.Lens' Transcript Prelude.Text
transcript_participantRole = Lens.lens (\Transcript' {participantRole} -> participantRole) (\s@Transcript' {} a -> s {participantRole = a} :: Transcript)

-- | The content of the transcript.
transcript_content :: Lens.Lens' Transcript Prelude.Text
transcript_content = Lens.lens (\Transcript' {content} -> content) (\s@Transcript' {} a -> s {content = a} :: Transcript)

-- | The beginning offset in the contact for this transcript.
transcript_beginOffsetMillis :: Lens.Lens' Transcript Prelude.Natural
transcript_beginOffsetMillis = Lens.lens (\Transcript' {beginOffsetMillis} -> beginOffsetMillis) (\s@Transcript' {} a -> s {beginOffsetMillis = a} :: Transcript)

-- | The end offset in the contact for this transcript.
transcript_endOffsetMillis :: Lens.Lens' Transcript Prelude.Natural
transcript_endOffsetMillis = Lens.lens (\Transcript' {endOffsetMillis} -> endOffsetMillis) (\s@Transcript' {} a -> s {endOffsetMillis = a} :: Transcript)

-- | The sentiment of the detected for this piece of transcript.
transcript_sentiment :: Lens.Lens' Transcript SentimentValue
transcript_sentiment = Lens.lens (\Transcript' {sentiment} -> sentiment) (\s@Transcript' {} a -> s {sentiment = a} :: Transcript)

instance Data.FromJSON Transcript where
  parseJSON =
    Data.withObject
      "Transcript"
      ( \x ->
          Transcript'
            Prelude.<$> (x Data..:? "IssuesDetected" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "ParticipantId")
            Prelude.<*> (x Data..: "ParticipantRole")
            Prelude.<*> (x Data..: "Content")
            Prelude.<*> (x Data..: "BeginOffsetMillis")
            Prelude.<*> (x Data..: "EndOffsetMillis")
            Prelude.<*> (x Data..: "Sentiment")
      )

instance Prelude.Hashable Transcript where
  hashWithSalt _salt Transcript' {..} =
    _salt `Prelude.hashWithSalt` issuesDetected
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` participantId
      `Prelude.hashWithSalt` participantRole
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` beginOffsetMillis
      `Prelude.hashWithSalt` endOffsetMillis
      `Prelude.hashWithSalt` sentiment

instance Prelude.NFData Transcript where
  rnf Transcript' {..} =
    Prelude.rnf issuesDetected
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf participantRole
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf beginOffsetMillis
      `Prelude.seq` Prelude.rnf endOffsetMillis
      `Prelude.seq` Prelude.rnf sentiment
