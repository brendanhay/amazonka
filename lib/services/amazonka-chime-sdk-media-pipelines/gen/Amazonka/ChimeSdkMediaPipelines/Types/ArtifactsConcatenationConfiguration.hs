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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.AudioConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.CompositedVideoConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.ContentConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.DataChannelConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.MeetingEventsConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.TranscriptionMessagesConcatenationConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.VideoConcatenationConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the artifacts concatenation.
--
-- /See:/ 'newArtifactsConcatenationConfiguration' smart constructor.
data ArtifactsConcatenationConfiguration = ArtifactsConcatenationConfiguration'
  { -- | The configuration for the audio artifacts concatenation.
    audio :: AudioConcatenationConfiguration,
    -- | The configuration for the video artifacts concatenation.
    video :: VideoConcatenationConfiguration,
    -- | The configuration for the content artifacts concatenation.
    content :: ContentConcatenationConfiguration,
    -- | The configuration for the data channel artifacts concatenation.
    dataChannel :: DataChannelConcatenationConfiguration,
    -- | The configuration for the transcription messages artifacts
    -- concatenation.
    transcriptionMessages :: TranscriptionMessagesConcatenationConfiguration,
    -- | The configuration for the meeting events artifacts concatenation.
    meetingEvents :: MeetingEventsConcatenationConfiguration,
    -- | The configuration for the composited video artifacts concatenation.
    compositedVideo :: CompositedVideoConcatenationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArtifactsConcatenationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audio', 'artifactsConcatenationConfiguration_audio' - The configuration for the audio artifacts concatenation.
--
-- 'video', 'artifactsConcatenationConfiguration_video' - The configuration for the video artifacts concatenation.
--
-- 'content', 'artifactsConcatenationConfiguration_content' - The configuration for the content artifacts concatenation.
--
-- 'dataChannel', 'artifactsConcatenationConfiguration_dataChannel' - The configuration for the data channel artifacts concatenation.
--
-- 'transcriptionMessages', 'artifactsConcatenationConfiguration_transcriptionMessages' - The configuration for the transcription messages artifacts
-- concatenation.
--
-- 'meetingEvents', 'artifactsConcatenationConfiguration_meetingEvents' - The configuration for the meeting events artifacts concatenation.
--
-- 'compositedVideo', 'artifactsConcatenationConfiguration_compositedVideo' - The configuration for the composited video artifacts concatenation.
newArtifactsConcatenationConfiguration ::
  -- | 'audio'
  AudioConcatenationConfiguration ->
  -- | 'video'
  VideoConcatenationConfiguration ->
  -- | 'content'
  ContentConcatenationConfiguration ->
  -- | 'dataChannel'
  DataChannelConcatenationConfiguration ->
  -- | 'transcriptionMessages'
  TranscriptionMessagesConcatenationConfiguration ->
  -- | 'meetingEvents'
  MeetingEventsConcatenationConfiguration ->
  -- | 'compositedVideo'
  CompositedVideoConcatenationConfiguration ->
  ArtifactsConcatenationConfiguration
newArtifactsConcatenationConfiguration
  pAudio_
  pVideo_
  pContent_
  pDataChannel_
  pTranscriptionMessages_
  pMeetingEvents_
  pCompositedVideo_ =
    ArtifactsConcatenationConfiguration'
      { audio =
          pAudio_,
        video = pVideo_,
        content = pContent_,
        dataChannel = pDataChannel_,
        transcriptionMessages =
          pTranscriptionMessages_,
        meetingEvents = pMeetingEvents_,
        compositedVideo = pCompositedVideo_
      }

-- | The configuration for the audio artifacts concatenation.
artifactsConcatenationConfiguration_audio :: Lens.Lens' ArtifactsConcatenationConfiguration AudioConcatenationConfiguration
artifactsConcatenationConfiguration_audio = Lens.lens (\ArtifactsConcatenationConfiguration' {audio} -> audio) (\s@ArtifactsConcatenationConfiguration' {} a -> s {audio = a} :: ArtifactsConcatenationConfiguration)

-- | The configuration for the video artifacts concatenation.
artifactsConcatenationConfiguration_video :: Lens.Lens' ArtifactsConcatenationConfiguration VideoConcatenationConfiguration
artifactsConcatenationConfiguration_video = Lens.lens (\ArtifactsConcatenationConfiguration' {video} -> video) (\s@ArtifactsConcatenationConfiguration' {} a -> s {video = a} :: ArtifactsConcatenationConfiguration)

-- | The configuration for the content artifacts concatenation.
artifactsConcatenationConfiguration_content :: Lens.Lens' ArtifactsConcatenationConfiguration ContentConcatenationConfiguration
artifactsConcatenationConfiguration_content = Lens.lens (\ArtifactsConcatenationConfiguration' {content} -> content) (\s@ArtifactsConcatenationConfiguration' {} a -> s {content = a} :: ArtifactsConcatenationConfiguration)

-- | The configuration for the data channel artifacts concatenation.
artifactsConcatenationConfiguration_dataChannel :: Lens.Lens' ArtifactsConcatenationConfiguration DataChannelConcatenationConfiguration
artifactsConcatenationConfiguration_dataChannel = Lens.lens (\ArtifactsConcatenationConfiguration' {dataChannel} -> dataChannel) (\s@ArtifactsConcatenationConfiguration' {} a -> s {dataChannel = a} :: ArtifactsConcatenationConfiguration)

-- | The configuration for the transcription messages artifacts
-- concatenation.
artifactsConcatenationConfiguration_transcriptionMessages :: Lens.Lens' ArtifactsConcatenationConfiguration TranscriptionMessagesConcatenationConfiguration
artifactsConcatenationConfiguration_transcriptionMessages = Lens.lens (\ArtifactsConcatenationConfiguration' {transcriptionMessages} -> transcriptionMessages) (\s@ArtifactsConcatenationConfiguration' {} a -> s {transcriptionMessages = a} :: ArtifactsConcatenationConfiguration)

-- | The configuration for the meeting events artifacts concatenation.
artifactsConcatenationConfiguration_meetingEvents :: Lens.Lens' ArtifactsConcatenationConfiguration MeetingEventsConcatenationConfiguration
artifactsConcatenationConfiguration_meetingEvents = Lens.lens (\ArtifactsConcatenationConfiguration' {meetingEvents} -> meetingEvents) (\s@ArtifactsConcatenationConfiguration' {} a -> s {meetingEvents = a} :: ArtifactsConcatenationConfiguration)

-- | The configuration for the composited video artifacts concatenation.
artifactsConcatenationConfiguration_compositedVideo :: Lens.Lens' ArtifactsConcatenationConfiguration CompositedVideoConcatenationConfiguration
artifactsConcatenationConfiguration_compositedVideo = Lens.lens (\ArtifactsConcatenationConfiguration' {compositedVideo} -> compositedVideo) (\s@ArtifactsConcatenationConfiguration' {} a -> s {compositedVideo = a} :: ArtifactsConcatenationConfiguration)

instance
  Data.FromJSON
    ArtifactsConcatenationConfiguration
  where
  parseJSON =
    Data.withObject
      "ArtifactsConcatenationConfiguration"
      ( \x ->
          ArtifactsConcatenationConfiguration'
            Prelude.<$> (x Data..: "Audio")
            Prelude.<*> (x Data..: "Video")
            Prelude.<*> (x Data..: "Content")
            Prelude.<*> (x Data..: "DataChannel")
            Prelude.<*> (x Data..: "TranscriptionMessages")
            Prelude.<*> (x Data..: "MeetingEvents")
            Prelude.<*> (x Data..: "CompositedVideo")
      )

instance
  Prelude.Hashable
    ArtifactsConcatenationConfiguration
  where
  hashWithSalt
    _salt
    ArtifactsConcatenationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` audio
        `Prelude.hashWithSalt` video
        `Prelude.hashWithSalt` content
        `Prelude.hashWithSalt` dataChannel
        `Prelude.hashWithSalt` transcriptionMessages
        `Prelude.hashWithSalt` meetingEvents
        `Prelude.hashWithSalt` compositedVideo

instance
  Prelude.NFData
    ArtifactsConcatenationConfiguration
  where
  rnf ArtifactsConcatenationConfiguration' {..} =
    Prelude.rnf audio `Prelude.seq`
      Prelude.rnf video `Prelude.seq`
        Prelude.rnf content `Prelude.seq`
          Prelude.rnf dataChannel `Prelude.seq`
            Prelude.rnf transcriptionMessages `Prelude.seq`
              Prelude.rnf meetingEvents `Prelude.seq`
                Prelude.rnf compositedVideo

instance
  Data.ToJSON
    ArtifactsConcatenationConfiguration
  where
  toJSON ArtifactsConcatenationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Audio" Data..= audio),
            Prelude.Just ("Video" Data..= video),
            Prelude.Just ("Content" Data..= content),
            Prelude.Just ("DataChannel" Data..= dataChannel),
            Prelude.Just
              ( "TranscriptionMessages"
                  Data..= transcriptionMessages
              ),
            Prelude.Just ("MeetingEvents" Data..= meetingEvents),
            Prelude.Just
              ("CompositedVideo" Data..= compositedVideo)
          ]
      )
