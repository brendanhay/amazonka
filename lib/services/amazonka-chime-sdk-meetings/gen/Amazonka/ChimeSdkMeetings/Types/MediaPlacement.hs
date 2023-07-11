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
-- Module      : Amazonka.ChimeSdkMeetings.Types.MediaPlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.MediaPlacement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of endpoints used by clients to connect to the media service group
-- for an Amazon Chime SDK meeting.
--
-- /See:/ 'newMediaPlacement' smart constructor.
data MediaPlacement = MediaPlacement'
  { -- | The audio fallback URL.
    audioFallbackUrl :: Prelude.Maybe Prelude.Text,
    -- | The audio host URL.
    audioHostUrl :: Prelude.Maybe Prelude.Text,
    -- | The event ingestion URL.
    eventIngestionUrl :: Prelude.Maybe Prelude.Text,
    -- | The screen data URL.
    screenDataUrl :: Prelude.Maybe Prelude.Text,
    -- | The screen sharing URL.
    screenSharingUrl :: Prelude.Maybe Prelude.Text,
    -- | The screen viewing URL.
    screenViewingUrl :: Prelude.Maybe Prelude.Text,
    -- | The signaling URL.
    signalingUrl :: Prelude.Maybe Prelude.Text,
    -- | The turn control URL.
    turnControlUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioFallbackUrl', 'mediaPlacement_audioFallbackUrl' - The audio fallback URL.
--
-- 'audioHostUrl', 'mediaPlacement_audioHostUrl' - The audio host URL.
--
-- 'eventIngestionUrl', 'mediaPlacement_eventIngestionUrl' - The event ingestion URL.
--
-- 'screenDataUrl', 'mediaPlacement_screenDataUrl' - The screen data URL.
--
-- 'screenSharingUrl', 'mediaPlacement_screenSharingUrl' - The screen sharing URL.
--
-- 'screenViewingUrl', 'mediaPlacement_screenViewingUrl' - The screen viewing URL.
--
-- 'signalingUrl', 'mediaPlacement_signalingUrl' - The signaling URL.
--
-- 'turnControlUrl', 'mediaPlacement_turnControlUrl' - The turn control URL.
newMediaPlacement ::
  MediaPlacement
newMediaPlacement =
  MediaPlacement'
    { audioFallbackUrl = Prelude.Nothing,
      audioHostUrl = Prelude.Nothing,
      eventIngestionUrl = Prelude.Nothing,
      screenDataUrl = Prelude.Nothing,
      screenSharingUrl = Prelude.Nothing,
      screenViewingUrl = Prelude.Nothing,
      signalingUrl = Prelude.Nothing,
      turnControlUrl = Prelude.Nothing
    }

-- | The audio fallback URL.
mediaPlacement_audioFallbackUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_audioFallbackUrl = Lens.lens (\MediaPlacement' {audioFallbackUrl} -> audioFallbackUrl) (\s@MediaPlacement' {} a -> s {audioFallbackUrl = a} :: MediaPlacement)

-- | The audio host URL.
mediaPlacement_audioHostUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_audioHostUrl = Lens.lens (\MediaPlacement' {audioHostUrl} -> audioHostUrl) (\s@MediaPlacement' {} a -> s {audioHostUrl = a} :: MediaPlacement)

-- | The event ingestion URL.
mediaPlacement_eventIngestionUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_eventIngestionUrl = Lens.lens (\MediaPlacement' {eventIngestionUrl} -> eventIngestionUrl) (\s@MediaPlacement' {} a -> s {eventIngestionUrl = a} :: MediaPlacement)

-- | The screen data URL.
mediaPlacement_screenDataUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_screenDataUrl = Lens.lens (\MediaPlacement' {screenDataUrl} -> screenDataUrl) (\s@MediaPlacement' {} a -> s {screenDataUrl = a} :: MediaPlacement)

-- | The screen sharing URL.
mediaPlacement_screenSharingUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_screenSharingUrl = Lens.lens (\MediaPlacement' {screenSharingUrl} -> screenSharingUrl) (\s@MediaPlacement' {} a -> s {screenSharingUrl = a} :: MediaPlacement)

-- | The screen viewing URL.
mediaPlacement_screenViewingUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_screenViewingUrl = Lens.lens (\MediaPlacement' {screenViewingUrl} -> screenViewingUrl) (\s@MediaPlacement' {} a -> s {screenViewingUrl = a} :: MediaPlacement)

-- | The signaling URL.
mediaPlacement_signalingUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_signalingUrl = Lens.lens (\MediaPlacement' {signalingUrl} -> signalingUrl) (\s@MediaPlacement' {} a -> s {signalingUrl = a} :: MediaPlacement)

-- | The turn control URL.
mediaPlacement_turnControlUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_turnControlUrl = Lens.lens (\MediaPlacement' {turnControlUrl} -> turnControlUrl) (\s@MediaPlacement' {} a -> s {turnControlUrl = a} :: MediaPlacement)

instance Data.FromJSON MediaPlacement where
  parseJSON =
    Data.withObject
      "MediaPlacement"
      ( \x ->
          MediaPlacement'
            Prelude.<$> (x Data..:? "AudioFallbackUrl")
            Prelude.<*> (x Data..:? "AudioHostUrl")
            Prelude.<*> (x Data..:? "EventIngestionUrl")
            Prelude.<*> (x Data..:? "ScreenDataUrl")
            Prelude.<*> (x Data..:? "ScreenSharingUrl")
            Prelude.<*> (x Data..:? "ScreenViewingUrl")
            Prelude.<*> (x Data..:? "SignalingUrl")
            Prelude.<*> (x Data..:? "TurnControlUrl")
      )

instance Prelude.Hashable MediaPlacement where
  hashWithSalt _salt MediaPlacement' {..} =
    _salt
      `Prelude.hashWithSalt` audioFallbackUrl
      `Prelude.hashWithSalt` audioHostUrl
      `Prelude.hashWithSalt` eventIngestionUrl
      `Prelude.hashWithSalt` screenDataUrl
      `Prelude.hashWithSalt` screenSharingUrl
      `Prelude.hashWithSalt` screenViewingUrl
      `Prelude.hashWithSalt` signalingUrl
      `Prelude.hashWithSalt` turnControlUrl

instance Prelude.NFData MediaPlacement where
  rnf MediaPlacement' {..} =
    Prelude.rnf audioFallbackUrl
      `Prelude.seq` Prelude.rnf audioHostUrl
      `Prelude.seq` Prelude.rnf eventIngestionUrl
      `Prelude.seq` Prelude.rnf screenDataUrl
      `Prelude.seq` Prelude.rnf screenSharingUrl
      `Prelude.seq` Prelude.rnf screenViewingUrl
      `Prelude.seq` Prelude.rnf signalingUrl
      `Prelude.seq` Prelude.rnf turnControlUrl
