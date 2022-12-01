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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.MediaPlacement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A set of endpoints used by clients to connect to the media service group
-- for an Amazon Chime SDK meeting.
--
-- /See:/ 'newMediaPlacement' smart constructor.
data MediaPlacement = MediaPlacement'
  { -- | The signaling URL.
    signalingUrl :: Prelude.Maybe Prelude.Text,
    -- | The screen viewing URL.
    screenViewingUrl :: Prelude.Maybe Prelude.Text,
    -- | The event ingestion URL.
    eventIngestionUrl :: Prelude.Maybe Prelude.Text,
    -- | The audio host URL.
    audioHostUrl :: Prelude.Maybe Prelude.Text,
    -- | The screen sharing URL.
    screenSharingUrl :: Prelude.Maybe Prelude.Text,
    -- | The screen data URL.
    screenDataUrl :: Prelude.Maybe Prelude.Text,
    -- | The audio fallback URL.
    audioFallbackUrl :: Prelude.Maybe Prelude.Text,
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
-- 'signalingUrl', 'mediaPlacement_signalingUrl' - The signaling URL.
--
-- 'screenViewingUrl', 'mediaPlacement_screenViewingUrl' - The screen viewing URL.
--
-- 'eventIngestionUrl', 'mediaPlacement_eventIngestionUrl' - The event ingestion URL.
--
-- 'audioHostUrl', 'mediaPlacement_audioHostUrl' - The audio host URL.
--
-- 'screenSharingUrl', 'mediaPlacement_screenSharingUrl' - The screen sharing URL.
--
-- 'screenDataUrl', 'mediaPlacement_screenDataUrl' - The screen data URL.
--
-- 'audioFallbackUrl', 'mediaPlacement_audioFallbackUrl' - The audio fallback URL.
--
-- 'turnControlUrl', 'mediaPlacement_turnControlUrl' - The turn control URL.
newMediaPlacement ::
  MediaPlacement
newMediaPlacement =
  MediaPlacement'
    { signalingUrl = Prelude.Nothing,
      screenViewingUrl = Prelude.Nothing,
      eventIngestionUrl = Prelude.Nothing,
      audioHostUrl = Prelude.Nothing,
      screenSharingUrl = Prelude.Nothing,
      screenDataUrl = Prelude.Nothing,
      audioFallbackUrl = Prelude.Nothing,
      turnControlUrl = Prelude.Nothing
    }

-- | The signaling URL.
mediaPlacement_signalingUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_signalingUrl = Lens.lens (\MediaPlacement' {signalingUrl} -> signalingUrl) (\s@MediaPlacement' {} a -> s {signalingUrl = a} :: MediaPlacement)

-- | The screen viewing URL.
mediaPlacement_screenViewingUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_screenViewingUrl = Lens.lens (\MediaPlacement' {screenViewingUrl} -> screenViewingUrl) (\s@MediaPlacement' {} a -> s {screenViewingUrl = a} :: MediaPlacement)

-- | The event ingestion URL.
mediaPlacement_eventIngestionUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_eventIngestionUrl = Lens.lens (\MediaPlacement' {eventIngestionUrl} -> eventIngestionUrl) (\s@MediaPlacement' {} a -> s {eventIngestionUrl = a} :: MediaPlacement)

-- | The audio host URL.
mediaPlacement_audioHostUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_audioHostUrl = Lens.lens (\MediaPlacement' {audioHostUrl} -> audioHostUrl) (\s@MediaPlacement' {} a -> s {audioHostUrl = a} :: MediaPlacement)

-- | The screen sharing URL.
mediaPlacement_screenSharingUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_screenSharingUrl = Lens.lens (\MediaPlacement' {screenSharingUrl} -> screenSharingUrl) (\s@MediaPlacement' {} a -> s {screenSharingUrl = a} :: MediaPlacement)

-- | The screen data URL.
mediaPlacement_screenDataUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_screenDataUrl = Lens.lens (\MediaPlacement' {screenDataUrl} -> screenDataUrl) (\s@MediaPlacement' {} a -> s {screenDataUrl = a} :: MediaPlacement)

-- | The audio fallback URL.
mediaPlacement_audioFallbackUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_audioFallbackUrl = Lens.lens (\MediaPlacement' {audioFallbackUrl} -> audioFallbackUrl) (\s@MediaPlacement' {} a -> s {audioFallbackUrl = a} :: MediaPlacement)

-- | The turn control URL.
mediaPlacement_turnControlUrl :: Lens.Lens' MediaPlacement (Prelude.Maybe Prelude.Text)
mediaPlacement_turnControlUrl = Lens.lens (\MediaPlacement' {turnControlUrl} -> turnControlUrl) (\s@MediaPlacement' {} a -> s {turnControlUrl = a} :: MediaPlacement)

instance Core.FromJSON MediaPlacement where
  parseJSON =
    Core.withObject
      "MediaPlacement"
      ( \x ->
          MediaPlacement'
            Prelude.<$> (x Core..:? "SignalingUrl")
            Prelude.<*> (x Core..:? "ScreenViewingUrl")
            Prelude.<*> (x Core..:? "EventIngestionUrl")
            Prelude.<*> (x Core..:? "AudioHostUrl")
            Prelude.<*> (x Core..:? "ScreenSharingUrl")
            Prelude.<*> (x Core..:? "ScreenDataUrl")
            Prelude.<*> (x Core..:? "AudioFallbackUrl")
            Prelude.<*> (x Core..:? "TurnControlUrl")
      )

instance Prelude.Hashable MediaPlacement where
  hashWithSalt _salt MediaPlacement' {..} =
    _salt `Prelude.hashWithSalt` signalingUrl
      `Prelude.hashWithSalt` screenViewingUrl
      `Prelude.hashWithSalt` eventIngestionUrl
      `Prelude.hashWithSalt` audioHostUrl
      `Prelude.hashWithSalt` screenSharingUrl
      `Prelude.hashWithSalt` screenDataUrl
      `Prelude.hashWithSalt` audioFallbackUrl
      `Prelude.hashWithSalt` turnControlUrl

instance Prelude.NFData MediaPlacement where
  rnf MediaPlacement' {..} =
    Prelude.rnf signalingUrl
      `Prelude.seq` Prelude.rnf screenViewingUrl
      `Prelude.seq` Prelude.rnf eventIngestionUrl
      `Prelude.seq` Prelude.rnf audioHostUrl
      `Prelude.seq` Prelude.rnf screenSharingUrl
      `Prelude.seq` Prelude.rnf screenDataUrl
      `Prelude.seq` Prelude.rnf audioFallbackUrl
      `Prelude.seq` Prelude.rnf turnControlUrl
