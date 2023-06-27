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
-- Module      : Amazonka.AlexaBusiness.Types.Audio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Audio where

import Amazonka.AlexaBusiness.Types.Locale
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The audio message. There is a 1 MB limit on the audio file input and the
-- only supported format is MP3. To convert your MP3 audio files to an
-- Alexa-friendly,
--
-- required codec version (MPEG version 2) and bit rate (48 kbps), you
-- might use converter software. One option for this is a command-line
-- tool, FFmpeg. For more information, see
-- <https://www.ffmpeg.org/ FFmpeg>. The following command converts the
-- provided \<input-file> to an MP3 file that is played in the
-- announcement:
--
-- @ffmpeg -i \<input-file> -ac 2 -codec:a libmp3lame -b:a 48k -ar 16000 \<output-file.mp3>@
--
-- /See:/ 'newAudio' smart constructor.
data Audio = Audio'
  { -- | The locale of the audio message. Currently, en-US is supported.
    locale :: Locale,
    -- | The location of the audio file. Currently, S3 URLs are supported. Only
    -- S3 locations comprised of safe characters are valid. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#Safe%20Characters Safe Characters>.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Audio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'audio_locale' - The locale of the audio message. Currently, en-US is supported.
--
-- 'location', 'audio_location' - The location of the audio file. Currently, S3 URLs are supported. Only
-- S3 locations comprised of safe characters are valid. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#Safe%20Characters Safe Characters>.
newAudio ::
  -- | 'locale'
  Locale ->
  -- | 'location'
  Prelude.Text ->
  Audio
newAudio pLocale_ pLocation_ =
  Audio' {locale = pLocale_, location = pLocation_}

-- | The locale of the audio message. Currently, en-US is supported.
audio_locale :: Lens.Lens' Audio Locale
audio_locale = Lens.lens (\Audio' {locale} -> locale) (\s@Audio' {} a -> s {locale = a} :: Audio)

-- | The location of the audio file. Currently, S3 URLs are supported. Only
-- S3 locations comprised of safe characters are valid. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#Safe%20Characters Safe Characters>.
audio_location :: Lens.Lens' Audio Prelude.Text
audio_location = Lens.lens (\Audio' {location} -> location) (\s@Audio' {} a -> s {location = a} :: Audio)

instance Prelude.Hashable Audio where
  hashWithSalt _salt Audio' {..} =
    _salt
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` location

instance Prelude.NFData Audio where
  rnf Audio' {..} =
    Prelude.rnf locale
      `Prelude.seq` Prelude.rnf location

instance Data.ToJSON Audio where
  toJSON Audio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Locale" Data..= locale),
            Prelude.Just ("Location" Data..= location)
          ]
      )
