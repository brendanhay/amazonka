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
-- Module      : Amazonka.ElasticTranscoder.Types.CaptionSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.CaptionSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.Encryption
import qualified Amazonka.Prelude as Prelude

-- | A source file for the input sidecar captions used during the transcoding
-- process.
--
-- /See:/ 'newCaptionSource' smart constructor.
data CaptionSource = CaptionSource'
  { -- | The encryption settings, if any, that Elastic Transcoder needs to
    -- decyrpt your caption sources, or that you want Elastic Transcoder to
    -- apply to your caption sources.
    encryption :: Prelude.Maybe Encryption,
    -- | The name of the sidecar caption file that you want Elastic Transcoder to
    -- include in the output file.
    key :: Prelude.Maybe Prelude.Text,
    -- | The label of the caption shown in the player when choosing a language.
    -- We recommend that you put the caption language name here, in the
    -- language of the captions.
    label :: Prelude.Maybe Prelude.Text,
    -- | A string that specifies the language of the caption. If you specified
    -- multiple inputs with captions, the caption language must match in order
    -- to be included in the output. Specify this as one of:
    --
    -- -   2-character ISO 639-1 code
    --
    -- -   3-character ISO 639-2 code
    --
    -- For more information on ISO language codes and language names, see the
    -- List of ISO 639-1 codes.
    language :: Prelude.Maybe Prelude.Text,
    -- | For clip generation or captions that do not start at the same time as
    -- the associated video file, the @TimeOffset@ tells Elastic Transcoder how
    -- much of the video to encode before including captions.
    --
    -- Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
    timeOffset :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'captionSource_encryption' - The encryption settings, if any, that Elastic Transcoder needs to
-- decyrpt your caption sources, or that you want Elastic Transcoder to
-- apply to your caption sources.
--
-- 'key', 'captionSource_key' - The name of the sidecar caption file that you want Elastic Transcoder to
-- include in the output file.
--
-- 'label', 'captionSource_label' - The label of the caption shown in the player when choosing a language.
-- We recommend that you put the caption language name here, in the
-- language of the captions.
--
-- 'language', 'captionSource_language' - A string that specifies the language of the caption. If you specified
-- multiple inputs with captions, the caption language must match in order
-- to be included in the output. Specify this as one of:
--
-- -   2-character ISO 639-1 code
--
-- -   3-character ISO 639-2 code
--
-- For more information on ISO language codes and language names, see the
-- List of ISO 639-1 codes.
--
-- 'timeOffset', 'captionSource_timeOffset' - For clip generation or captions that do not start at the same time as
-- the associated video file, the @TimeOffset@ tells Elastic Transcoder how
-- much of the video to encode before including captions.
--
-- Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
newCaptionSource ::
  CaptionSource
newCaptionSource =
  CaptionSource'
    { encryption = Prelude.Nothing,
      key = Prelude.Nothing,
      label = Prelude.Nothing,
      language = Prelude.Nothing,
      timeOffset = Prelude.Nothing
    }

-- | The encryption settings, if any, that Elastic Transcoder needs to
-- decyrpt your caption sources, or that you want Elastic Transcoder to
-- apply to your caption sources.
captionSource_encryption :: Lens.Lens' CaptionSource (Prelude.Maybe Encryption)
captionSource_encryption = Lens.lens (\CaptionSource' {encryption} -> encryption) (\s@CaptionSource' {} a -> s {encryption = a} :: CaptionSource)

-- | The name of the sidecar caption file that you want Elastic Transcoder to
-- include in the output file.
captionSource_key :: Lens.Lens' CaptionSource (Prelude.Maybe Prelude.Text)
captionSource_key = Lens.lens (\CaptionSource' {key} -> key) (\s@CaptionSource' {} a -> s {key = a} :: CaptionSource)

-- | The label of the caption shown in the player when choosing a language.
-- We recommend that you put the caption language name here, in the
-- language of the captions.
captionSource_label :: Lens.Lens' CaptionSource (Prelude.Maybe Prelude.Text)
captionSource_label = Lens.lens (\CaptionSource' {label} -> label) (\s@CaptionSource' {} a -> s {label = a} :: CaptionSource)

-- | A string that specifies the language of the caption. If you specified
-- multiple inputs with captions, the caption language must match in order
-- to be included in the output. Specify this as one of:
--
-- -   2-character ISO 639-1 code
--
-- -   3-character ISO 639-2 code
--
-- For more information on ISO language codes and language names, see the
-- List of ISO 639-1 codes.
captionSource_language :: Lens.Lens' CaptionSource (Prelude.Maybe Prelude.Text)
captionSource_language = Lens.lens (\CaptionSource' {language} -> language) (\s@CaptionSource' {} a -> s {language = a} :: CaptionSource)

-- | For clip generation or captions that do not start at the same time as
-- the associated video file, the @TimeOffset@ tells Elastic Transcoder how
-- much of the video to encode before including captions.
--
-- Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
captionSource_timeOffset :: Lens.Lens' CaptionSource (Prelude.Maybe Prelude.Text)
captionSource_timeOffset = Lens.lens (\CaptionSource' {timeOffset} -> timeOffset) (\s@CaptionSource' {} a -> s {timeOffset = a} :: CaptionSource)

instance Data.FromJSON CaptionSource where
  parseJSON =
    Data.withObject
      "CaptionSource"
      ( \x ->
          CaptionSource'
            Prelude.<$> (x Data..:? "Encryption")
            Prelude.<*> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "Language")
            Prelude.<*> (x Data..:? "TimeOffset")
      )

instance Prelude.Hashable CaptionSource where
  hashWithSalt _salt CaptionSource' {..} =
    _salt
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` timeOffset

instance Prelude.NFData CaptionSource where
  rnf CaptionSource' {..} =
    Prelude.rnf encryption `Prelude.seq`
      Prelude.rnf key `Prelude.seq`
        Prelude.rnf label `Prelude.seq`
          Prelude.rnf language `Prelude.seq`
            Prelude.rnf timeOffset

instance Data.ToJSON CaptionSource where
  toJSON CaptionSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Encryption" Data..=) Prelude.<$> encryption,
            ("Key" Data..=) Prelude.<$> key,
            ("Label" Data..=) Prelude.<$> label,
            ("Language" Data..=) Prelude.<$> language,
            ("TimeOffset" Data..=) Prelude.<$> timeOffset
          ]
      )
