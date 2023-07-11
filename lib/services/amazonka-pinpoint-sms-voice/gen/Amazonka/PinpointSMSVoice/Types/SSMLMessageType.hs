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
-- Module      : Amazonka.PinpointSMSVoice.Types.SSMLMessageType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSMSVoice.Types.SSMLMessageType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that defines a message that contains SSML-formatted text.
--
-- /See:/ 'newSSMLMessageType' smart constructor.
data SSMLMessageType = SSMLMessageType'
  { -- | The language to use when delivering the message. For a complete list of
    -- supported languages, see the Amazon Polly Developer Guide.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The SSML-formatted text to deliver to the recipient.
    text :: Prelude.Maybe Prelude.Text,
    -- | The name of the voice that you want to use to deliver the message. For a
    -- complete list of supported voices, see the Amazon Polly Developer Guide.
    voiceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SSMLMessageType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'sSMLMessageType_languageCode' - The language to use when delivering the message. For a complete list of
-- supported languages, see the Amazon Polly Developer Guide.
--
-- 'text', 'sSMLMessageType_text' - The SSML-formatted text to deliver to the recipient.
--
-- 'voiceId', 'sSMLMessageType_voiceId' - The name of the voice that you want to use to deliver the message. For a
-- complete list of supported voices, see the Amazon Polly Developer Guide.
newSSMLMessageType ::
  SSMLMessageType
newSSMLMessageType =
  SSMLMessageType'
    { languageCode = Prelude.Nothing,
      text = Prelude.Nothing,
      voiceId = Prelude.Nothing
    }

-- | The language to use when delivering the message. For a complete list of
-- supported languages, see the Amazon Polly Developer Guide.
sSMLMessageType_languageCode :: Lens.Lens' SSMLMessageType (Prelude.Maybe Prelude.Text)
sSMLMessageType_languageCode = Lens.lens (\SSMLMessageType' {languageCode} -> languageCode) (\s@SSMLMessageType' {} a -> s {languageCode = a} :: SSMLMessageType)

-- | The SSML-formatted text to deliver to the recipient.
sSMLMessageType_text :: Lens.Lens' SSMLMessageType (Prelude.Maybe Prelude.Text)
sSMLMessageType_text = Lens.lens (\SSMLMessageType' {text} -> text) (\s@SSMLMessageType' {} a -> s {text = a} :: SSMLMessageType)

-- | The name of the voice that you want to use to deliver the message. For a
-- complete list of supported voices, see the Amazon Polly Developer Guide.
sSMLMessageType_voiceId :: Lens.Lens' SSMLMessageType (Prelude.Maybe Prelude.Text)
sSMLMessageType_voiceId = Lens.lens (\SSMLMessageType' {voiceId} -> voiceId) (\s@SSMLMessageType' {} a -> s {voiceId = a} :: SSMLMessageType)

instance Prelude.Hashable SSMLMessageType where
  hashWithSalt _salt SSMLMessageType' {..} =
    _salt
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` voiceId

instance Prelude.NFData SSMLMessageType where
  rnf SSMLMessageType' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf voiceId

instance Data.ToJSON SSMLMessageType where
  toJSON SSMLMessageType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("Text" Data..=) Prelude.<$> text,
            ("VoiceId" Data..=) Prelude.<$> voiceId
          ]
      )
