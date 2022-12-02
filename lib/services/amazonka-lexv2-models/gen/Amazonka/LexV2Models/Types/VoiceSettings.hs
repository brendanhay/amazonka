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
-- Module      : Amazonka.LexV2Models.Types.VoiceSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.VoiceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.VoiceEngine
import qualified Amazonka.Prelude as Prelude

-- | Defines settings for using an Amazon Polly voice to communicate with a
-- user.
--
-- /See:/ 'newVoiceSettings' smart constructor.
data VoiceSettings = VoiceSettings'
  { -- | Indicates the type of Amazon Polly voice that Amazon Lex should use for
    -- voice interaction with the user. For more information, see the
    -- <https://docs.aws.amazon.com/polly/latest/dg/API_SynthesizeSpeech.html#polly-SynthesizeSpeech-request-Engine engine parameter of the SynthesizeSpeech operation>
    -- in the /Amazon Polly developer guide/.
    --
    -- If you do not specify a value, the default is @standard@.
    engine :: Prelude.Maybe VoiceEngine,
    -- | The identifier of the Amazon Polly voice to use.
    voiceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engine', 'voiceSettings_engine' - Indicates the type of Amazon Polly voice that Amazon Lex should use for
-- voice interaction with the user. For more information, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_SynthesizeSpeech.html#polly-SynthesizeSpeech-request-Engine engine parameter of the SynthesizeSpeech operation>
-- in the /Amazon Polly developer guide/.
--
-- If you do not specify a value, the default is @standard@.
--
-- 'voiceId', 'voiceSettings_voiceId' - The identifier of the Amazon Polly voice to use.
newVoiceSettings ::
  -- | 'voiceId'
  Prelude.Text ->
  VoiceSettings
newVoiceSettings pVoiceId_ =
  VoiceSettings'
    { engine = Prelude.Nothing,
      voiceId = pVoiceId_
    }

-- | Indicates the type of Amazon Polly voice that Amazon Lex should use for
-- voice interaction with the user. For more information, see the
-- <https://docs.aws.amazon.com/polly/latest/dg/API_SynthesizeSpeech.html#polly-SynthesizeSpeech-request-Engine engine parameter of the SynthesizeSpeech operation>
-- in the /Amazon Polly developer guide/.
--
-- If you do not specify a value, the default is @standard@.
voiceSettings_engine :: Lens.Lens' VoiceSettings (Prelude.Maybe VoiceEngine)
voiceSettings_engine = Lens.lens (\VoiceSettings' {engine} -> engine) (\s@VoiceSettings' {} a -> s {engine = a} :: VoiceSettings)

-- | The identifier of the Amazon Polly voice to use.
voiceSettings_voiceId :: Lens.Lens' VoiceSettings Prelude.Text
voiceSettings_voiceId = Lens.lens (\VoiceSettings' {voiceId} -> voiceId) (\s@VoiceSettings' {} a -> s {voiceId = a} :: VoiceSettings)

instance Data.FromJSON VoiceSettings where
  parseJSON =
    Data.withObject
      "VoiceSettings"
      ( \x ->
          VoiceSettings'
            Prelude.<$> (x Data..:? "engine")
            Prelude.<*> (x Data..: "voiceId")
      )

instance Prelude.Hashable VoiceSettings where
  hashWithSalt _salt VoiceSettings' {..} =
    _salt `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` voiceId

instance Prelude.NFData VoiceSettings where
  rnf VoiceSettings' {..} =
    Prelude.rnf engine
      `Prelude.seq` Prelude.rnf voiceId

instance Data.ToJSON VoiceSettings where
  toJSON VoiceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("engine" Data..=) Prelude.<$> engine,
            Prelude.Just ("voiceId" Data..= voiceId)
          ]
      )
