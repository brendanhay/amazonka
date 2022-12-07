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
-- Module      : Amazonka.LexV2Models.Types.AudioLogSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AudioLogSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.AudioLogDestination
import qualified Amazonka.Prelude as Prelude

-- | Settings for logging audio of conversations between Amazon Lex and a
-- user. You specify whether to log audio and the Amazon S3 bucket where
-- the audio file is stored.
--
-- /See:/ 'newAudioLogSetting' smart constructor.
data AudioLogSetting = AudioLogSetting'
  { -- | Determines whether audio logging in enabled for the bot.
    enabled :: Prelude.Bool,
    destination :: AudioLogDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioLogSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'audioLogSetting_enabled' - Determines whether audio logging in enabled for the bot.
--
-- 'destination', 'audioLogSetting_destination' - Undocumented member.
newAudioLogSetting ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'destination'
  AudioLogDestination ->
  AudioLogSetting
newAudioLogSetting pEnabled_ pDestination_ =
  AudioLogSetting'
    { enabled = pEnabled_,
      destination = pDestination_
    }

-- | Determines whether audio logging in enabled for the bot.
audioLogSetting_enabled :: Lens.Lens' AudioLogSetting Prelude.Bool
audioLogSetting_enabled = Lens.lens (\AudioLogSetting' {enabled} -> enabled) (\s@AudioLogSetting' {} a -> s {enabled = a} :: AudioLogSetting)

-- | Undocumented member.
audioLogSetting_destination :: Lens.Lens' AudioLogSetting AudioLogDestination
audioLogSetting_destination = Lens.lens (\AudioLogSetting' {destination} -> destination) (\s@AudioLogSetting' {} a -> s {destination = a} :: AudioLogSetting)

instance Data.FromJSON AudioLogSetting where
  parseJSON =
    Data.withObject
      "AudioLogSetting"
      ( \x ->
          AudioLogSetting'
            Prelude.<$> (x Data..: "enabled")
            Prelude.<*> (x Data..: "destination")
      )

instance Prelude.Hashable AudioLogSetting where
  hashWithSalt _salt AudioLogSetting' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` destination

instance Prelude.NFData AudioLogSetting where
  rnf AudioLogSetting' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf destination

instance Data.ToJSON AudioLogSetting where
  toJSON AudioLogSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("enabled" Data..= enabled),
            Prelude.Just ("destination" Data..= destination)
          ]
      )
