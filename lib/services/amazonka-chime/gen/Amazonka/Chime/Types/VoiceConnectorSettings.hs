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
-- Module      : Amazonka.Chime.Types.VoiceConnectorSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.VoiceConnectorSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Chime Voice Connector settings. Includes any Amazon S3
-- buckets designated for storing call detail records.
--
-- /See:/ 'newVoiceConnectorSettings' smart constructor.
data VoiceConnectorSettings = VoiceConnectorSettings'
  { -- | The Amazon S3 bucket designated for call detail record storage.
    cdrBucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceConnectorSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cdrBucket', 'voiceConnectorSettings_cdrBucket' - The Amazon S3 bucket designated for call detail record storage.
newVoiceConnectorSettings ::
  VoiceConnectorSettings
newVoiceConnectorSettings =
  VoiceConnectorSettings'
    { cdrBucket =
        Prelude.Nothing
    }

-- | The Amazon S3 bucket designated for call detail record storage.
voiceConnectorSettings_cdrBucket :: Lens.Lens' VoiceConnectorSettings (Prelude.Maybe Prelude.Text)
voiceConnectorSettings_cdrBucket = Lens.lens (\VoiceConnectorSettings' {cdrBucket} -> cdrBucket) (\s@VoiceConnectorSettings' {} a -> s {cdrBucket = a} :: VoiceConnectorSettings)

instance Data.FromJSON VoiceConnectorSettings where
  parseJSON =
    Data.withObject
      "VoiceConnectorSettings"
      ( \x ->
          VoiceConnectorSettings'
            Prelude.<$> (x Data..:? "CdrBucket")
      )

instance Prelude.Hashable VoiceConnectorSettings where
  hashWithSalt _salt VoiceConnectorSettings' {..} =
    _salt `Prelude.hashWithSalt` cdrBucket

instance Prelude.NFData VoiceConnectorSettings where
  rnf VoiceConnectorSettings' {..} =
    Prelude.rnf cdrBucket

instance Data.ToJSON VoiceConnectorSettings where
  toJSON VoiceConnectorSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("CdrBucket" Data..=) Prelude.<$> cdrBucket]
      )
