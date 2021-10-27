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
-- Module      : Network.AWS.LexV2Models.Types.VoiceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.VoiceSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines settings for using an Amazon Polly voice to communicate with a
-- user.
--
-- /See:/ 'newVoiceSettings' smart constructor.
data VoiceSettings = VoiceSettings'
  { -- | The identifier of the Amazon Polly voice to use.
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
-- 'voiceId', 'voiceSettings_voiceId' - The identifier of the Amazon Polly voice to use.
newVoiceSettings ::
  -- | 'voiceId'
  Prelude.Text ->
  VoiceSettings
newVoiceSettings pVoiceId_ =
  VoiceSettings' {voiceId = pVoiceId_}

-- | The identifier of the Amazon Polly voice to use.
voiceSettings_voiceId :: Lens.Lens' VoiceSettings Prelude.Text
voiceSettings_voiceId = Lens.lens (\VoiceSettings' {voiceId} -> voiceId) (\s@VoiceSettings' {} a -> s {voiceId = a} :: VoiceSettings)

instance Core.FromJSON VoiceSettings where
  parseJSON =
    Core.withObject
      "VoiceSettings"
      ( \x ->
          VoiceSettings' Prelude.<$> (x Core..: "voiceId")
      )

instance Prelude.Hashable VoiceSettings

instance Prelude.NFData VoiceSettings

instance Core.ToJSON VoiceSettings where
  toJSON VoiceSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("voiceId" Core..= voiceId)]
      )
