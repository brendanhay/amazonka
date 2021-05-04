{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.VoiceChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceChannelRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the status and settings of the voice channel for an
-- application.
--
-- /See:/ 'newVoiceChannelRequest' smart constructor.
data VoiceChannelRequest = VoiceChannelRequest'
  { -- | Specifies whether to enable the voice channel for the application.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VoiceChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'voiceChannelRequest_enabled' - Specifies whether to enable the voice channel for the application.
newVoiceChannelRequest ::
  VoiceChannelRequest
newVoiceChannelRequest =
  VoiceChannelRequest' {enabled = Prelude.Nothing}

-- | Specifies whether to enable the voice channel for the application.
voiceChannelRequest_enabled :: Lens.Lens' VoiceChannelRequest (Prelude.Maybe Prelude.Bool)
voiceChannelRequest_enabled = Lens.lens (\VoiceChannelRequest' {enabled} -> enabled) (\s@VoiceChannelRequest' {} a -> s {enabled = a} :: VoiceChannelRequest)

instance Prelude.Hashable VoiceChannelRequest

instance Prelude.NFData VoiceChannelRequest

instance Prelude.ToJSON VoiceChannelRequest where
  toJSON VoiceChannelRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Enabled" Prelude..=) Prelude.<$> enabled]
      )
