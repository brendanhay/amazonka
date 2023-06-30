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
-- Module      : Amazonka.Transcribe.Types.ChannelDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.ChannelDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.ParticipantRole

-- | Makes it possible to specify which speaker is on which channel. For
-- example, if your agent is the first participant to speak, you would set
-- @ChannelId@ to @0@ (to indicate the first channel) and @ParticipantRole@
-- to @AGENT@ (to indicate that it\'s the agent speaking).
--
-- /See:/ 'newChannelDefinition' smart constructor.
data ChannelDefinition = ChannelDefinition'
  { -- | Specify the audio channel you want to define.
    channelId :: Prelude.Maybe Prelude.Natural,
    -- | Specify the speaker you want to define. Omitting this parameter is
    -- equivalent to specifying both participants.
    participantRole :: Prelude.Maybe ParticipantRole
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'channelDefinition_channelId' - Specify the audio channel you want to define.
--
-- 'participantRole', 'channelDefinition_participantRole' - Specify the speaker you want to define. Omitting this parameter is
-- equivalent to specifying both participants.
newChannelDefinition ::
  ChannelDefinition
newChannelDefinition =
  ChannelDefinition'
    { channelId = Prelude.Nothing,
      participantRole = Prelude.Nothing
    }

-- | Specify the audio channel you want to define.
channelDefinition_channelId :: Lens.Lens' ChannelDefinition (Prelude.Maybe Prelude.Natural)
channelDefinition_channelId = Lens.lens (\ChannelDefinition' {channelId} -> channelId) (\s@ChannelDefinition' {} a -> s {channelId = a} :: ChannelDefinition)

-- | Specify the speaker you want to define. Omitting this parameter is
-- equivalent to specifying both participants.
channelDefinition_participantRole :: Lens.Lens' ChannelDefinition (Prelude.Maybe ParticipantRole)
channelDefinition_participantRole = Lens.lens (\ChannelDefinition' {participantRole} -> participantRole) (\s@ChannelDefinition' {} a -> s {participantRole = a} :: ChannelDefinition)

instance Data.FromJSON ChannelDefinition where
  parseJSON =
    Data.withObject
      "ChannelDefinition"
      ( \x ->
          ChannelDefinition'
            Prelude.<$> (x Data..:? "ChannelId")
            Prelude.<*> (x Data..:? "ParticipantRole")
      )

instance Prelude.Hashable ChannelDefinition where
  hashWithSalt _salt ChannelDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` channelId
      `Prelude.hashWithSalt` participantRole

instance Prelude.NFData ChannelDefinition where
  rnf ChannelDefinition' {..} =
    Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf participantRole

instance Data.ToJSON ChannelDefinition where
  toJSON ChannelDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChannelId" Data..=) Prelude.<$> channelId,
            ("ParticipantRole" Data..=)
              Prelude.<$> participantRole
          ]
      )
