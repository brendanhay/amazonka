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
-- Module      : Network.AWS.Transcribe.Types.ChannelDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ChannelDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transcribe.Types.ParticipantRole

-- | For a call analytics job, an object that indicates the audio channel
-- that belongs to the agent and the audio channel that belongs to the
-- customer.
--
-- /See:/ 'newChannelDefinition' smart constructor.
data ChannelDefinition = ChannelDefinition'
  { -- | A value that indicates the audio channel.
    channelId :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether the person speaking on the audio channel is the agent
    -- or customer.
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
-- 'channelId', 'channelDefinition_channelId' - A value that indicates the audio channel.
--
-- 'participantRole', 'channelDefinition_participantRole' - Indicates whether the person speaking on the audio channel is the agent
-- or customer.
newChannelDefinition ::
  ChannelDefinition
newChannelDefinition =
  ChannelDefinition'
    { channelId = Prelude.Nothing,
      participantRole = Prelude.Nothing
    }

-- | A value that indicates the audio channel.
channelDefinition_channelId :: Lens.Lens' ChannelDefinition (Prelude.Maybe Prelude.Natural)
channelDefinition_channelId = Lens.lens (\ChannelDefinition' {channelId} -> channelId) (\s@ChannelDefinition' {} a -> s {channelId = a} :: ChannelDefinition)

-- | Indicates whether the person speaking on the audio channel is the agent
-- or customer.
channelDefinition_participantRole :: Lens.Lens' ChannelDefinition (Prelude.Maybe ParticipantRole)
channelDefinition_participantRole = Lens.lens (\ChannelDefinition' {participantRole} -> participantRole) (\s@ChannelDefinition' {} a -> s {participantRole = a} :: ChannelDefinition)

instance Core.FromJSON ChannelDefinition where
  parseJSON =
    Core.withObject
      "ChannelDefinition"
      ( \x ->
          ChannelDefinition'
            Prelude.<$> (x Core..:? "ChannelId")
            Prelude.<*> (x Core..:? "ParticipantRole")
      )

instance Prelude.Hashable ChannelDefinition

instance Prelude.NFData ChannelDefinition

instance Core.ToJSON ChannelDefinition where
  toJSON ChannelDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ChannelId" Core..=) Prelude.<$> channelId,
            ("ParticipantRole" Core..=)
              Prelude.<$> participantRole
          ]
      )
