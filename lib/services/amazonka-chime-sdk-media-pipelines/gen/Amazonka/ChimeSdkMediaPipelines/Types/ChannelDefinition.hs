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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ChannelDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ChannelDefinition where

import Amazonka.ChimeSdkMediaPipelines.Types.ParticipantRole
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines an audio channel in a Kinesis video stream.
--
-- /See:/ 'newChannelDefinition' smart constructor.
data ChannelDefinition = ChannelDefinition'
  { -- | Specifies whether the audio in a channel belongs to the @AGENT@ or
    -- @CUSTOMER@.
    participantRole :: Prelude.Maybe ParticipantRole,
    -- | The channel ID.
    channelId :: Prelude.Natural
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
-- 'participantRole', 'channelDefinition_participantRole' - Specifies whether the audio in a channel belongs to the @AGENT@ or
-- @CUSTOMER@.
--
-- 'channelId', 'channelDefinition_channelId' - The channel ID.
newChannelDefinition ::
  -- | 'channelId'
  Prelude.Natural ->
  ChannelDefinition
newChannelDefinition pChannelId_ =
  ChannelDefinition'
    { participantRole =
        Prelude.Nothing,
      channelId = pChannelId_
    }

-- | Specifies whether the audio in a channel belongs to the @AGENT@ or
-- @CUSTOMER@.
channelDefinition_participantRole :: Lens.Lens' ChannelDefinition (Prelude.Maybe ParticipantRole)
channelDefinition_participantRole = Lens.lens (\ChannelDefinition' {participantRole} -> participantRole) (\s@ChannelDefinition' {} a -> s {participantRole = a} :: ChannelDefinition)

-- | The channel ID.
channelDefinition_channelId :: Lens.Lens' ChannelDefinition Prelude.Natural
channelDefinition_channelId = Lens.lens (\ChannelDefinition' {channelId} -> channelId) (\s@ChannelDefinition' {} a -> s {channelId = a} :: ChannelDefinition)

instance Data.FromJSON ChannelDefinition where
  parseJSON =
    Data.withObject
      "ChannelDefinition"
      ( \x ->
          ChannelDefinition'
            Prelude.<$> (x Data..:? "ParticipantRole")
            Prelude.<*> (x Data..: "ChannelId")
      )

instance Prelude.Hashable ChannelDefinition where
  hashWithSalt _salt ChannelDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` participantRole
      `Prelude.hashWithSalt` channelId

instance Prelude.NFData ChannelDefinition where
  rnf ChannelDefinition' {..} =
    Prelude.rnf participantRole
      `Prelude.seq` Prelude.rnf channelId

instance Data.ToJSON ChannelDefinition where
  toJSON ChannelDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParticipantRole" Data..=)
              Prelude.<$> participantRole,
            Prelude.Just ("ChannelId" Data..= channelId)
          ]
      )
