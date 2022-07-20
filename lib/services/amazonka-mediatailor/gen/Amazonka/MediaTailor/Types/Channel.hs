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
-- Module      : Amazonka.MediaTailor.Types.Channel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.Channel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.ResponseOutputItem
import Amazonka.MediaTailor.Types.SlateSource
import qualified Amazonka.Prelude as Prelude

-- | The configuration parameters for a channel.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | The tags to assign to the channel.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contains information about the slate used to fill gaps between programs
    -- in the schedule. You must configure FillerSlate if your channel uses an
    -- LINEAR PlaybackMode.
    fillerSlate :: Prelude.Maybe SlateSource,
    -- | The timestamp of when the channel was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The timestamp of when the channel was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Returns the state whether the channel is running or not.
    channelState :: Prelude.Text,
    -- | The name of the channel.
    channelName :: Prelude.Text,
    -- | The channel\'s output properties.
    outputs :: [ResponseOutputItem],
    -- | The ARN of the channel.
    arn :: Prelude.Text,
    -- | The type of playback mode for this channel.
    --
    -- LINEAR - Programs play back-to-back only once.
    --
    -- LOOP - Programs play back-to-back in an endless loop. When the last
    -- program in the schedule plays, playback loops back to the first program
    -- in the schedule.
    playbackMode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'channel_tags' - The tags to assign to the channel.
--
-- 'fillerSlate', 'channel_fillerSlate' - Contains information about the slate used to fill gaps between programs
-- in the schedule. You must configure FillerSlate if your channel uses an
-- LINEAR PlaybackMode.
--
-- 'lastModifiedTime', 'channel_lastModifiedTime' - The timestamp of when the channel was last modified.
--
-- 'creationTime', 'channel_creationTime' - The timestamp of when the channel was created.
--
-- 'channelState', 'channel_channelState' - Returns the state whether the channel is running or not.
--
-- 'channelName', 'channel_channelName' - The name of the channel.
--
-- 'outputs', 'channel_outputs' - The channel\'s output properties.
--
-- 'arn', 'channel_arn' - The ARN of the channel.
--
-- 'playbackMode', 'channel_playbackMode' - The type of playback mode for this channel.
--
-- LINEAR - Programs play back-to-back only once.
--
-- LOOP - Programs play back-to-back in an endless loop. When the last
-- program in the schedule plays, playback loops back to the first program
-- in the schedule.
newChannel ::
  -- | 'channelState'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'playbackMode'
  Prelude.Text ->
  Channel
newChannel
  pChannelState_
  pChannelName_
  pArn_
  pPlaybackMode_ =
    Channel'
      { tags = Prelude.Nothing,
        fillerSlate = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        channelState = pChannelState_,
        channelName = pChannelName_,
        outputs = Prelude.mempty,
        arn = pArn_,
        playbackMode = pPlaybackMode_
      }

-- | The tags to assign to the channel.
channel_tags :: Lens.Lens' Channel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channel_tags = Lens.lens (\Channel' {tags} -> tags) (\s@Channel' {} a -> s {tags = a} :: Channel) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about the slate used to fill gaps between programs
-- in the schedule. You must configure FillerSlate if your channel uses an
-- LINEAR PlaybackMode.
channel_fillerSlate :: Lens.Lens' Channel (Prelude.Maybe SlateSource)
channel_fillerSlate = Lens.lens (\Channel' {fillerSlate} -> fillerSlate) (\s@Channel' {} a -> s {fillerSlate = a} :: Channel)

-- | The timestamp of when the channel was last modified.
channel_lastModifiedTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastModifiedTime = Lens.lens (\Channel' {lastModifiedTime} -> lastModifiedTime) (\s@Channel' {} a -> s {lastModifiedTime = a} :: Channel) Prelude.. Lens.mapping Core._Time

-- | The timestamp of when the channel was created.
channel_creationTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_creationTime = Lens.lens (\Channel' {creationTime} -> creationTime) (\s@Channel' {} a -> s {creationTime = a} :: Channel) Prelude.. Lens.mapping Core._Time

-- | Returns the state whether the channel is running or not.
channel_channelState :: Lens.Lens' Channel Prelude.Text
channel_channelState = Lens.lens (\Channel' {channelState} -> channelState) (\s@Channel' {} a -> s {channelState = a} :: Channel)

-- | The name of the channel.
channel_channelName :: Lens.Lens' Channel Prelude.Text
channel_channelName = Lens.lens (\Channel' {channelName} -> channelName) (\s@Channel' {} a -> s {channelName = a} :: Channel)

-- | The channel\'s output properties.
channel_outputs :: Lens.Lens' Channel [ResponseOutputItem]
channel_outputs = Lens.lens (\Channel' {outputs} -> outputs) (\s@Channel' {} a -> s {outputs = a} :: Channel) Prelude.. Lens.coerced

-- | The ARN of the channel.
channel_arn :: Lens.Lens' Channel Prelude.Text
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | The type of playback mode for this channel.
--
-- LINEAR - Programs play back-to-back only once.
--
-- LOOP - Programs play back-to-back in an endless loop. When the last
-- program in the schedule plays, playback loops back to the first program
-- in the schedule.
channel_playbackMode :: Lens.Lens' Channel Prelude.Text
channel_playbackMode = Lens.lens (\Channel' {playbackMode} -> playbackMode) (\s@Channel' {} a -> s {playbackMode = a} :: Channel)

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "FillerSlate")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..: "ChannelState")
            Prelude.<*> (x Core..: "ChannelName")
            Prelude.<*> (x Core..:? "Outputs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Arn")
            Prelude.<*> (x Core..: "PlaybackMode")
      )

instance Prelude.Hashable Channel where
  hashWithSalt _salt Channel' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` fillerSlate
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` channelState
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` playbackMode

instance Prelude.NFData Channel where
  rnf Channel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf fillerSlate
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf channelState
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf playbackMode
