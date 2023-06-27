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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.Channel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.LogConfigurationForChannel
import Amazonka.MediaTailor.Types.ResponseOutputItem
import Amazonka.MediaTailor.Types.SlateSource
import qualified Amazonka.Prelude as Prelude

-- | The configuration parameters for a channel. For information about
-- MediaTailor channels, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-channels.html Working with channels>
-- in the /MediaTailor User Guide/.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | The timestamp of when the channel was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The slate used to fill gaps between programs in the schedule. You must
    -- configure filler slate if your channel uses the @LINEAR@ @PlaybackMode@.
    -- MediaTailor doesn\'t support filler slate for channels using the @LOOP@
    -- @PlaybackMode@.
    fillerSlate :: Prelude.Maybe SlateSource,
    -- | The timestamp of when the channel was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The tags to assign to the channel. Tags are key-value pairs that you can
    -- associate with Amazon resources to help with organization, access
    -- control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the channel.
    arn :: Prelude.Text,
    -- | The name of the channel.
    channelName :: Prelude.Text,
    -- | Returns the state whether the channel is running or not.
    channelState :: Prelude.Text,
    -- | The log configuration.
    logConfiguration :: LogConfigurationForChannel,
    -- | The channel\'s output properties.
    outputs :: [ResponseOutputItem],
    -- | The type of playback mode for this channel.
    --
    -- @LINEAR@ - Programs play back-to-back only once.
    --
    -- @LOOP@ - Programs play back-to-back in an endless loop. When the last
    -- program in the schedule plays, playback loops back to the first program
    -- in the schedule.
    playbackMode :: Prelude.Text,
    -- | The tier for this channel. STANDARD tier channels can contain live
    -- programs.
    tier :: Prelude.Text
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
-- 'creationTime', 'channel_creationTime' - The timestamp of when the channel was created.
--
-- 'fillerSlate', 'channel_fillerSlate' - The slate used to fill gaps between programs in the schedule. You must
-- configure filler slate if your channel uses the @LINEAR@ @PlaybackMode@.
-- MediaTailor doesn\'t support filler slate for channels using the @LOOP@
-- @PlaybackMode@.
--
-- 'lastModifiedTime', 'channel_lastModifiedTime' - The timestamp of when the channel was last modified.
--
-- 'tags', 'channel_tags' - The tags to assign to the channel. Tags are key-value pairs that you can
-- associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'arn', 'channel_arn' - The ARN of the channel.
--
-- 'channelName', 'channel_channelName' - The name of the channel.
--
-- 'channelState', 'channel_channelState' - Returns the state whether the channel is running or not.
--
-- 'logConfiguration', 'channel_logConfiguration' - The log configuration.
--
-- 'outputs', 'channel_outputs' - The channel\'s output properties.
--
-- 'playbackMode', 'channel_playbackMode' - The type of playback mode for this channel.
--
-- @LINEAR@ - Programs play back-to-back only once.
--
-- @LOOP@ - Programs play back-to-back in an endless loop. When the last
-- program in the schedule plays, playback loops back to the first program
-- in the schedule.
--
-- 'tier', 'channel_tier' - The tier for this channel. STANDARD tier channels can contain live
-- programs.
newChannel ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'channelState'
  Prelude.Text ->
  -- | 'logConfiguration'
  LogConfigurationForChannel ->
  -- | 'playbackMode'
  Prelude.Text ->
  -- | 'tier'
  Prelude.Text ->
  Channel
newChannel
  pArn_
  pChannelName_
  pChannelState_
  pLogConfiguration_
  pPlaybackMode_
  pTier_ =
    Channel'
      { creationTime = Prelude.Nothing,
        fillerSlate = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        channelName = pChannelName_,
        channelState = pChannelState_,
        logConfiguration = pLogConfiguration_,
        outputs = Prelude.mempty,
        playbackMode = pPlaybackMode_,
        tier = pTier_
      }

-- | The timestamp of when the channel was created.
channel_creationTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_creationTime = Lens.lens (\Channel' {creationTime} -> creationTime) (\s@Channel' {} a -> s {creationTime = a} :: Channel) Prelude.. Lens.mapping Data._Time

-- | The slate used to fill gaps between programs in the schedule. You must
-- configure filler slate if your channel uses the @LINEAR@ @PlaybackMode@.
-- MediaTailor doesn\'t support filler slate for channels using the @LOOP@
-- @PlaybackMode@.
channel_fillerSlate :: Lens.Lens' Channel (Prelude.Maybe SlateSource)
channel_fillerSlate = Lens.lens (\Channel' {fillerSlate} -> fillerSlate) (\s@Channel' {} a -> s {fillerSlate = a} :: Channel)

-- | The timestamp of when the channel was last modified.
channel_lastModifiedTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastModifiedTime = Lens.lens (\Channel' {lastModifiedTime} -> lastModifiedTime) (\s@Channel' {} a -> s {lastModifiedTime = a} :: Channel) Prelude.. Lens.mapping Data._Time

-- | The tags to assign to the channel. Tags are key-value pairs that you can
-- associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
channel_tags :: Lens.Lens' Channel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channel_tags = Lens.lens (\Channel' {tags} -> tags) (\s@Channel' {} a -> s {tags = a} :: Channel) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the channel.
channel_arn :: Lens.Lens' Channel Prelude.Text
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | The name of the channel.
channel_channelName :: Lens.Lens' Channel Prelude.Text
channel_channelName = Lens.lens (\Channel' {channelName} -> channelName) (\s@Channel' {} a -> s {channelName = a} :: Channel)

-- | Returns the state whether the channel is running or not.
channel_channelState :: Lens.Lens' Channel Prelude.Text
channel_channelState = Lens.lens (\Channel' {channelState} -> channelState) (\s@Channel' {} a -> s {channelState = a} :: Channel)

-- | The log configuration.
channel_logConfiguration :: Lens.Lens' Channel LogConfigurationForChannel
channel_logConfiguration = Lens.lens (\Channel' {logConfiguration} -> logConfiguration) (\s@Channel' {} a -> s {logConfiguration = a} :: Channel)

-- | The channel\'s output properties.
channel_outputs :: Lens.Lens' Channel [ResponseOutputItem]
channel_outputs = Lens.lens (\Channel' {outputs} -> outputs) (\s@Channel' {} a -> s {outputs = a} :: Channel) Prelude.. Lens.coerced

-- | The type of playback mode for this channel.
--
-- @LINEAR@ - Programs play back-to-back only once.
--
-- @LOOP@ - Programs play back-to-back in an endless loop. When the last
-- program in the schedule plays, playback loops back to the first program
-- in the schedule.
channel_playbackMode :: Lens.Lens' Channel Prelude.Text
channel_playbackMode = Lens.lens (\Channel' {playbackMode} -> playbackMode) (\s@Channel' {} a -> s {playbackMode = a} :: Channel)

-- | The tier for this channel. STANDARD tier channels can contain live
-- programs.
channel_tier :: Lens.Lens' Channel Prelude.Text
channel_tier = Lens.lens (\Channel' {tier} -> tier) (\s@Channel' {} a -> s {tier = a} :: Channel)

instance Data.FromJSON Channel where
  parseJSON =
    Data.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FillerSlate")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "ChannelName")
            Prelude.<*> (x Data..: "ChannelState")
            Prelude.<*> (x Data..: "LogConfiguration")
            Prelude.<*> (x Data..:? "Outputs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "PlaybackMode")
            Prelude.<*> (x Data..: "Tier")
      )

instance Prelude.Hashable Channel where
  hashWithSalt _salt Channel' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` fillerSlate
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` channelState
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` playbackMode
      `Prelude.hashWithSalt` tier

instance Prelude.NFData Channel where
  rnf Channel' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf fillerSlate
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelState
      `Prelude.seq` Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf playbackMode
      `Prelude.seq` Prelude.rnf tier
