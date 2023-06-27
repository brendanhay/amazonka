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
-- Module      : Amazonka.IVS.Types.Channel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.Channel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.ChannelLatencyMode
import Amazonka.IVS.Types.ChannelType
import Amazonka.IVS.Types.TranscodePreset
import qualified Amazonka.Prelude as Prelude

-- | Object specifying a channel.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | Channel ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Whether the channel is private (enabled for playback authorization).
    -- Default: @false@.
    authorized :: Prelude.Maybe Prelude.Bool,
    -- | Channel ingest endpoint, part of the definition of an ingest server,
    -- used when you set up streaming software.
    ingestEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Whether the channel allows insecure RTMP ingest. Default: @false@.
    insecureIngest :: Prelude.Maybe Prelude.Bool,
    -- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
    -- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
    -- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
    -- correspond to Ultra-low and Standard, respectively.)
    latencyMode :: Prelude.Maybe ChannelLatencyMode,
    -- | Channel name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Channel playback URL.
    playbackUrl :: Prelude.Maybe Prelude.Text,
    -- | Optional transcode preset for the channel. This is selectable only for
    -- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
    -- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
    -- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
    preset :: Prelude.Maybe TranscodePreset,
    -- | Recording-configuration ARN. A value other than an empty string
    -- indicates that recording is enabled. Default: \"\" (empty string,
    -- recording is disabled).
    recordingConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Tags attached to the resource. Array of 1-50 maps, each of the form
    -- @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- for more information, including restrictions that apply to tags and
    -- \"Tag naming limits and requirements\"; Amazon IVS has no
    -- service-specific constraints beyond what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Channel type, which determines the allowable resolution and bitrate. /If
    -- you exceed the allowable input resolution or bitrate, the stream
    -- probably will disconnect immediately./ Some types generate multiple
    -- qualities (renditions) from the original input; this automatically gives
    -- viewers the best experience for their devices and network conditions.
    -- Some types provide transcoded video; transcoding allows higher playback
    -- quality across a range of download speeds. Default: @STANDARD@. Valid
    -- values:
    --
    -- -   @BASIC@: Video is transmuxed: Amazon IVS delivers the original input
    --     quality to viewers. The viewer’s video-quality choice is limited to
    --     the original input. Input resolution can be up to 1080p and bitrate
    --     can be up to 1.5 Mbps for 480p and up to 3.5 Mbps for resolutions
    --     between 480p and 1080p. Original audio is passed through.
    --
    -- -   @STANDARD@: Video is transcoded: multiple qualities are generated
    --     from the original input, to automatically give viewers the best
    --     experience for their devices and network conditions. Transcoding
    --     allows higher playback quality across a range of download speeds.
    --     Resolution can be up to 1080p and bitrate can be up to 8.5 Mbps.
    --     Audio is transcoded only for renditions 360p and below; above that,
    --     audio is passed through. This is the default when you create a
    --     channel.
    --
    -- -   @ADVANCED_SD@: Video is transcoded; multiple qualities are generated
    --     from the original input, to automatically give viewers the best
    --     experience for their devices and network conditions. Input
    --     resolution can be up to 1080p and bitrate can be up to 8.5 Mbps;
    --     output is capped at SD quality (480p). You can select an optional
    --     transcode preset (see below). Audio for all renditions is
    --     transcoded, and an audio-only rendition is available.
    --
    -- -   @ADVANCED_HD@: Video is transcoded; multiple qualities are generated
    --     from the original input, to automatically give viewers the best
    --     experience for their devices and network conditions. Input
    --     resolution can be up to 1080p and bitrate can be up to 8.5 Mbps;
    --     output is capped at HD quality (720p). You can select an optional
    --     transcode preset (see below). Audio for all renditions is
    --     transcoded, and an audio-only rendition is available.
    --
    -- Optional /transcode presets/ (available for the @ADVANCED@ types) allow
    -- you to trade off available download bandwidth and video quality, to
    -- optimize the viewing experience. There are two presets:
    --
    -- -   /Constrained bandwidth delivery/ uses a lower bitrate for each
    --     quality level. Use it if you have low download bandwidth and\/or
    --     simple video content (e.g., talking heads)
    --
    -- -   /Higher bandwidth delivery/ uses a higher bitrate for each quality
    --     level. Use it if you have high download bandwidth and\/or complex
    --     video content (e.g., flashes and quick scene changes).
    type' :: Prelude.Maybe ChannelType
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
-- 'arn', 'channel_arn' - Channel ARN.
--
-- 'authorized', 'channel_authorized' - Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
--
-- 'ingestEndpoint', 'channel_ingestEndpoint' - Channel ingest endpoint, part of the definition of an ingest server,
-- used when you set up streaming software.
--
-- 'insecureIngest', 'channel_insecureIngest' - Whether the channel allows insecure RTMP ingest. Default: @false@.
--
-- 'latencyMode', 'channel_latencyMode' - Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
-- correspond to Ultra-low and Standard, respectively.)
--
-- 'name', 'channel_name' - Channel name.
--
-- 'playbackUrl', 'channel_playbackUrl' - Channel playback URL.
--
-- 'preset', 'channel_preset' - Optional transcode preset for the channel. This is selectable only for
-- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
-- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
-- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
--
-- 'recordingConfigurationArn', 'channel_recordingConfigurationArn' - Recording-configuration ARN. A value other than an empty string
-- indicates that recording is enabled. Default: \"\" (empty string,
-- recording is disabled).
--
-- 'tags', 'channel_tags' - Tags attached to the resource. Array of 1-50 maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
--
-- 'type'', 'channel_type' - Channel type, which determines the allowable resolution and bitrate. /If
-- you exceed the allowable input resolution or bitrate, the stream
-- probably will disconnect immediately./ Some types generate multiple
-- qualities (renditions) from the original input; this automatically gives
-- viewers the best experience for their devices and network conditions.
-- Some types provide transcoded video; transcoding allows higher playback
-- quality across a range of download speeds. Default: @STANDARD@. Valid
-- values:
--
-- -   @BASIC@: Video is transmuxed: Amazon IVS delivers the original input
--     quality to viewers. The viewer’s video-quality choice is limited to
--     the original input. Input resolution can be up to 1080p and bitrate
--     can be up to 1.5 Mbps for 480p and up to 3.5 Mbps for resolutions
--     between 480p and 1080p. Original audio is passed through.
--
-- -   @STANDARD@: Video is transcoded: multiple qualities are generated
--     from the original input, to automatically give viewers the best
--     experience for their devices and network conditions. Transcoding
--     allows higher playback quality across a range of download speeds.
--     Resolution can be up to 1080p and bitrate can be up to 8.5 Mbps.
--     Audio is transcoded only for renditions 360p and below; above that,
--     audio is passed through. This is the default when you create a
--     channel.
--
-- -   @ADVANCED_SD@: Video is transcoded; multiple qualities are generated
--     from the original input, to automatically give viewers the best
--     experience for their devices and network conditions. Input
--     resolution can be up to 1080p and bitrate can be up to 8.5 Mbps;
--     output is capped at SD quality (480p). You can select an optional
--     transcode preset (see below). Audio for all renditions is
--     transcoded, and an audio-only rendition is available.
--
-- -   @ADVANCED_HD@: Video is transcoded; multiple qualities are generated
--     from the original input, to automatically give viewers the best
--     experience for their devices and network conditions. Input
--     resolution can be up to 1080p and bitrate can be up to 8.5 Mbps;
--     output is capped at HD quality (720p). You can select an optional
--     transcode preset (see below). Audio for all renditions is
--     transcoded, and an audio-only rendition is available.
--
-- Optional /transcode presets/ (available for the @ADVANCED@ types) allow
-- you to trade off available download bandwidth and video quality, to
-- optimize the viewing experience. There are two presets:
--
-- -   /Constrained bandwidth delivery/ uses a lower bitrate for each
--     quality level. Use it if you have low download bandwidth and\/or
--     simple video content (e.g., talking heads)
--
-- -   /Higher bandwidth delivery/ uses a higher bitrate for each quality
--     level. Use it if you have high download bandwidth and\/or complex
--     video content (e.g., flashes and quick scene changes).
newChannel ::
  Channel
newChannel =
  Channel'
    { arn = Prelude.Nothing,
      authorized = Prelude.Nothing,
      ingestEndpoint = Prelude.Nothing,
      insecureIngest = Prelude.Nothing,
      latencyMode = Prelude.Nothing,
      name = Prelude.Nothing,
      playbackUrl = Prelude.Nothing,
      preset = Prelude.Nothing,
      recordingConfigurationArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Channel ARN.
channel_arn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
channel_authorized :: Lens.Lens' Channel (Prelude.Maybe Prelude.Bool)
channel_authorized = Lens.lens (\Channel' {authorized} -> authorized) (\s@Channel' {} a -> s {authorized = a} :: Channel)

-- | Channel ingest endpoint, part of the definition of an ingest server,
-- used when you set up streaming software.
channel_ingestEndpoint :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_ingestEndpoint = Lens.lens (\Channel' {ingestEndpoint} -> ingestEndpoint) (\s@Channel' {} a -> s {ingestEndpoint = a} :: Channel)

-- | Whether the channel allows insecure RTMP ingest. Default: @false@.
channel_insecureIngest :: Lens.Lens' Channel (Prelude.Maybe Prelude.Bool)
channel_insecureIngest = Lens.lens (\Channel' {insecureIngest} -> insecureIngest) (\s@Channel' {} a -> s {insecureIngest = a} :: Channel)

-- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
-- correspond to Ultra-low and Standard, respectively.)
channel_latencyMode :: Lens.Lens' Channel (Prelude.Maybe ChannelLatencyMode)
channel_latencyMode = Lens.lens (\Channel' {latencyMode} -> latencyMode) (\s@Channel' {} a -> s {latencyMode = a} :: Channel)

-- | Channel name.
channel_name :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_name = Lens.lens (\Channel' {name} -> name) (\s@Channel' {} a -> s {name = a} :: Channel)

-- | Channel playback URL.
channel_playbackUrl :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_playbackUrl = Lens.lens (\Channel' {playbackUrl} -> playbackUrl) (\s@Channel' {} a -> s {playbackUrl = a} :: Channel)

-- | Optional transcode preset for the channel. This is selectable only for
-- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
-- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
-- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
channel_preset :: Lens.Lens' Channel (Prelude.Maybe TranscodePreset)
channel_preset = Lens.lens (\Channel' {preset} -> preset) (\s@Channel' {} a -> s {preset = a} :: Channel)

-- | Recording-configuration ARN. A value other than an empty string
-- indicates that recording is enabled. Default: \"\" (empty string,
-- recording is disabled).
channel_recordingConfigurationArn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_recordingConfigurationArn = Lens.lens (\Channel' {recordingConfigurationArn} -> recordingConfigurationArn) (\s@Channel' {} a -> s {recordingConfigurationArn = a} :: Channel)

-- | Tags attached to the resource. Array of 1-50 maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
channel_tags :: Lens.Lens' Channel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channel_tags = Lens.lens (\Channel' {tags} -> tags) (\s@Channel' {} a -> s {tags = a} :: Channel) Prelude.. Lens.mapping Lens.coerced

-- | Channel type, which determines the allowable resolution and bitrate. /If
-- you exceed the allowable input resolution or bitrate, the stream
-- probably will disconnect immediately./ Some types generate multiple
-- qualities (renditions) from the original input; this automatically gives
-- viewers the best experience for their devices and network conditions.
-- Some types provide transcoded video; transcoding allows higher playback
-- quality across a range of download speeds. Default: @STANDARD@. Valid
-- values:
--
-- -   @BASIC@: Video is transmuxed: Amazon IVS delivers the original input
--     quality to viewers. The viewer’s video-quality choice is limited to
--     the original input. Input resolution can be up to 1080p and bitrate
--     can be up to 1.5 Mbps for 480p and up to 3.5 Mbps for resolutions
--     between 480p and 1080p. Original audio is passed through.
--
-- -   @STANDARD@: Video is transcoded: multiple qualities are generated
--     from the original input, to automatically give viewers the best
--     experience for their devices and network conditions. Transcoding
--     allows higher playback quality across a range of download speeds.
--     Resolution can be up to 1080p and bitrate can be up to 8.5 Mbps.
--     Audio is transcoded only for renditions 360p and below; above that,
--     audio is passed through. This is the default when you create a
--     channel.
--
-- -   @ADVANCED_SD@: Video is transcoded; multiple qualities are generated
--     from the original input, to automatically give viewers the best
--     experience for their devices and network conditions. Input
--     resolution can be up to 1080p and bitrate can be up to 8.5 Mbps;
--     output is capped at SD quality (480p). You can select an optional
--     transcode preset (see below). Audio for all renditions is
--     transcoded, and an audio-only rendition is available.
--
-- -   @ADVANCED_HD@: Video is transcoded; multiple qualities are generated
--     from the original input, to automatically give viewers the best
--     experience for their devices and network conditions. Input
--     resolution can be up to 1080p and bitrate can be up to 8.5 Mbps;
--     output is capped at HD quality (720p). You can select an optional
--     transcode preset (see below). Audio for all renditions is
--     transcoded, and an audio-only rendition is available.
--
-- Optional /transcode presets/ (available for the @ADVANCED@ types) allow
-- you to trade off available download bandwidth and video quality, to
-- optimize the viewing experience. There are two presets:
--
-- -   /Constrained bandwidth delivery/ uses a lower bitrate for each
--     quality level. Use it if you have low download bandwidth and\/or
--     simple video content (e.g., talking heads)
--
-- -   /Higher bandwidth delivery/ uses a higher bitrate for each quality
--     level. Use it if you have high download bandwidth and\/or complex
--     video content (e.g., flashes and quick scene changes).
channel_type :: Lens.Lens' Channel (Prelude.Maybe ChannelType)
channel_type = Lens.lens (\Channel' {type'} -> type') (\s@Channel' {} a -> s {type' = a} :: Channel)

instance Data.FromJSON Channel where
  parseJSON =
    Data.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "authorized")
            Prelude.<*> (x Data..:? "ingestEndpoint")
            Prelude.<*> (x Data..:? "insecureIngest")
            Prelude.<*> (x Data..:? "latencyMode")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "playbackUrl")
            Prelude.<*> (x Data..:? "preset")
            Prelude.<*> (x Data..:? "recordingConfigurationArn")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable Channel where
  hashWithSalt _salt Channel' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` authorized
      `Prelude.hashWithSalt` ingestEndpoint
      `Prelude.hashWithSalt` insecureIngest
      `Prelude.hashWithSalt` latencyMode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` playbackUrl
      `Prelude.hashWithSalt` preset
      `Prelude.hashWithSalt` recordingConfigurationArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Channel where
  rnf Channel' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authorized
      `Prelude.seq` Prelude.rnf ingestEndpoint
      `Prelude.seq` Prelude.rnf insecureIngest
      `Prelude.seq` Prelude.rnf latencyMode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf playbackUrl
      `Prelude.seq` Prelude.rnf preset
      `Prelude.seq` Prelude.rnf recordingConfigurationArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
