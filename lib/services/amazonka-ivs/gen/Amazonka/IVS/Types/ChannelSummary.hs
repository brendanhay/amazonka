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
-- Module      : Amazonka.IVS.Types.ChannelSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.ChannelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.ChannelLatencyMode
import Amazonka.IVS.Types.ChannelType
import Amazonka.IVS.Types.TranscodePreset
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a channel.
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { -- | Channel ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Whether the channel is private (enabled for playback authorization).
    -- Default: @false@.
    authorized :: Prelude.Maybe Prelude.Bool,
    -- | Whether the channel allows insecure RTMP ingest. Default: @false@.
    insecureIngest :: Prelude.Maybe Prelude.Bool,
    -- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
    -- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
    -- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
    -- correspond to Ultra-low and Standard, respectively.)
    latencyMode :: Prelude.Maybe ChannelLatencyMode,
    -- | Channel name.
    name :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ChannelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'channelSummary_arn' - Channel ARN.
--
-- 'authorized', 'channelSummary_authorized' - Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
--
-- 'insecureIngest', 'channelSummary_insecureIngest' - Whether the channel allows insecure RTMP ingest. Default: @false@.
--
-- 'latencyMode', 'channelSummary_latencyMode' - Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
-- correspond to Ultra-low and Standard, respectively.)
--
-- 'name', 'channelSummary_name' - Channel name.
--
-- 'preset', 'channelSummary_preset' - Optional transcode preset for the channel. This is selectable only for
-- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
-- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
-- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
--
-- 'recordingConfigurationArn', 'channelSummary_recordingConfigurationArn' - Recording-configuration ARN. A value other than an empty string
-- indicates that recording is enabled. Default: \"\" (empty string,
-- recording is disabled).
--
-- 'tags', 'channelSummary_tags' - Tags attached to the resource. Array of 1-50 maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
--
-- 'type'', 'channelSummary_type' - Channel type, which determines the allowable resolution and bitrate. /If
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
newChannelSummary ::
  ChannelSummary
newChannelSummary =
  ChannelSummary'
    { arn = Prelude.Nothing,
      authorized = Prelude.Nothing,
      insecureIngest = Prelude.Nothing,
      latencyMode = Prelude.Nothing,
      name = Prelude.Nothing,
      preset = Prelude.Nothing,
      recordingConfigurationArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Channel ARN.
channelSummary_arn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_arn = Lens.lens (\ChannelSummary' {arn} -> arn) (\s@ChannelSummary' {} a -> s {arn = a} :: ChannelSummary)

-- | Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
channelSummary_authorized :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Bool)
channelSummary_authorized = Lens.lens (\ChannelSummary' {authorized} -> authorized) (\s@ChannelSummary' {} a -> s {authorized = a} :: ChannelSummary)

-- | Whether the channel allows insecure RTMP ingest. Default: @false@.
channelSummary_insecureIngest :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Bool)
channelSummary_insecureIngest = Lens.lens (\ChannelSummary' {insecureIngest} -> insecureIngest) (\s@ChannelSummary' {} a -> s {insecureIngest = a} :: ChannelSummary)

-- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
-- correspond to Ultra-low and Standard, respectively.)
channelSummary_latencyMode :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelLatencyMode)
channelSummary_latencyMode = Lens.lens (\ChannelSummary' {latencyMode} -> latencyMode) (\s@ChannelSummary' {} a -> s {latencyMode = a} :: ChannelSummary)

-- | Channel name.
channelSummary_name :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_name = Lens.lens (\ChannelSummary' {name} -> name) (\s@ChannelSummary' {} a -> s {name = a} :: ChannelSummary)

-- | Optional transcode preset for the channel. This is selectable only for
-- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
-- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
-- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
channelSummary_preset :: Lens.Lens' ChannelSummary (Prelude.Maybe TranscodePreset)
channelSummary_preset = Lens.lens (\ChannelSummary' {preset} -> preset) (\s@ChannelSummary' {} a -> s {preset = a} :: ChannelSummary)

-- | Recording-configuration ARN. A value other than an empty string
-- indicates that recording is enabled. Default: \"\" (empty string,
-- recording is disabled).
channelSummary_recordingConfigurationArn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_recordingConfigurationArn = Lens.lens (\ChannelSummary' {recordingConfigurationArn} -> recordingConfigurationArn) (\s@ChannelSummary' {} a -> s {recordingConfigurationArn = a} :: ChannelSummary)

-- | Tags attached to the resource. Array of 1-50 maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
channelSummary_tags :: Lens.Lens' ChannelSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channelSummary_tags = Lens.lens (\ChannelSummary' {tags} -> tags) (\s@ChannelSummary' {} a -> s {tags = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

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
channelSummary_type :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelType)
channelSummary_type = Lens.lens (\ChannelSummary' {type'} -> type') (\s@ChannelSummary' {} a -> s {type' = a} :: ChannelSummary)

instance Data.FromJSON ChannelSummary where
  parseJSON =
    Data.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "authorized")
            Prelude.<*> (x Data..:? "insecureIngest")
            Prelude.<*> (x Data..:? "latencyMode")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "preset")
            Prelude.<*> (x Data..:? "recordingConfigurationArn")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable ChannelSummary where
  hashWithSalt _salt ChannelSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` authorized
      `Prelude.hashWithSalt` insecureIngest
      `Prelude.hashWithSalt` latencyMode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` preset
      `Prelude.hashWithSalt` recordingConfigurationArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ChannelSummary where
  rnf ChannelSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authorized
      `Prelude.seq` Prelude.rnf insecureIngest
      `Prelude.seq` Prelude.rnf latencyMode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf preset
      `Prelude.seq` Prelude.rnf recordingConfigurationArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
