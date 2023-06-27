{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVS.CreateChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new channel and an associated stream key to start streaming.
module Amazonka.IVS.CreateChannel
  ( -- * Creating a Request
    CreateChannel (..),
    newCreateChannel,

    -- * Request Lenses
    createChannel_authorized,
    createChannel_insecureIngest,
    createChannel_latencyMode,
    createChannel_name,
    createChannel_preset,
    createChannel_recordingConfigurationArn,
    createChannel_tags,
    createChannel_type,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_channel,
    createChannelResponse_streamKey,
    createChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { -- | Whether the channel is private (enabled for playback authorization).
    -- Default: @false@.
    authorized :: Prelude.Maybe Prelude.Bool,
    -- | Whether the channel allows insecure RTMP ingest. Default: @false@.
    insecureIngest :: Prelude.Maybe Prelude.Bool,
    -- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
    -- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
    -- (Note: In the Amazon IVS console, @LOW@ and @NORMAL@ correspond to
    -- Ultra-low and Standard, respectively.) Default: @LOW@.
    latencyMode :: Prelude.Maybe ChannelLatencyMode,
    -- | Channel name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Optional transcode preset for the channel. This is selectable only for
    -- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
    -- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
    -- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
    preset :: Prelude.Maybe TranscodePreset,
    -- | Recording-configuration ARN. Default: \"\" (empty string, recording is
    -- disabled).
    recordingConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
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
-- Create a value of 'CreateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorized', 'createChannel_authorized' - Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
--
-- 'insecureIngest', 'createChannel_insecureIngest' - Whether the channel allows insecure RTMP ingest. Default: @false@.
--
-- 'latencyMode', 'createChannel_latencyMode' - Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- (Note: In the Amazon IVS console, @LOW@ and @NORMAL@ correspond to
-- Ultra-low and Standard, respectively.) Default: @LOW@.
--
-- 'name', 'createChannel_name' - Channel name.
--
-- 'preset', 'createChannel_preset' - Optional transcode preset for the channel. This is selectable only for
-- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
-- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
-- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
--
-- 'recordingConfigurationArn', 'createChannel_recordingConfigurationArn' - Recording-configuration ARN. Default: \"\" (empty string, recording is
-- disabled).
--
-- 'tags', 'createChannel_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
--
-- 'type'', 'createChannel_type' - Channel type, which determines the allowable resolution and bitrate. /If
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
newCreateChannel ::
  CreateChannel
newCreateChannel =
  CreateChannel'
    { authorized = Prelude.Nothing,
      insecureIngest = Prelude.Nothing,
      latencyMode = Prelude.Nothing,
      name = Prelude.Nothing,
      preset = Prelude.Nothing,
      recordingConfigurationArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
createChannel_authorized :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Bool)
createChannel_authorized = Lens.lens (\CreateChannel' {authorized} -> authorized) (\s@CreateChannel' {} a -> s {authorized = a} :: CreateChannel)

-- | Whether the channel allows insecure RTMP ingest. Default: @false@.
createChannel_insecureIngest :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Bool)
createChannel_insecureIngest = Lens.lens (\CreateChannel' {insecureIngest} -> insecureIngest) (\s@CreateChannel' {} a -> s {insecureIngest = a} :: CreateChannel)

-- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- (Note: In the Amazon IVS console, @LOW@ and @NORMAL@ correspond to
-- Ultra-low and Standard, respectively.) Default: @LOW@.
createChannel_latencyMode :: Lens.Lens' CreateChannel (Prelude.Maybe ChannelLatencyMode)
createChannel_latencyMode = Lens.lens (\CreateChannel' {latencyMode} -> latencyMode) (\s@CreateChannel' {} a -> s {latencyMode = a} :: CreateChannel)

-- | Channel name.
createChannel_name :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Text)
createChannel_name = Lens.lens (\CreateChannel' {name} -> name) (\s@CreateChannel' {} a -> s {name = a} :: CreateChannel)

-- | Optional transcode preset for the channel. This is selectable only for
-- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
-- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
-- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
createChannel_preset :: Lens.Lens' CreateChannel (Prelude.Maybe TranscodePreset)
createChannel_preset = Lens.lens (\CreateChannel' {preset} -> preset) (\s@CreateChannel' {} a -> s {preset = a} :: CreateChannel)

-- | Recording-configuration ARN. Default: \"\" (empty string, recording is
-- disabled).
createChannel_recordingConfigurationArn :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Text)
createChannel_recordingConfigurationArn = Lens.lens (\CreateChannel' {recordingConfigurationArn} -> recordingConfigurationArn) (\s@CreateChannel' {} a -> s {recordingConfigurationArn = a} :: CreateChannel)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
createChannel_tags :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Prelude.. Lens.mapping Lens.coerced

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
createChannel_type :: Lens.Lens' CreateChannel (Prelude.Maybe ChannelType)
createChannel_type = Lens.lens (\CreateChannel' {type'} -> type') (\s@CreateChannel' {} a -> s {type' = a} :: CreateChannel)

instance Core.AWSRequest CreateChannel where
  type
    AWSResponse CreateChannel =
      CreateChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Prelude.<$> (x Data..?> "channel")
            Prelude.<*> (x Data..?> "streamKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel where
  hashWithSalt _salt CreateChannel' {..} =
    _salt
      `Prelude.hashWithSalt` authorized
      `Prelude.hashWithSalt` insecureIngest
      `Prelude.hashWithSalt` latencyMode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` preset
      `Prelude.hashWithSalt` recordingConfigurationArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateChannel where
  rnf CreateChannel' {..} =
    Prelude.rnf authorized
      `Prelude.seq` Prelude.rnf insecureIngest
      `Prelude.seq` Prelude.rnf latencyMode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf preset
      `Prelude.seq` Prelude.rnf recordingConfigurationArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorized" Data..=) Prelude.<$> authorized,
            ("insecureIngest" Data..=)
              Prelude.<$> insecureIngest,
            ("latencyMode" Data..=) Prelude.<$> latencyMode,
            ("name" Data..=) Prelude.<$> name,
            ("preset" Data..=) Prelude.<$> preset,
            ("recordingConfigurationArn" Data..=)
              Prelude.<$> recordingConfigurationArn,
            ("tags" Data..=) Prelude.<$> tags,
            ("type" Data..=) Prelude.<$> type'
          ]
      )

instance Data.ToPath CreateChannel where
  toPath = Prelude.const "/CreateChannel"

instance Data.ToQuery CreateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { channel :: Prelude.Maybe Channel,
    streamKey :: Prelude.Maybe StreamKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'createChannelResponse_channel' -
--
-- 'streamKey', 'createChannelResponse_streamKey' -
--
-- 'httpStatus', 'createChannelResponse_httpStatus' - The response's http status code.
newCreateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChannelResponse
newCreateChannelResponse pHttpStatus_ =
  CreateChannelResponse'
    { channel = Prelude.Nothing,
      streamKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

createChannelResponse_channel :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Channel)
createChannelResponse_channel = Lens.lens (\CreateChannelResponse' {channel} -> channel) (\s@CreateChannelResponse' {} a -> s {channel = a} :: CreateChannelResponse)

createChannelResponse_streamKey :: Lens.Lens' CreateChannelResponse (Prelude.Maybe StreamKey)
createChannelResponse_streamKey = Lens.lens (\CreateChannelResponse' {streamKey} -> streamKey) (\s@CreateChannelResponse' {} a -> s {streamKey = a} :: CreateChannelResponse)

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Prelude.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Prelude.NFData CreateChannelResponse where
  rnf CreateChannelResponse' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf streamKey
      `Prelude.seq` Prelude.rnf httpStatus
