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
-- Module      : Amazonka.IVS.UpdateChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a channel\'s configuration. Live channels cannot be updated. You
-- must stop the ongoing stream, update the channel, and restart the stream
-- for the changes to take effect.
module Amazonka.IVS.UpdateChannel
  ( -- * Creating a Request
    UpdateChannel (..),
    newUpdateChannel,

    -- * Request Lenses
    updateChannel_authorized,
    updateChannel_insecureIngest,
    updateChannel_latencyMode,
    updateChannel_name,
    updateChannel_preset,
    updateChannel_recordingConfigurationArn,
    updateChannel_type,
    updateChannel_arn,

    -- * Destructuring the Response
    UpdateChannelResponse (..),
    newUpdateChannelResponse,

    -- * Response Lenses
    updateChannelResponse_channel,
    updateChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | Whether the channel is private (enabled for playback authorization).
    authorized :: Prelude.Maybe Prelude.Bool,
    -- | Whether the channel allows insecure RTMP ingest. Default: @false@.
    insecureIngest :: Prelude.Maybe Prelude.Bool,
    -- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
    -- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
    -- (Note: In the Amazon IVS console, @LOW@ and @NORMAL@ correspond to
    -- Ultra-low and Standard, respectively.)
    latencyMode :: Prelude.Maybe ChannelLatencyMode,
    -- | Channel name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Optional transcode preset for the channel. This is selectable only for
    -- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
    -- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
    -- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
    preset :: Prelude.Maybe TranscodePreset,
    -- | Recording-configuration ARN. If this is set to an empty string,
    -- recording is disabled. A value other than an empty string indicates that
    -- recording is enabled
    recordingConfigurationArn :: Prelude.Maybe Prelude.Text,
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
    type' :: Prelude.Maybe ChannelType,
    -- | ARN of the channel to be updated.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorized', 'updateChannel_authorized' - Whether the channel is private (enabled for playback authorization).
--
-- 'insecureIngest', 'updateChannel_insecureIngest' - Whether the channel allows insecure RTMP ingest. Default: @false@.
--
-- 'latencyMode', 'updateChannel_latencyMode' - Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- (Note: In the Amazon IVS console, @LOW@ and @NORMAL@ correspond to
-- Ultra-low and Standard, respectively.)
--
-- 'name', 'updateChannel_name' - Channel name.
--
-- 'preset', 'updateChannel_preset' - Optional transcode preset for the channel. This is selectable only for
-- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
-- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
-- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
--
-- 'recordingConfigurationArn', 'updateChannel_recordingConfigurationArn' - Recording-configuration ARN. If this is set to an empty string,
-- recording is disabled. A value other than an empty string indicates that
-- recording is enabled
--
-- 'type'', 'updateChannel_type' - Channel type, which determines the allowable resolution and bitrate. /If
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
--
-- 'arn', 'updateChannel_arn' - ARN of the channel to be updated.
newUpdateChannel ::
  -- | 'arn'
  Prelude.Text ->
  UpdateChannel
newUpdateChannel pArn_ =
  UpdateChannel'
    { authorized = Prelude.Nothing,
      insecureIngest = Prelude.Nothing,
      latencyMode = Prelude.Nothing,
      name = Prelude.Nothing,
      preset = Prelude.Nothing,
      recordingConfigurationArn = Prelude.Nothing,
      type' = Prelude.Nothing,
      arn = pArn_
    }

-- | Whether the channel is private (enabled for playback authorization).
updateChannel_authorized :: Lens.Lens' UpdateChannel (Prelude.Maybe Prelude.Bool)
updateChannel_authorized = Lens.lens (\UpdateChannel' {authorized} -> authorized) (\s@UpdateChannel' {} a -> s {authorized = a} :: UpdateChannel)

-- | Whether the channel allows insecure RTMP ingest. Default: @false@.
updateChannel_insecureIngest :: Lens.Lens' UpdateChannel (Prelude.Maybe Prelude.Bool)
updateChannel_insecureIngest = Lens.lens (\UpdateChannel' {insecureIngest} -> insecureIngest) (\s@UpdateChannel' {} a -> s {insecureIngest = a} :: UpdateChannel)

-- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- (Note: In the Amazon IVS console, @LOW@ and @NORMAL@ correspond to
-- Ultra-low and Standard, respectively.)
updateChannel_latencyMode :: Lens.Lens' UpdateChannel (Prelude.Maybe ChannelLatencyMode)
updateChannel_latencyMode = Lens.lens (\UpdateChannel' {latencyMode} -> latencyMode) (\s@UpdateChannel' {} a -> s {latencyMode = a} :: UpdateChannel)

-- | Channel name.
updateChannel_name :: Lens.Lens' UpdateChannel (Prelude.Maybe Prelude.Text)
updateChannel_name = Lens.lens (\UpdateChannel' {name} -> name) (\s@UpdateChannel' {} a -> s {name = a} :: UpdateChannel)

-- | Optional transcode preset for the channel. This is selectable only for
-- @ADVANCED_HD@ and @ADVANCED_SD@ channel types. For those channel types,
-- the default @preset@ is @HIGHER_BANDWIDTH_DELIVERY@. For other channel
-- types (@BASIC@ and @STANDARD@), @preset@ is the empty string (@\"\"@).
updateChannel_preset :: Lens.Lens' UpdateChannel (Prelude.Maybe TranscodePreset)
updateChannel_preset = Lens.lens (\UpdateChannel' {preset} -> preset) (\s@UpdateChannel' {} a -> s {preset = a} :: UpdateChannel)

-- | Recording-configuration ARN. If this is set to an empty string,
-- recording is disabled. A value other than an empty string indicates that
-- recording is enabled
updateChannel_recordingConfigurationArn :: Lens.Lens' UpdateChannel (Prelude.Maybe Prelude.Text)
updateChannel_recordingConfigurationArn = Lens.lens (\UpdateChannel' {recordingConfigurationArn} -> recordingConfigurationArn) (\s@UpdateChannel' {} a -> s {recordingConfigurationArn = a} :: UpdateChannel)

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
updateChannel_type :: Lens.Lens' UpdateChannel (Prelude.Maybe ChannelType)
updateChannel_type = Lens.lens (\UpdateChannel' {type'} -> type') (\s@UpdateChannel' {} a -> s {type' = a} :: UpdateChannel)

-- | ARN of the channel to be updated.
updateChannel_arn :: Lens.Lens' UpdateChannel Prelude.Text
updateChannel_arn = Lens.lens (\UpdateChannel' {arn} -> arn) (\s@UpdateChannel' {} a -> s {arn = a} :: UpdateChannel)

instance Core.AWSRequest UpdateChannel where
  type
    AWSResponse UpdateChannel =
      UpdateChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Prelude.<$> (x Data..?> "channel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChannel where
  hashWithSalt _salt UpdateChannel' {..} =
    _salt
      `Prelude.hashWithSalt` authorized
      `Prelude.hashWithSalt` insecureIngest
      `Prelude.hashWithSalt` latencyMode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` preset
      `Prelude.hashWithSalt` recordingConfigurationArn
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateChannel where
  rnf UpdateChannel' {..} =
    Prelude.rnf authorized
      `Prelude.seq` Prelude.rnf insecureIngest
      `Prelude.seq` Prelude.rnf latencyMode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf preset
      `Prelude.seq` Prelude.rnf recordingConfigurationArn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdateChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
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
            ("type" Data..=) Prelude.<$> type',
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateChannel where
  toPath = Prelude.const "/UpdateChannel"

instance Data.ToQuery UpdateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { channel :: Prelude.Maybe Channel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'updateChannelResponse_channel' - Undocumented member.
--
-- 'httpStatus', 'updateChannelResponse_httpStatus' - The response's http status code.
newUpdateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChannelResponse
newUpdateChannelResponse pHttpStatus_ =
  UpdateChannelResponse'
    { channel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateChannelResponse_channel :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Channel)
updateChannelResponse_channel = Lens.lens (\UpdateChannelResponse' {channel} -> channel) (\s@UpdateChannelResponse' {} a -> s {channel = a} :: UpdateChannelResponse)

-- | The response's http status code.
updateChannelResponse_httpStatus :: Lens.Lens' UpdateChannelResponse Prelude.Int
updateChannelResponse_httpStatus = Lens.lens (\UpdateChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelResponse' {} a -> s {httpStatus = a} :: UpdateChannelResponse)

instance Prelude.NFData UpdateChannelResponse where
  rnf UpdateChannelResponse' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf httpStatus
