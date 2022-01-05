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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createChannel_latencyMode,
    createChannel_name,
    createChannel_recordingConfigurationArn,
    createChannel_type,
    createChannel_tags,

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
import Amazonka.IVS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { -- | Whether the channel is private (enabled for playback authorization).
    -- Default: @false@.
    authorized :: Prelude.Maybe Prelude.Bool,
    -- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
    -- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
    -- (Note: In the Amazon IVS console, @LOW@ and @NORMAL@ correspond to
    -- Ultra-low and Standard, respectively.) Default: @LOW@.
    latencyMode :: Prelude.Maybe ChannelLatencyMode,
    -- | Channel name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Recording-configuration ARN. Default: \"\" (empty string, recording is
    -- disabled).
    recordingConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Channel type, which determines the allowable resolution and bitrate. /If
    -- you exceed the allowable resolution or bitrate, the stream probably will
    -- disconnect immediately./ Default: @STANDARD@. Valid values:
    --
    -- -   @STANDARD@: Multiple qualities are generated from the original
    --     input, to automatically give viewers the best experience for their
    --     devices and network conditions. Resolution can be up to 1080p and
    --     bitrate can be up to 8.5 Mbps. Audio is transcoded only for
    --     renditions 360p and below; above that, audio is passed through.
    --
    -- -   @BASIC@: Amazon IVS delivers the original input to viewers. The
    --     viewer’s video-quality choice is limited to the original input.
    --     Resolution can be up to 480p and bitrate can be up to 1.5 Mbps.
    type' :: Prelude.Maybe ChannelType,
    -- | Array of 1-50 maps, each of the form @string:string (key:value)@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'latencyMode', 'createChannel_latencyMode' - Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- (Note: In the Amazon IVS console, @LOW@ and @NORMAL@ correspond to
-- Ultra-low and Standard, respectively.) Default: @LOW@.
--
-- 'name', 'createChannel_name' - Channel name.
--
-- 'recordingConfigurationArn', 'createChannel_recordingConfigurationArn' - Recording-configuration ARN. Default: \"\" (empty string, recording is
-- disabled).
--
-- 'type'', 'createChannel_type' - Channel type, which determines the allowable resolution and bitrate. /If
-- you exceed the allowable resolution or bitrate, the stream probably will
-- disconnect immediately./ Default: @STANDARD@. Valid values:
--
-- -   @STANDARD@: Multiple qualities are generated from the original
--     input, to automatically give viewers the best experience for their
--     devices and network conditions. Resolution can be up to 1080p and
--     bitrate can be up to 8.5 Mbps. Audio is transcoded only for
--     renditions 360p and below; above that, audio is passed through.
--
-- -   @BASIC@: Amazon IVS delivers the original input to viewers. The
--     viewer’s video-quality choice is limited to the original input.
--     Resolution can be up to 480p and bitrate can be up to 1.5 Mbps.
--
-- 'tags', 'createChannel_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@.
newCreateChannel ::
  CreateChannel
newCreateChannel =
  CreateChannel'
    { authorized = Prelude.Nothing,
      latencyMode = Prelude.Nothing,
      name = Prelude.Nothing,
      recordingConfigurationArn = Prelude.Nothing,
      type' = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
createChannel_authorized :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Bool)
createChannel_authorized = Lens.lens (\CreateChannel' {authorized} -> authorized) (\s@CreateChannel' {} a -> s {authorized = a} :: CreateChannel)

-- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- (Note: In the Amazon IVS console, @LOW@ and @NORMAL@ correspond to
-- Ultra-low and Standard, respectively.) Default: @LOW@.
createChannel_latencyMode :: Lens.Lens' CreateChannel (Prelude.Maybe ChannelLatencyMode)
createChannel_latencyMode = Lens.lens (\CreateChannel' {latencyMode} -> latencyMode) (\s@CreateChannel' {} a -> s {latencyMode = a} :: CreateChannel)

-- | Channel name.
createChannel_name :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Text)
createChannel_name = Lens.lens (\CreateChannel' {name} -> name) (\s@CreateChannel' {} a -> s {name = a} :: CreateChannel)

-- | Recording-configuration ARN. Default: \"\" (empty string, recording is
-- disabled).
createChannel_recordingConfigurationArn :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Text)
createChannel_recordingConfigurationArn = Lens.lens (\CreateChannel' {recordingConfigurationArn} -> recordingConfigurationArn) (\s@CreateChannel' {} a -> s {recordingConfigurationArn = a} :: CreateChannel)

-- | Channel type, which determines the allowable resolution and bitrate. /If
-- you exceed the allowable resolution or bitrate, the stream probably will
-- disconnect immediately./ Default: @STANDARD@. Valid values:
--
-- -   @STANDARD@: Multiple qualities are generated from the original
--     input, to automatically give viewers the best experience for their
--     devices and network conditions. Resolution can be up to 1080p and
--     bitrate can be up to 8.5 Mbps. Audio is transcoded only for
--     renditions 360p and below; above that, audio is passed through.
--
-- -   @BASIC@: Amazon IVS delivers the original input to viewers. The
--     viewer’s video-quality choice is limited to the original input.
--     Resolution can be up to 480p and bitrate can be up to 1.5 Mbps.
createChannel_type :: Lens.Lens' CreateChannel (Prelude.Maybe ChannelType)
createChannel_type = Lens.lens (\CreateChannel' {type'} -> type') (\s@CreateChannel' {} a -> s {type' = a} :: CreateChannel)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@.
createChannel_tags :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateChannel where
  type
    AWSResponse CreateChannel =
      CreateChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Prelude.<$> (x Core..?> "channel")
            Prelude.<*> (x Core..?> "streamKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel where
  hashWithSalt _salt CreateChannel' {..} =
    _salt `Prelude.hashWithSalt` authorized
      `Prelude.hashWithSalt` latencyMode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recordingConfigurationArn
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateChannel where
  rnf CreateChannel' {..} =
    Prelude.rnf authorized
      `Prelude.seq` Prelude.rnf latencyMode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recordingConfigurationArn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf tags

instance Core.ToHeaders CreateChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("authorized" Core..=) Prelude.<$> authorized,
            ("latencyMode" Core..=) Prelude.<$> latencyMode,
            ("name" Core..=) Prelude.<$> name,
            ("recordingConfigurationArn" Core..=)
              Prelude.<$> recordingConfigurationArn,
            ("type" Core..=) Prelude.<$> type',
            ("tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateChannel where
  toPath = Prelude.const "/CreateChannel"

instance Core.ToQuery CreateChannel where
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

-- |
createChannelResponse_channel :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Channel)
createChannelResponse_channel = Lens.lens (\CreateChannelResponse' {channel} -> channel) (\s@CreateChannelResponse' {} a -> s {channel = a} :: CreateChannelResponse)

-- |
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
