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
-- Module      : Amazonka.MediaTailor.CreateChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a channel.
module Amazonka.MediaTailor.CreateChannel
  ( -- * Creating a Request
    CreateChannel (..),
    newCreateChannel,

    -- * Request Lenses
    createChannel_fillerSlate,
    createChannel_tags,
    createChannel_channelName,
    createChannel_outputs,
    createChannel_playbackMode,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_creationTime,
    createChannelResponse_arn,
    createChannelResponse_lastModifiedTime,
    createChannelResponse_playbackMode,
    createChannelResponse_channelName,
    createChannelResponse_outputs,
    createChannelResponse_channelState,
    createChannelResponse_fillerSlate,
    createChannelResponse_tags,
    createChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { -- | The slate used to fill gaps between programs in the schedule. You must
    -- configure filler slate if your channel uses a LINEAR PlaybackMode.
    fillerSlate :: Prelude.Maybe SlateSource,
    -- | The tags to assign to the channel.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier for the channel you are working on.
    channelName :: Prelude.Text,
    -- | The channel\'s output properties.
    outputs :: [RequestOutputItem],
    -- | The type of playback mode to use for this channel.
    --
    -- LINEAR - The programs in the schedule play once back-to-back in the
    -- schedule.
    --
    -- LOOP - The programs in the schedule play back-to-back in an endless
    -- loop. When the last program in the schedule stops playing, playback
    -- loops back to the first program in the schedule.
    playbackMode :: PlaybackMode
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
-- 'fillerSlate', 'createChannel_fillerSlate' - The slate used to fill gaps between programs in the schedule. You must
-- configure filler slate if your channel uses a LINEAR PlaybackMode.
--
-- 'tags', 'createChannel_tags' - The tags to assign to the channel.
--
-- 'channelName', 'createChannel_channelName' - The identifier for the channel you are working on.
--
-- 'outputs', 'createChannel_outputs' - The channel\'s output properties.
--
-- 'playbackMode', 'createChannel_playbackMode' - The type of playback mode to use for this channel.
--
-- LINEAR - The programs in the schedule play once back-to-back in the
-- schedule.
--
-- LOOP - The programs in the schedule play back-to-back in an endless
-- loop. When the last program in the schedule stops playing, playback
-- loops back to the first program in the schedule.
newCreateChannel ::
  -- | 'channelName'
  Prelude.Text ->
  -- | 'playbackMode'
  PlaybackMode ->
  CreateChannel
newCreateChannel pChannelName_ pPlaybackMode_ =
  CreateChannel'
    { fillerSlate = Prelude.Nothing,
      tags = Prelude.Nothing,
      channelName = pChannelName_,
      outputs = Prelude.mempty,
      playbackMode = pPlaybackMode_
    }

-- | The slate used to fill gaps between programs in the schedule. You must
-- configure filler slate if your channel uses a LINEAR PlaybackMode.
createChannel_fillerSlate :: Lens.Lens' CreateChannel (Prelude.Maybe SlateSource)
createChannel_fillerSlate = Lens.lens (\CreateChannel' {fillerSlate} -> fillerSlate) (\s@CreateChannel' {} a -> s {fillerSlate = a} :: CreateChannel)

-- | The tags to assign to the channel.
createChannel_tags :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the channel you are working on.
createChannel_channelName :: Lens.Lens' CreateChannel Prelude.Text
createChannel_channelName = Lens.lens (\CreateChannel' {channelName} -> channelName) (\s@CreateChannel' {} a -> s {channelName = a} :: CreateChannel)

-- | The channel\'s output properties.
createChannel_outputs :: Lens.Lens' CreateChannel [RequestOutputItem]
createChannel_outputs = Lens.lens (\CreateChannel' {outputs} -> outputs) (\s@CreateChannel' {} a -> s {outputs = a} :: CreateChannel) Prelude.. Lens.coerced

-- | The type of playback mode to use for this channel.
--
-- LINEAR - The programs in the schedule play once back-to-back in the
-- schedule.
--
-- LOOP - The programs in the schedule play back-to-back in an endless
-- loop. When the last program in the schedule stops playing, playback
-- loops back to the first program in the schedule.
createChannel_playbackMode :: Lens.Lens' CreateChannel PlaybackMode
createChannel_playbackMode = Lens.lens (\CreateChannel' {playbackMode} -> playbackMode) (\s@CreateChannel' {} a -> s {playbackMode = a} :: CreateChannel)

instance Core.AWSRequest CreateChannel where
  type
    AWSResponse CreateChannel =
      CreateChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "PlaybackMode")
            Prelude.<*> (x Core..?> "ChannelName")
            Prelude.<*> (x Core..?> "Outputs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ChannelState")
            Prelude.<*> (x Core..?> "FillerSlate")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel where
  hashWithSalt _salt CreateChannel' {..} =
    _salt `Prelude.hashWithSalt` fillerSlate
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` playbackMode

instance Prelude.NFData CreateChannel where
  rnf CreateChannel' {..} =
    Prelude.rnf fillerSlate
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf playbackMode

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
          [ ("FillerSlate" Core..=) Prelude.<$> fillerSlate,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Outputs" Core..= outputs),
            Prelude.Just ("PlaybackMode" Core..= playbackMode)
          ]
      )

instance Core.ToPath CreateChannel where
  toPath CreateChannel' {..} =
    Prelude.mconcat
      ["/channel/", Core.toBS channelName]

instance Core.ToQuery CreateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { -- | The timestamp of when the channel was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the channel was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The channel\'s playback mode.
    playbackMode :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The channel\'s output properties.
    outputs :: Prelude.Maybe [ResponseOutputItem],
    -- | Indicates whether the channel is in a running state or not.
    channelState :: Prelude.Maybe ChannelState,
    -- | Contains information about the slate used to fill gaps between programs
    -- in the schedule.
    fillerSlate :: Prelude.Maybe SlateSource,
    -- | The tags assigned to the channel.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'createChannelResponse_creationTime' - The timestamp of when the channel was created.
--
-- 'arn', 'createChannelResponse_arn' - The ARN of the channel.
--
-- 'lastModifiedTime', 'createChannelResponse_lastModifiedTime' - The timestamp of when the channel was last modified.
--
-- 'playbackMode', 'createChannelResponse_playbackMode' - The channel\'s playback mode.
--
-- 'channelName', 'createChannelResponse_channelName' - The name of the channel.
--
-- 'outputs', 'createChannelResponse_outputs' - The channel\'s output properties.
--
-- 'channelState', 'createChannelResponse_channelState' - Indicates whether the channel is in a running state or not.
--
-- 'fillerSlate', 'createChannelResponse_fillerSlate' - Contains information about the slate used to fill gaps between programs
-- in the schedule.
--
-- 'tags', 'createChannelResponse_tags' - The tags assigned to the channel.
--
-- 'httpStatus', 'createChannelResponse_httpStatus' - The response's http status code.
newCreateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChannelResponse
newCreateChannelResponse pHttpStatus_ =
  CreateChannelResponse'
    { creationTime =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      playbackMode = Prelude.Nothing,
      channelName = Prelude.Nothing,
      outputs = Prelude.Nothing,
      channelState = Prelude.Nothing,
      fillerSlate = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp of when the channel was created.
createChannelResponse_creationTime :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.UTCTime)
createChannelResponse_creationTime = Lens.lens (\CreateChannelResponse' {creationTime} -> creationTime) (\s@CreateChannelResponse' {} a -> s {creationTime = a} :: CreateChannelResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN of the channel.
createChannelResponse_arn :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_arn = Lens.lens (\CreateChannelResponse' {arn} -> arn) (\s@CreateChannelResponse' {} a -> s {arn = a} :: CreateChannelResponse)

-- | The timestamp of when the channel was last modified.
createChannelResponse_lastModifiedTime :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.UTCTime)
createChannelResponse_lastModifiedTime = Lens.lens (\CreateChannelResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateChannelResponse' {} a -> s {lastModifiedTime = a} :: CreateChannelResponse) Prelude.. Lens.mapping Core._Time

-- | The channel\'s playback mode.
createChannelResponse_playbackMode :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_playbackMode = Lens.lens (\CreateChannelResponse' {playbackMode} -> playbackMode) (\s@CreateChannelResponse' {} a -> s {playbackMode = a} :: CreateChannelResponse)

-- | The name of the channel.
createChannelResponse_channelName :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_channelName = Lens.lens (\CreateChannelResponse' {channelName} -> channelName) (\s@CreateChannelResponse' {} a -> s {channelName = a} :: CreateChannelResponse)

-- | The channel\'s output properties.
createChannelResponse_outputs :: Lens.Lens' CreateChannelResponse (Prelude.Maybe [ResponseOutputItem])
createChannelResponse_outputs = Lens.lens (\CreateChannelResponse' {outputs} -> outputs) (\s@CreateChannelResponse' {} a -> s {outputs = a} :: CreateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the channel is in a running state or not.
createChannelResponse_channelState :: Lens.Lens' CreateChannelResponse (Prelude.Maybe ChannelState)
createChannelResponse_channelState = Lens.lens (\CreateChannelResponse' {channelState} -> channelState) (\s@CreateChannelResponse' {} a -> s {channelState = a} :: CreateChannelResponse)

-- | Contains information about the slate used to fill gaps between programs
-- in the schedule.
createChannelResponse_fillerSlate :: Lens.Lens' CreateChannelResponse (Prelude.Maybe SlateSource)
createChannelResponse_fillerSlate = Lens.lens (\CreateChannelResponse' {fillerSlate} -> fillerSlate) (\s@CreateChannelResponse' {} a -> s {fillerSlate = a} :: CreateChannelResponse)

-- | The tags assigned to the channel.
createChannelResponse_tags :: Lens.Lens' CreateChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannelResponse_tags = Lens.lens (\CreateChannelResponse' {tags} -> tags) (\s@CreateChannelResponse' {} a -> s {tags = a} :: CreateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Prelude.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Prelude.NFData CreateChannelResponse where
  rnf CreateChannelResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf playbackMode
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf channelState
      `Prelude.seq` Prelude.rnf fillerSlate
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
