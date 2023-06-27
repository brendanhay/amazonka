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
-- Module      : Amazonka.MediaTailor.DescribeChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a channel. For information about MediaTailor channels, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-channels.html Working with channels>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.DescribeChannel
  ( -- * Creating a Request
    DescribeChannel (..),
    newDescribeChannel,

    -- * Request Lenses
    describeChannel_channelName,

    -- * Destructuring the Response
    DescribeChannelResponse (..),
    newDescribeChannelResponse,

    -- * Response Lenses
    describeChannelResponse_arn,
    describeChannelResponse_channelName,
    describeChannelResponse_channelState,
    describeChannelResponse_creationTime,
    describeChannelResponse_fillerSlate,
    describeChannelResponse_lastModifiedTime,
    describeChannelResponse_outputs,
    describeChannelResponse_playbackMode,
    describeChannelResponse_tags,
    describeChannelResponse_tier,
    describeChannelResponse_httpStatus,
    describeChannelResponse_logConfiguration,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | The name of the channel.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'describeChannel_channelName' - The name of the channel.
newDescribeChannel ::
  -- | 'channelName'
  Prelude.Text ->
  DescribeChannel
newDescribeChannel pChannelName_ =
  DescribeChannel' {channelName = pChannelName_}

-- | The name of the channel.
describeChannel_channelName :: Lens.Lens' DescribeChannel Prelude.Text
describeChannel_channelName = Lens.lens (\DescribeChannel' {channelName} -> channelName) (\s@DescribeChannel' {} a -> s {channelName = a} :: DescribeChannel)

instance Core.AWSRequest DescribeChannel where
  type
    AWSResponse DescribeChannel =
      DescribeChannelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "ChannelName")
            Prelude.<*> (x Data..?> "ChannelState")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "FillerSlate")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Outputs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "PlaybackMode")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Tier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "LogConfiguration")
      )

instance Prelude.Hashable DescribeChannel where
  hashWithSalt _salt DescribeChannel' {..} =
    _salt `Prelude.hashWithSalt` channelName

instance Prelude.NFData DescribeChannel where
  rnf DescribeChannel' {..} = Prelude.rnf channelName

instance Data.ToHeaders DescribeChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Prelude.mconcat
      ["/channel/", Data.toBS channelName]

instance Data.ToQuery DescribeChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { -- | The ARN of the channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the channel is in a running state or not.
    channelState :: Prelude.Maybe ChannelState,
    -- | The timestamp of when the channel was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Contains information about the slate used to fill gaps between programs
    -- in the schedule.
    fillerSlate :: Prelude.Maybe SlateSource,
    -- | The timestamp of when the channel was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The channel\'s output properties.
    outputs :: Prelude.Maybe [ResponseOutputItem],
    -- | The channel\'s playback mode.
    playbackMode :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the channel. Tags are key-value pairs that you can
    -- associate with Amazon resources to help with organization, access
    -- control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The channel\'s tier.
    tier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The log configuration for the channel.
    logConfiguration :: LogConfigurationForChannel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeChannelResponse_arn' - The ARN of the channel.
--
-- 'channelName', 'describeChannelResponse_channelName' - The name of the channel.
--
-- 'channelState', 'describeChannelResponse_channelState' - Indicates whether the channel is in a running state or not.
--
-- 'creationTime', 'describeChannelResponse_creationTime' - The timestamp of when the channel was created.
--
-- 'fillerSlate', 'describeChannelResponse_fillerSlate' - Contains information about the slate used to fill gaps between programs
-- in the schedule.
--
-- 'lastModifiedTime', 'describeChannelResponse_lastModifiedTime' - The timestamp of when the channel was last modified.
--
-- 'outputs', 'describeChannelResponse_outputs' - The channel\'s output properties.
--
-- 'playbackMode', 'describeChannelResponse_playbackMode' - The channel\'s playback mode.
--
-- 'tags', 'describeChannelResponse_tags' - The tags assigned to the channel. Tags are key-value pairs that you can
-- associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'tier', 'describeChannelResponse_tier' - The channel\'s tier.
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
--
-- 'logConfiguration', 'describeChannelResponse_logConfiguration' - The log configuration for the channel.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'logConfiguration'
  LogConfigurationForChannel ->
  DescribeChannelResponse
newDescribeChannelResponse
  pHttpStatus_
  pLogConfiguration_ =
    DescribeChannelResponse'
      { arn = Prelude.Nothing,
        channelName = Prelude.Nothing,
        channelState = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        fillerSlate = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        outputs = Prelude.Nothing,
        playbackMode = Prelude.Nothing,
        tags = Prelude.Nothing,
        tier = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        logConfiguration = pLogConfiguration_
      }

-- | The ARN of the channel.
describeChannelResponse_arn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_arn = Lens.lens (\DescribeChannelResponse' {arn} -> arn) (\s@DescribeChannelResponse' {} a -> s {arn = a} :: DescribeChannelResponse)

-- | The name of the channel.
describeChannelResponse_channelName :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_channelName = Lens.lens (\DescribeChannelResponse' {channelName} -> channelName) (\s@DescribeChannelResponse' {} a -> s {channelName = a} :: DescribeChannelResponse)

-- | Indicates whether the channel is in a running state or not.
describeChannelResponse_channelState :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelState)
describeChannelResponse_channelState = Lens.lens (\DescribeChannelResponse' {channelState} -> channelState) (\s@DescribeChannelResponse' {} a -> s {channelState = a} :: DescribeChannelResponse)

-- | The timestamp of when the channel was created.
describeChannelResponse_creationTime :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.UTCTime)
describeChannelResponse_creationTime = Lens.lens (\DescribeChannelResponse' {creationTime} -> creationTime) (\s@DescribeChannelResponse' {} a -> s {creationTime = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Data._Time

-- | Contains information about the slate used to fill gaps between programs
-- in the schedule.
describeChannelResponse_fillerSlate :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe SlateSource)
describeChannelResponse_fillerSlate = Lens.lens (\DescribeChannelResponse' {fillerSlate} -> fillerSlate) (\s@DescribeChannelResponse' {} a -> s {fillerSlate = a} :: DescribeChannelResponse)

-- | The timestamp of when the channel was last modified.
describeChannelResponse_lastModifiedTime :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.UTCTime)
describeChannelResponse_lastModifiedTime = Lens.lens (\DescribeChannelResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeChannelResponse' {} a -> s {lastModifiedTime = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Data._Time

-- | The channel\'s output properties.
describeChannelResponse_outputs :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [ResponseOutputItem])
describeChannelResponse_outputs = Lens.lens (\DescribeChannelResponse' {outputs} -> outputs) (\s@DescribeChannelResponse' {} a -> s {outputs = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The channel\'s playback mode.
describeChannelResponse_playbackMode :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_playbackMode = Lens.lens (\DescribeChannelResponse' {playbackMode} -> playbackMode) (\s@DescribeChannelResponse' {} a -> s {playbackMode = a} :: DescribeChannelResponse)

-- | The tags assigned to the channel. Tags are key-value pairs that you can
-- associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
describeChannelResponse_tags :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeChannelResponse_tags = Lens.lens (\DescribeChannelResponse' {tags} -> tags) (\s@DescribeChannelResponse' {} a -> s {tags = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The channel\'s tier.
describeChannelResponse_tier :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_tier = Lens.lens (\DescribeChannelResponse' {tier} -> tier) (\s@DescribeChannelResponse' {} a -> s {tier = a} :: DescribeChannelResponse)

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Prelude.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

-- | The log configuration for the channel.
describeChannelResponse_logConfiguration :: Lens.Lens' DescribeChannelResponse LogConfigurationForChannel
describeChannelResponse_logConfiguration = Lens.lens (\DescribeChannelResponse' {logConfiguration} -> logConfiguration) (\s@DescribeChannelResponse' {} a -> s {logConfiguration = a} :: DescribeChannelResponse)

instance Prelude.NFData DescribeChannelResponse where
  rnf DescribeChannelResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelState
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf fillerSlate
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf playbackMode
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tier
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf logConfiguration
