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
-- Module      : Network.AWS.IoTAnalytics.StartPipelineReprocessing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the reprocessing of raw message data through the pipeline.
module Network.AWS.IoTAnalytics.StartPipelineReprocessing
  ( -- * Creating a Request
    StartPipelineReprocessing (..),
    newStartPipelineReprocessing,

    -- * Request Lenses
    startPipelineReprocessing_channelMessages,
    startPipelineReprocessing_startTime,
    startPipelineReprocessing_endTime,
    startPipelineReprocessing_pipelineName,

    -- * Destructuring the Response
    StartPipelineReprocessingResponse (..),
    newStartPipelineReprocessingResponse,

    -- * Response Lenses
    startPipelineReprocessingResponse_reprocessingId,
    startPipelineReprocessingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartPipelineReprocessing' smart constructor.
data StartPipelineReprocessing = StartPipelineReprocessing'
  { -- | Specifies one or more sets of channel messages that you want to
    -- reprocess.
    --
    -- If you use the @channelMessages@ object, you must not specify a value
    -- for @startTime@ and @endTime@.
    channelMessages :: Core.Maybe ChannelMessages,
    -- | The start time (inclusive) of raw message data that is reprocessed.
    --
    -- If you specify a value for the @startTime@ parameter, you must not use
    -- the @channelMessages@ object.
    startTime :: Core.Maybe Core.POSIX,
    -- | The end time (exclusive) of raw message data that is reprocessed.
    --
    -- If you specify a value for the @endTime@ parameter, you must not use the
    -- @channelMessages@ object.
    endTime :: Core.Maybe Core.POSIX,
    -- | The name of the pipeline on which to start reprocessing.
    pipelineName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartPipelineReprocessing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelMessages', 'startPipelineReprocessing_channelMessages' - Specifies one or more sets of channel messages that you want to
-- reprocess.
--
-- If you use the @channelMessages@ object, you must not specify a value
-- for @startTime@ and @endTime@.
--
-- 'startTime', 'startPipelineReprocessing_startTime' - The start time (inclusive) of raw message data that is reprocessed.
--
-- If you specify a value for the @startTime@ parameter, you must not use
-- the @channelMessages@ object.
--
-- 'endTime', 'startPipelineReprocessing_endTime' - The end time (exclusive) of raw message data that is reprocessed.
--
-- If you specify a value for the @endTime@ parameter, you must not use the
-- @channelMessages@ object.
--
-- 'pipelineName', 'startPipelineReprocessing_pipelineName' - The name of the pipeline on which to start reprocessing.
newStartPipelineReprocessing ::
  -- | 'pipelineName'
  Core.Text ->
  StartPipelineReprocessing
newStartPipelineReprocessing pPipelineName_ =
  StartPipelineReprocessing'
    { channelMessages =
        Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      pipelineName = pPipelineName_
    }

-- | Specifies one or more sets of channel messages that you want to
-- reprocess.
--
-- If you use the @channelMessages@ object, you must not specify a value
-- for @startTime@ and @endTime@.
startPipelineReprocessing_channelMessages :: Lens.Lens' StartPipelineReprocessing (Core.Maybe ChannelMessages)
startPipelineReprocessing_channelMessages = Lens.lens (\StartPipelineReprocessing' {channelMessages} -> channelMessages) (\s@StartPipelineReprocessing' {} a -> s {channelMessages = a} :: StartPipelineReprocessing)

-- | The start time (inclusive) of raw message data that is reprocessed.
--
-- If you specify a value for the @startTime@ parameter, you must not use
-- the @channelMessages@ object.
startPipelineReprocessing_startTime :: Lens.Lens' StartPipelineReprocessing (Core.Maybe Core.UTCTime)
startPipelineReprocessing_startTime = Lens.lens (\StartPipelineReprocessing' {startTime} -> startTime) (\s@StartPipelineReprocessing' {} a -> s {startTime = a} :: StartPipelineReprocessing) Core.. Lens.mapping Core._Time

-- | The end time (exclusive) of raw message data that is reprocessed.
--
-- If you specify a value for the @endTime@ parameter, you must not use the
-- @channelMessages@ object.
startPipelineReprocessing_endTime :: Lens.Lens' StartPipelineReprocessing (Core.Maybe Core.UTCTime)
startPipelineReprocessing_endTime = Lens.lens (\StartPipelineReprocessing' {endTime} -> endTime) (\s@StartPipelineReprocessing' {} a -> s {endTime = a} :: StartPipelineReprocessing) Core.. Lens.mapping Core._Time

-- | The name of the pipeline on which to start reprocessing.
startPipelineReprocessing_pipelineName :: Lens.Lens' StartPipelineReprocessing Core.Text
startPipelineReprocessing_pipelineName = Lens.lens (\StartPipelineReprocessing' {pipelineName} -> pipelineName) (\s@StartPipelineReprocessing' {} a -> s {pipelineName = a} :: StartPipelineReprocessing)

instance Core.AWSRequest StartPipelineReprocessing where
  type
    AWSResponse StartPipelineReprocessing =
      StartPipelineReprocessingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPipelineReprocessingResponse'
            Core.<$> (x Core..?> "reprocessingId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartPipelineReprocessing

instance Core.NFData StartPipelineReprocessing

instance Core.ToHeaders StartPipelineReprocessing where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON StartPipelineReprocessing where
  toJSON StartPipelineReprocessing' {..} =
    Core.object
      ( Core.catMaybes
          [ ("channelMessages" Core..=)
              Core.<$> channelMessages,
            ("startTime" Core..=) Core.<$> startTime,
            ("endTime" Core..=) Core.<$> endTime
          ]
      )

instance Core.ToPath StartPipelineReprocessing where
  toPath StartPipelineReprocessing' {..} =
    Core.mconcat
      [ "/pipelines/",
        Core.toBS pipelineName,
        "/reprocessing"
      ]

instance Core.ToQuery StartPipelineReprocessing where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartPipelineReprocessingResponse' smart constructor.
data StartPipelineReprocessingResponse = StartPipelineReprocessingResponse'
  { -- | The ID of the pipeline reprocessing activity that was started.
    reprocessingId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartPipelineReprocessingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reprocessingId', 'startPipelineReprocessingResponse_reprocessingId' - The ID of the pipeline reprocessing activity that was started.
--
-- 'httpStatus', 'startPipelineReprocessingResponse_httpStatus' - The response's http status code.
newStartPipelineReprocessingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartPipelineReprocessingResponse
newStartPipelineReprocessingResponse pHttpStatus_ =
  StartPipelineReprocessingResponse'
    { reprocessingId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the pipeline reprocessing activity that was started.
startPipelineReprocessingResponse_reprocessingId :: Lens.Lens' StartPipelineReprocessingResponse (Core.Maybe Core.Text)
startPipelineReprocessingResponse_reprocessingId = Lens.lens (\StartPipelineReprocessingResponse' {reprocessingId} -> reprocessingId) (\s@StartPipelineReprocessingResponse' {} a -> s {reprocessingId = a} :: StartPipelineReprocessingResponse)

-- | The response's http status code.
startPipelineReprocessingResponse_httpStatus :: Lens.Lens' StartPipelineReprocessingResponse Core.Int
startPipelineReprocessingResponse_httpStatus = Lens.lens (\StartPipelineReprocessingResponse' {httpStatus} -> httpStatus) (\s@StartPipelineReprocessingResponse' {} a -> s {httpStatus = a} :: StartPipelineReprocessingResponse)

instance
  Core.NFData
    StartPipelineReprocessingResponse
