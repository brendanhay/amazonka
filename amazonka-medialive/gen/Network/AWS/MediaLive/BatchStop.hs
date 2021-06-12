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
-- Module      : Network.AWS.MediaLive.BatchStop
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops running resources
module Network.AWS.MediaLive.BatchStop
  ( -- * Creating a Request
    BatchStop' (..),
    newBatchStop',

    -- * Request Lenses
    batchStop'_multiplexIds,
    batchStop'_channelIds,

    -- * Destructuring the Response
    BatchStopResponse (..),
    newBatchStopResponse,

    -- * Response Lenses
    batchStopResponse_successful,
    batchStopResponse_failed,
    batchStopResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to stop resources
--
-- /See:/ 'newBatchStop'' smart constructor.
data BatchStop' = BatchStop''
  { -- | List of multiplex IDs
    multiplexIds :: Core.Maybe [Core.Text],
    -- | List of channel IDs
    channelIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchStop'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexIds', 'batchStop'_multiplexIds' - List of multiplex IDs
--
-- 'channelIds', 'batchStop'_channelIds' - List of channel IDs
newBatchStop' ::
  BatchStop'
newBatchStop' =
  BatchStop''
    { multiplexIds = Core.Nothing,
      channelIds = Core.Nothing
    }

-- | List of multiplex IDs
batchStop'_multiplexIds :: Lens.Lens' BatchStop' (Core.Maybe [Core.Text])
batchStop'_multiplexIds = Lens.lens (\BatchStop'' {multiplexIds} -> multiplexIds) (\s@BatchStop'' {} a -> s {multiplexIds = a} :: BatchStop') Core.. Lens.mapping Lens._Coerce

-- | List of channel IDs
batchStop'_channelIds :: Lens.Lens' BatchStop' (Core.Maybe [Core.Text])
batchStop'_channelIds = Lens.lens (\BatchStop'' {channelIds} -> channelIds) (\s@BatchStop'' {} a -> s {channelIds = a} :: BatchStop') Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest BatchStop' where
  type AWSResponse BatchStop' = BatchStopResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchStopResponse'
            Core.<$> (x Core..?> "successful" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failed" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchStop'

instance Core.NFData BatchStop'

instance Core.ToHeaders BatchStop' where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchStop' where
  toJSON BatchStop'' {..} =
    Core.object
      ( Core.catMaybes
          [ ("multiplexIds" Core..=) Core.<$> multiplexIds,
            ("channelIds" Core..=) Core.<$> channelIds
          ]
      )

instance Core.ToPath BatchStop' where
  toPath = Core.const "/prod/batch/stop"

instance Core.ToQuery BatchStop' where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for BatchStopResponse
--
-- /See:/ 'newBatchStopResponse' smart constructor.
data BatchStopResponse = BatchStopResponse'
  { -- | List of successful operations
    successful :: Core.Maybe [BatchSuccessfulResultModel],
    -- | List of failed operations
    failed :: Core.Maybe [BatchFailedResultModel],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchStopResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successful', 'batchStopResponse_successful' - List of successful operations
--
-- 'failed', 'batchStopResponse_failed' - List of failed operations
--
-- 'httpStatus', 'batchStopResponse_httpStatus' - The response's http status code.
newBatchStopResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchStopResponse
newBatchStopResponse pHttpStatus_ =
  BatchStopResponse'
    { successful = Core.Nothing,
      failed = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of successful operations
batchStopResponse_successful :: Lens.Lens' BatchStopResponse (Core.Maybe [BatchSuccessfulResultModel])
batchStopResponse_successful = Lens.lens (\BatchStopResponse' {successful} -> successful) (\s@BatchStopResponse' {} a -> s {successful = a} :: BatchStopResponse) Core.. Lens.mapping Lens._Coerce

-- | List of failed operations
batchStopResponse_failed :: Lens.Lens' BatchStopResponse (Core.Maybe [BatchFailedResultModel])
batchStopResponse_failed = Lens.lens (\BatchStopResponse' {failed} -> failed) (\s@BatchStopResponse' {} a -> s {failed = a} :: BatchStopResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchStopResponse_httpStatus :: Lens.Lens' BatchStopResponse Core.Int
batchStopResponse_httpStatus = Lens.lens (\BatchStopResponse' {httpStatus} -> httpStatus) (\s@BatchStopResponse' {} a -> s {httpStatus = a} :: BatchStopResponse)

instance Core.NFData BatchStopResponse
