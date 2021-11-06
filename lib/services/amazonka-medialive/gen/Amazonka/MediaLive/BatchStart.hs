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
-- Module      : Amazonka.MediaLive.BatchStart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts existing resources
module Amazonka.MediaLive.BatchStart
  ( -- * Creating a Request
    BatchStart' (..),
    newBatchStart',

    -- * Request Lenses
    batchStart'_channelIds,
    batchStart'_multiplexIds,

    -- * Destructuring the Response
    BatchStartResponse (..),
    newBatchStartResponse,

    -- * Response Lenses
    batchStartResponse_successful,
    batchStartResponse_failed,
    batchStartResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to start resources
--
-- /See:/ 'newBatchStart'' smart constructor.
data BatchStart' = BatchStart''
  { -- | List of channel IDs
    channelIds :: Prelude.Maybe [Prelude.Text],
    -- | List of multiplex IDs
    multiplexIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStart'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelIds', 'batchStart'_channelIds' - List of channel IDs
--
-- 'multiplexIds', 'batchStart'_multiplexIds' - List of multiplex IDs
newBatchStart' ::
  BatchStart'
newBatchStart' =
  BatchStart''
    { channelIds = Prelude.Nothing,
      multiplexIds = Prelude.Nothing
    }

-- | List of channel IDs
batchStart'_channelIds :: Lens.Lens' BatchStart' (Prelude.Maybe [Prelude.Text])
batchStart'_channelIds = Lens.lens (\BatchStart'' {channelIds} -> channelIds) (\s@BatchStart'' {} a -> s {channelIds = a} :: BatchStart') Prelude.. Lens.mapping Lens.coerced

-- | List of multiplex IDs
batchStart'_multiplexIds :: Lens.Lens' BatchStart' (Prelude.Maybe [Prelude.Text])
batchStart'_multiplexIds = Lens.lens (\BatchStart'' {multiplexIds} -> multiplexIds) (\s@BatchStart'' {} a -> s {multiplexIds = a} :: BatchStart') Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest BatchStart' where
  type AWSResponse BatchStart' = BatchStartResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchStartResponse'
            Prelude.<$> (x Core..?> "successful" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "failed" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchStart'

instance Prelude.NFData BatchStart'

instance Core.ToHeaders BatchStart' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchStart' where
  toJSON BatchStart'' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("channelIds" Core..=) Prelude.<$> channelIds,
            ("multiplexIds" Core..=) Prelude.<$> multiplexIds
          ]
      )

instance Core.ToPath BatchStart' where
  toPath = Prelude.const "/prod/batch/start"

instance Core.ToQuery BatchStart' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for BatchStartResponse
--
-- /See:/ 'newBatchStartResponse' smart constructor.
data BatchStartResponse = BatchStartResponse'
  { -- | List of successful operations
    successful :: Prelude.Maybe [BatchSuccessfulResultModel],
    -- | List of failed operations
    failed :: Prelude.Maybe [BatchFailedResultModel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStartResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successful', 'batchStartResponse_successful' - List of successful operations
--
-- 'failed', 'batchStartResponse_failed' - List of failed operations
--
-- 'httpStatus', 'batchStartResponse_httpStatus' - The response's http status code.
newBatchStartResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchStartResponse
newBatchStartResponse pHttpStatus_ =
  BatchStartResponse'
    { successful = Prelude.Nothing,
      failed = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of successful operations
batchStartResponse_successful :: Lens.Lens' BatchStartResponse (Prelude.Maybe [BatchSuccessfulResultModel])
batchStartResponse_successful = Lens.lens (\BatchStartResponse' {successful} -> successful) (\s@BatchStartResponse' {} a -> s {successful = a} :: BatchStartResponse) Prelude.. Lens.mapping Lens.coerced

-- | List of failed operations
batchStartResponse_failed :: Lens.Lens' BatchStartResponse (Prelude.Maybe [BatchFailedResultModel])
batchStartResponse_failed = Lens.lens (\BatchStartResponse' {failed} -> failed) (\s@BatchStartResponse' {} a -> s {failed = a} :: BatchStartResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchStartResponse_httpStatus :: Lens.Lens' BatchStartResponse Prelude.Int
batchStartResponse_httpStatus = Lens.lens (\BatchStartResponse' {httpStatus} -> httpStatus) (\s@BatchStartResponse' {} a -> s {httpStatus = a} :: BatchStartResponse)

instance Prelude.NFData BatchStartResponse
