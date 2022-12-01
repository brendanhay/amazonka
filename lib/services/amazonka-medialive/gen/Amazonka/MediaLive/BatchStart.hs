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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    batchStart'_multiplexIds,
    batchStart'_channelIds,

    -- * Destructuring the Response
    BatchStartResponse (..),
    newBatchStartResponse,

    -- * Response Lenses
    batchStartResponse_failed,
    batchStartResponse_successful,
    batchStartResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to start resources
--
-- /See:/ 'newBatchStart'' smart constructor.
data BatchStart' = BatchStart''
  { -- | List of multiplex IDs
    multiplexIds :: Prelude.Maybe [Prelude.Text],
    -- | List of channel IDs
    channelIds :: Prelude.Maybe [Prelude.Text]
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
-- 'multiplexIds', 'batchStart'_multiplexIds' - List of multiplex IDs
--
-- 'channelIds', 'batchStart'_channelIds' - List of channel IDs
newBatchStart' ::
  BatchStart'
newBatchStart' =
  BatchStart''
    { multiplexIds = Prelude.Nothing,
      channelIds = Prelude.Nothing
    }

-- | List of multiplex IDs
batchStart'_multiplexIds :: Lens.Lens' BatchStart' (Prelude.Maybe [Prelude.Text])
batchStart'_multiplexIds = Lens.lens (\BatchStart'' {multiplexIds} -> multiplexIds) (\s@BatchStart'' {} a -> s {multiplexIds = a} :: BatchStart') Prelude.. Lens.mapping Lens.coerced

-- | List of channel IDs
batchStart'_channelIds :: Lens.Lens' BatchStart' (Prelude.Maybe [Prelude.Text])
batchStart'_channelIds = Lens.lens (\BatchStart'' {channelIds} -> channelIds) (\s@BatchStart'' {} a -> s {channelIds = a} :: BatchStart') Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest BatchStart' where
  type AWSResponse BatchStart' = BatchStartResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchStartResponse'
            Prelude.<$> (x Core..?> "failed" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "successful" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchStart' where
  hashWithSalt _salt BatchStart'' {..} =
    _salt `Prelude.hashWithSalt` multiplexIds
      `Prelude.hashWithSalt` channelIds

instance Prelude.NFData BatchStart' where
  rnf BatchStart'' {..} =
    Prelude.rnf multiplexIds
      `Prelude.seq` Prelude.rnf channelIds

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
          [ ("multiplexIds" Core..=) Prelude.<$> multiplexIds,
            ("channelIds" Core..=) Prelude.<$> channelIds
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
  { -- | List of failed operations
    failed :: Prelude.Maybe [BatchFailedResultModel],
    -- | List of successful operations
    successful :: Prelude.Maybe [BatchSuccessfulResultModel],
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
-- 'failed', 'batchStartResponse_failed' - List of failed operations
--
-- 'successful', 'batchStartResponse_successful' - List of successful operations
--
-- 'httpStatus', 'batchStartResponse_httpStatus' - The response's http status code.
newBatchStartResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchStartResponse
newBatchStartResponse pHttpStatus_ =
  BatchStartResponse'
    { failed = Prelude.Nothing,
      successful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of failed operations
batchStartResponse_failed :: Lens.Lens' BatchStartResponse (Prelude.Maybe [BatchFailedResultModel])
batchStartResponse_failed = Lens.lens (\BatchStartResponse' {failed} -> failed) (\s@BatchStartResponse' {} a -> s {failed = a} :: BatchStartResponse) Prelude.. Lens.mapping Lens.coerced

-- | List of successful operations
batchStartResponse_successful :: Lens.Lens' BatchStartResponse (Prelude.Maybe [BatchSuccessfulResultModel])
batchStartResponse_successful = Lens.lens (\BatchStartResponse' {successful} -> successful) (\s@BatchStartResponse' {} a -> s {successful = a} :: BatchStartResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchStartResponse_httpStatus :: Lens.Lens' BatchStartResponse Prelude.Int
batchStartResponse_httpStatus = Lens.lens (\BatchStartResponse' {httpStatus} -> httpStatus) (\s@BatchStartResponse' {} a -> s {httpStatus = a} :: BatchStartResponse)

instance Prelude.NFData BatchStartResponse where
  rnf BatchStartResponse' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf successful
      `Prelude.seq` Prelude.rnf httpStatus
