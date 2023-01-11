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
-- Module      : Amazonka.IVS.BatchGetChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs GetChannel on multiple ARNs simultaneously.
module Amazonka.IVS.BatchGetChannel
  ( -- * Creating a Request
    BatchGetChannel (..),
    newBatchGetChannel,

    -- * Request Lenses
    batchGetChannel_arns,

    -- * Destructuring the Response
    BatchGetChannelResponse (..),
    newBatchGetChannelResponse,

    -- * Response Lenses
    batchGetChannelResponse_channels,
    batchGetChannelResponse_errors,
    batchGetChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetChannel' smart constructor.
data BatchGetChannel = BatchGetChannel'
  { -- | Array of ARNs, one per channel.
    arns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arns', 'batchGetChannel_arns' - Array of ARNs, one per channel.
newBatchGetChannel ::
  -- | 'arns'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetChannel
newBatchGetChannel pArns_ =
  BatchGetChannel' {arns = Lens.coerced Lens.# pArns_}

-- | Array of ARNs, one per channel.
batchGetChannel_arns :: Lens.Lens' BatchGetChannel (Prelude.NonEmpty Prelude.Text)
batchGetChannel_arns = Lens.lens (\BatchGetChannel' {arns} -> arns) (\s@BatchGetChannel' {} a -> s {arns = a} :: BatchGetChannel) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetChannel where
  type
    AWSResponse BatchGetChannel =
      BatchGetChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetChannelResponse'
            Prelude.<$> (x Data..?> "channels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetChannel where
  hashWithSalt _salt BatchGetChannel' {..} =
    _salt `Prelude.hashWithSalt` arns

instance Prelude.NFData BatchGetChannel where
  rnf BatchGetChannel' {..} = Prelude.rnf arns

instance Data.ToHeaders BatchGetChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetChannel where
  toJSON BatchGetChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arns" Data..= arns)]
      )

instance Data.ToPath BatchGetChannel where
  toPath = Prelude.const "/BatchGetChannel"

instance Data.ToQuery BatchGetChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetChannelResponse' smart constructor.
data BatchGetChannelResponse = BatchGetChannelResponse'
  { channels :: Prelude.Maybe [Channel],
    -- | Each error object is related to a specific ARN in the request.
    errors :: Prelude.Maybe [BatchError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'batchGetChannelResponse_channels' -
--
-- 'errors', 'batchGetChannelResponse_errors' - Each error object is related to a specific ARN in the request.
--
-- 'httpStatus', 'batchGetChannelResponse_httpStatus' - The response's http status code.
newBatchGetChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetChannelResponse
newBatchGetChannelResponse pHttpStatus_ =
  BatchGetChannelResponse'
    { channels =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
batchGetChannelResponse_channels :: Lens.Lens' BatchGetChannelResponse (Prelude.Maybe [Channel])
batchGetChannelResponse_channels = Lens.lens (\BatchGetChannelResponse' {channels} -> channels) (\s@BatchGetChannelResponse' {} a -> s {channels = a} :: BatchGetChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Each error object is related to a specific ARN in the request.
batchGetChannelResponse_errors :: Lens.Lens' BatchGetChannelResponse (Prelude.Maybe [BatchError])
batchGetChannelResponse_errors = Lens.lens (\BatchGetChannelResponse' {errors} -> errors) (\s@BatchGetChannelResponse' {} a -> s {errors = a} :: BatchGetChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetChannelResponse_httpStatus :: Lens.Lens' BatchGetChannelResponse Prelude.Int
batchGetChannelResponse_httpStatus = Lens.lens (\BatchGetChannelResponse' {httpStatus} -> httpStatus) (\s@BatchGetChannelResponse' {} a -> s {httpStatus = a} :: BatchGetChannelResponse)

instance Prelude.NFData BatchGetChannelResponse where
  rnf BatchGetChannelResponse' {..} =
    Prelude.rnf channels
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
