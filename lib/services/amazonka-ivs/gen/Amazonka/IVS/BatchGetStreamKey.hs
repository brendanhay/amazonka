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
-- Module      : Amazonka.IVS.BatchGetStreamKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs GetStreamKey on multiple ARNs simultaneously.
module Amazonka.IVS.BatchGetStreamKey
  ( -- * Creating a Request
    BatchGetStreamKey (..),
    newBatchGetStreamKey,

    -- * Request Lenses
    batchGetStreamKey_arns,

    -- * Destructuring the Response
    BatchGetStreamKeyResponse (..),
    newBatchGetStreamKeyResponse,

    -- * Response Lenses
    batchGetStreamKeyResponse_errors,
    batchGetStreamKeyResponse_streamKeys,
    batchGetStreamKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetStreamKey' smart constructor.
data BatchGetStreamKey = BatchGetStreamKey'
  { -- | Array of ARNs, one per channel.
    arns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetStreamKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arns', 'batchGetStreamKey_arns' - Array of ARNs, one per channel.
newBatchGetStreamKey ::
  -- | 'arns'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetStreamKey
newBatchGetStreamKey pArns_ =
  BatchGetStreamKey'
    { arns =
        Lens.coerced Lens.# pArns_
    }

-- | Array of ARNs, one per channel.
batchGetStreamKey_arns :: Lens.Lens' BatchGetStreamKey (Prelude.NonEmpty Prelude.Text)
batchGetStreamKey_arns = Lens.lens (\BatchGetStreamKey' {arns} -> arns) (\s@BatchGetStreamKey' {} a -> s {arns = a} :: BatchGetStreamKey) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetStreamKey where
  type
    AWSResponse BatchGetStreamKey =
      BatchGetStreamKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetStreamKeyResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "streamKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetStreamKey where
  hashWithSalt _salt BatchGetStreamKey' {..} =
    _salt `Prelude.hashWithSalt` arns

instance Prelude.NFData BatchGetStreamKey where
  rnf BatchGetStreamKey' {..} = Prelude.rnf arns

instance Data.ToHeaders BatchGetStreamKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetStreamKey where
  toJSON BatchGetStreamKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arns" Data..= arns)]
      )

instance Data.ToPath BatchGetStreamKey where
  toPath = Prelude.const "/BatchGetStreamKey"

instance Data.ToQuery BatchGetStreamKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetStreamKeyResponse' smart constructor.
data BatchGetStreamKeyResponse = BatchGetStreamKeyResponse'
  { errors :: Prelude.Maybe [BatchError],
    streamKeys :: Prelude.Maybe [StreamKey],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetStreamKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchGetStreamKeyResponse_errors' -
--
-- 'streamKeys', 'batchGetStreamKeyResponse_streamKeys' -
--
-- 'httpStatus', 'batchGetStreamKeyResponse_httpStatus' - The response's http status code.
newBatchGetStreamKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetStreamKeyResponse
newBatchGetStreamKeyResponse pHttpStatus_ =
  BatchGetStreamKeyResponse'
    { errors =
        Prelude.Nothing,
      streamKeys = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

batchGetStreamKeyResponse_errors :: Lens.Lens' BatchGetStreamKeyResponse (Prelude.Maybe [BatchError])
batchGetStreamKeyResponse_errors = Lens.lens (\BatchGetStreamKeyResponse' {errors} -> errors) (\s@BatchGetStreamKeyResponse' {} a -> s {errors = a} :: BatchGetStreamKeyResponse) Prelude.. Lens.mapping Lens.coerced

batchGetStreamKeyResponse_streamKeys :: Lens.Lens' BatchGetStreamKeyResponse (Prelude.Maybe [StreamKey])
batchGetStreamKeyResponse_streamKeys = Lens.lens (\BatchGetStreamKeyResponse' {streamKeys} -> streamKeys) (\s@BatchGetStreamKeyResponse' {} a -> s {streamKeys = a} :: BatchGetStreamKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetStreamKeyResponse_httpStatus :: Lens.Lens' BatchGetStreamKeyResponse Prelude.Int
batchGetStreamKeyResponse_httpStatus = Lens.lens (\BatchGetStreamKeyResponse' {httpStatus} -> httpStatus) (\s@BatchGetStreamKeyResponse' {} a -> s {httpStatus = a} :: BatchGetStreamKeyResponse)

instance Prelude.NFData BatchGetStreamKeyResponse where
  rnf BatchGetStreamKeyResponse' {..} =
    Prelude.rnf errors `Prelude.seq`
      Prelude.rnf streamKeys `Prelude.seq`
        Prelude.rnf httpStatus
