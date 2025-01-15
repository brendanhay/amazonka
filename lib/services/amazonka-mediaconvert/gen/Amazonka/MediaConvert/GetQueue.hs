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
-- Module      : Amazonka.MediaConvert.GetQueue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific queue.
module Amazonka.MediaConvert.GetQueue
  ( -- * Creating a Request
    GetQueue (..),
    newGetQueue,

    -- * Request Lenses
    getQueue_name,

    -- * Destructuring the Response
    GetQueueResponse (..),
    newGetQueueResponse,

    -- * Response Lenses
    getQueueResponse_queue,
    getQueueResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQueue' smart constructor.
data GetQueue = GetQueue'
  { -- | The name of the queue that you want information about.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getQueue_name' - The name of the queue that you want information about.
newGetQueue ::
  -- | 'name'
  Prelude.Text ->
  GetQueue
newGetQueue pName_ = GetQueue' {name = pName_}

-- | The name of the queue that you want information about.
getQueue_name :: Lens.Lens' GetQueue Prelude.Text
getQueue_name = Lens.lens (\GetQueue' {name} -> name) (\s@GetQueue' {} a -> s {name = a} :: GetQueue)

instance Core.AWSRequest GetQueue where
  type AWSResponse GetQueue = GetQueueResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueueResponse'
            Prelude.<$> (x Data..?> "queue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQueue where
  hashWithSalt _salt GetQueue' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetQueue where
  rnf GetQueue' {..} = Prelude.rnf name

instance Data.ToHeaders GetQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetQueue where
  toPath GetQueue' {..} =
    Prelude.mconcat
      ["/2017-08-29/queues/", Data.toBS name]

instance Data.ToQuery GetQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQueueResponse' smart constructor.
data GetQueueResponse = GetQueueResponse'
  { -- | You can use queues to manage the resources that are available to your
    -- AWS account for running multiple transcoding jobs at the same time. If
    -- you don\'t specify a queue, the service sends all jobs through the
    -- default queue. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
    queue :: Prelude.Maybe Queue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queue', 'getQueueResponse_queue' - You can use queues to manage the resources that are available to your
-- AWS account for running multiple transcoding jobs at the same time. If
-- you don\'t specify a queue, the service sends all jobs through the
-- default queue. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
--
-- 'httpStatus', 'getQueueResponse_httpStatus' - The response's http status code.
newGetQueueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetQueueResponse
newGetQueueResponse pHttpStatus_ =
  GetQueueResponse'
    { queue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | You can use queues to manage the resources that are available to your
-- AWS account for running multiple transcoding jobs at the same time. If
-- you don\'t specify a queue, the service sends all jobs through the
-- default queue. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
getQueueResponse_queue :: Lens.Lens' GetQueueResponse (Prelude.Maybe Queue)
getQueueResponse_queue = Lens.lens (\GetQueueResponse' {queue} -> queue) (\s@GetQueueResponse' {} a -> s {queue = a} :: GetQueueResponse)

-- | The response's http status code.
getQueueResponse_httpStatus :: Lens.Lens' GetQueueResponse Prelude.Int
getQueueResponse_httpStatus = Lens.lens (\GetQueueResponse' {httpStatus} -> httpStatus) (\s@GetQueueResponse' {} a -> s {httpStatus = a} :: GetQueueResponse)

instance Prelude.NFData GetQueueResponse where
  rnf GetQueueResponse' {..} =
    Prelude.rnf queue `Prelude.seq`
      Prelude.rnf httpStatus
