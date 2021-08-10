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
-- Module      : Network.AWS.Connect.DescribeQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Describes the specified queue.
module Network.AWS.Connect.DescribeQueue
  ( -- * Creating a Request
    DescribeQueue (..),
    newDescribeQueue,

    -- * Request Lenses
    describeQueue_instanceId,
    describeQueue_queueId,

    -- * Destructuring the Response
    DescribeQueueResponse (..),
    newDescribeQueueResponse,

    -- * Response Lenses
    describeQueueResponse_queue,
    describeQueueResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeQueue' smart constructor.
data DescribeQueue = DescribeQueue'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeQueue_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'queueId', 'describeQueue_queueId' - The identifier for the queue.
newDescribeQueue ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
  DescribeQueue
newDescribeQueue pInstanceId_ pQueueId_ =
  DescribeQueue'
    { instanceId = pInstanceId_,
      queueId = pQueueId_
    }

-- | The identifier of the Amazon Connect instance.
describeQueue_instanceId :: Lens.Lens' DescribeQueue Prelude.Text
describeQueue_instanceId = Lens.lens (\DescribeQueue' {instanceId} -> instanceId) (\s@DescribeQueue' {} a -> s {instanceId = a} :: DescribeQueue)

-- | The identifier for the queue.
describeQueue_queueId :: Lens.Lens' DescribeQueue Prelude.Text
describeQueue_queueId = Lens.lens (\DescribeQueue' {queueId} -> queueId) (\s@DescribeQueue' {} a -> s {queueId = a} :: DescribeQueue)

instance Core.AWSRequest DescribeQueue where
  type
    AWSResponse DescribeQueue =
      DescribeQueueResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQueueResponse'
            Prelude.<$> (x Core..?> "Queue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeQueue

instance Prelude.NFData DescribeQueue

instance Core.ToHeaders DescribeQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeQueue where
  toPath DescribeQueue' {..} =
    Prelude.mconcat
      [ "/queues/",
        Core.toBS instanceId,
        "/",
        Core.toBS queueId
      ]

instance Core.ToQuery DescribeQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeQueueResponse' smart constructor.
data DescribeQueueResponse = DescribeQueueResponse'
  { -- | The name of the queue.
    queue :: Prelude.Maybe Queue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queue', 'describeQueueResponse_queue' - The name of the queue.
--
-- 'httpStatus', 'describeQueueResponse_httpStatus' - The response's http status code.
newDescribeQueueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeQueueResponse
newDescribeQueueResponse pHttpStatus_ =
  DescribeQueueResponse'
    { queue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the queue.
describeQueueResponse_queue :: Lens.Lens' DescribeQueueResponse (Prelude.Maybe Queue)
describeQueueResponse_queue = Lens.lens (\DescribeQueueResponse' {queue} -> queue) (\s@DescribeQueueResponse' {} a -> s {queue = a} :: DescribeQueueResponse)

-- | The response's http status code.
describeQueueResponse_httpStatus :: Lens.Lens' DescribeQueueResponse Prelude.Int
describeQueueResponse_httpStatus = Lens.lens (\DescribeQueueResponse' {httpStatus} -> httpStatus) (\s@DescribeQueueResponse' {} a -> s {httpStatus = a} :: DescribeQueueResponse)

instance Prelude.NFData DescribeQueueResponse
