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
-- Module      : Network.AWS.Connect.CreateQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Creates a new queue for the specified Amazon Connect instance.
module Network.AWS.Connect.CreateQueue
  ( -- * Creating a Request
    CreateQueue (..),
    newCreateQueue,

    -- * Request Lenses
    createQueue_maxContacts,
    createQueue_quickConnectIds,
    createQueue_outboundCallerConfig,
    createQueue_description,
    createQueue_tags,
    createQueue_instanceId,
    createQueue_name,
    createQueue_hoursOfOperationId,

    -- * Destructuring the Response
    CreateQueueResponse (..),
    newCreateQueueResponse,

    -- * Response Lenses
    createQueueResponse_queueArn,
    createQueueResponse_queueId,
    createQueueResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateQueue' smart constructor.
data CreateQueue = CreateQueue'
  { -- | The maximum number of contacts that can be in the queue before it is
    -- considered full.
    maxContacts :: Prelude.Maybe Prelude.Natural,
    -- | The quick connects available to agents who are working the queue.
    quickConnectIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The outbound caller ID name, number, and outbound whisper flow.
    outboundCallerConfig :: Prelude.Maybe OutboundCallerConfig,
    -- | The description of the queue.
    description :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The name of the queue.
    name :: Prelude.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxContacts', 'createQueue_maxContacts' - The maximum number of contacts that can be in the queue before it is
-- considered full.
--
-- 'quickConnectIds', 'createQueue_quickConnectIds' - The quick connects available to agents who are working the queue.
--
-- 'outboundCallerConfig', 'createQueue_outboundCallerConfig' - The outbound caller ID name, number, and outbound whisper flow.
--
-- 'description', 'createQueue_description' - The description of the queue.
--
-- 'tags', 'createQueue_tags' - One or more tags.
--
-- 'instanceId', 'createQueue_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'name', 'createQueue_name' - The name of the queue.
--
-- 'hoursOfOperationId', 'createQueue_hoursOfOperationId' - The identifier for the hours of operation.
newCreateQueue ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'hoursOfOperationId'
  Prelude.Text ->
  CreateQueue
newCreateQueue
  pInstanceId_
  pName_
  pHoursOfOperationId_ =
    CreateQueue'
      { maxContacts = Prelude.Nothing,
        quickConnectIds = Prelude.Nothing,
        outboundCallerConfig = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        hoursOfOperationId = pHoursOfOperationId_
      }

-- | The maximum number of contacts that can be in the queue before it is
-- considered full.
createQueue_maxContacts :: Lens.Lens' CreateQueue (Prelude.Maybe Prelude.Natural)
createQueue_maxContacts = Lens.lens (\CreateQueue' {maxContacts} -> maxContacts) (\s@CreateQueue' {} a -> s {maxContacts = a} :: CreateQueue)

-- | The quick connects available to agents who are working the queue.
createQueue_quickConnectIds :: Lens.Lens' CreateQueue (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createQueue_quickConnectIds = Lens.lens (\CreateQueue' {quickConnectIds} -> quickConnectIds) (\s@CreateQueue' {} a -> s {quickConnectIds = a} :: CreateQueue) Prelude.. Lens.mapping Lens.coerced

-- | The outbound caller ID name, number, and outbound whisper flow.
createQueue_outboundCallerConfig :: Lens.Lens' CreateQueue (Prelude.Maybe OutboundCallerConfig)
createQueue_outboundCallerConfig = Lens.lens (\CreateQueue' {outboundCallerConfig} -> outboundCallerConfig) (\s@CreateQueue' {} a -> s {outboundCallerConfig = a} :: CreateQueue)

-- | The description of the queue.
createQueue_description :: Lens.Lens' CreateQueue (Prelude.Maybe Prelude.Text)
createQueue_description = Lens.lens (\CreateQueue' {description} -> description) (\s@CreateQueue' {} a -> s {description = a} :: CreateQueue)

-- | One or more tags.
createQueue_tags :: Lens.Lens' CreateQueue (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createQueue_tags = Lens.lens (\CreateQueue' {tags} -> tags) (\s@CreateQueue' {} a -> s {tags = a} :: CreateQueue) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createQueue_instanceId :: Lens.Lens' CreateQueue Prelude.Text
createQueue_instanceId = Lens.lens (\CreateQueue' {instanceId} -> instanceId) (\s@CreateQueue' {} a -> s {instanceId = a} :: CreateQueue)

-- | The name of the queue.
createQueue_name :: Lens.Lens' CreateQueue Prelude.Text
createQueue_name = Lens.lens (\CreateQueue' {name} -> name) (\s@CreateQueue' {} a -> s {name = a} :: CreateQueue)

-- | The identifier for the hours of operation.
createQueue_hoursOfOperationId :: Lens.Lens' CreateQueue Prelude.Text
createQueue_hoursOfOperationId = Lens.lens (\CreateQueue' {hoursOfOperationId} -> hoursOfOperationId) (\s@CreateQueue' {} a -> s {hoursOfOperationId = a} :: CreateQueue)

instance Core.AWSRequest CreateQueue where
  type AWSResponse CreateQueue = CreateQueueResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateQueueResponse'
            Prelude.<$> (x Core..?> "QueueArn")
            Prelude.<*> (x Core..?> "QueueId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateQueue

instance Prelude.NFData CreateQueue

instance Core.ToHeaders CreateQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateQueue where
  toJSON CreateQueue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxContacts" Core..=) Prelude.<$> maxContacts,
            ("QuickConnectIds" Core..=)
              Prelude.<$> quickConnectIds,
            ("OutboundCallerConfig" Core..=)
              Prelude.<$> outboundCallerConfig,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("HoursOfOperationId" Core..= hoursOfOperationId)
          ]
      )

instance Core.ToPath CreateQueue where
  toPath CreateQueue' {..} =
    Prelude.mconcat ["/queues/", Core.toBS instanceId]

instance Core.ToQuery CreateQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
  { -- | The Amazon Resource Name (ARN) of the queue.
    queueArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueArn', 'createQueueResponse_queueArn' - The Amazon Resource Name (ARN) of the queue.
--
-- 'queueId', 'createQueueResponse_queueId' - The identifier for the queue.
--
-- 'httpStatus', 'createQueueResponse_httpStatus' - The response's http status code.
newCreateQueueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateQueueResponse
newCreateQueueResponse pHttpStatus_ =
  CreateQueueResponse'
    { queueArn = Prelude.Nothing,
      queueId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the queue.
createQueueResponse_queueArn :: Lens.Lens' CreateQueueResponse (Prelude.Maybe Prelude.Text)
createQueueResponse_queueArn = Lens.lens (\CreateQueueResponse' {queueArn} -> queueArn) (\s@CreateQueueResponse' {} a -> s {queueArn = a} :: CreateQueueResponse)

-- | The identifier for the queue.
createQueueResponse_queueId :: Lens.Lens' CreateQueueResponse (Prelude.Maybe Prelude.Text)
createQueueResponse_queueId = Lens.lens (\CreateQueueResponse' {queueId} -> queueId) (\s@CreateQueueResponse' {} a -> s {queueId = a} :: CreateQueueResponse)

-- | The response's http status code.
createQueueResponse_httpStatus :: Lens.Lens' CreateQueueResponse Prelude.Int
createQueueResponse_httpStatus = Lens.lens (\CreateQueueResponse' {httpStatus} -> httpStatus) (\s@CreateQueueResponse' {} a -> s {httpStatus = a} :: CreateQueueResponse)

instance Prelude.NFData CreateQueueResponse
