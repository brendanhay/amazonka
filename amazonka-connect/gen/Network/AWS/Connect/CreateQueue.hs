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
    createQueue_tags,
    createQueue_description,
    createQueue_outboundCallerConfig,
    createQueue_quickConnectIds,
    createQueue_instanceId,
    createQueue_name,
    createQueue_hoursOfOperationId,

    -- * Destructuring the Response
    CreateQueueResponse (..),
    newCreateQueueResponse,

    -- * Response Lenses
    createQueueResponse_queueId,
    createQueueResponse_queueArn,
    createQueueResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateQueue' smart constructor.
data CreateQueue = CreateQueue'
  { -- | The maximum number of contacts that can be in the queue before it is
    -- considered full.
    maxContacts :: Core.Maybe Core.Natural,
    -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the queue.
    description :: Core.Maybe Core.Text,
    -- | The outbound caller ID name, number, and outbound whisper flow.
    outboundCallerConfig :: Core.Maybe OutboundCallerConfig,
    -- | The quick connects available to agents who are working the queue.
    quickConnectIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The name of the queue.
    name :: Core.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'tags', 'createQueue_tags' - One or more tags.
--
-- 'description', 'createQueue_description' - The description of the queue.
--
-- 'outboundCallerConfig', 'createQueue_outboundCallerConfig' - The outbound caller ID name, number, and outbound whisper flow.
--
-- 'quickConnectIds', 'createQueue_quickConnectIds' - The quick connects available to agents who are working the queue.
--
-- 'instanceId', 'createQueue_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'name', 'createQueue_name' - The name of the queue.
--
-- 'hoursOfOperationId', 'createQueue_hoursOfOperationId' - The identifier for the hours of operation.
newCreateQueue ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'hoursOfOperationId'
  Core.Text ->
  CreateQueue
newCreateQueue
  pInstanceId_
  pName_
  pHoursOfOperationId_ =
    CreateQueue'
      { maxContacts = Core.Nothing,
        tags = Core.Nothing,
        description = Core.Nothing,
        outboundCallerConfig = Core.Nothing,
        quickConnectIds = Core.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        hoursOfOperationId = pHoursOfOperationId_
      }

-- | The maximum number of contacts that can be in the queue before it is
-- considered full.
createQueue_maxContacts :: Lens.Lens' CreateQueue (Core.Maybe Core.Natural)
createQueue_maxContacts = Lens.lens (\CreateQueue' {maxContacts} -> maxContacts) (\s@CreateQueue' {} a -> s {maxContacts = a} :: CreateQueue)

-- | One or more tags.
createQueue_tags :: Lens.Lens' CreateQueue (Core.Maybe (Core.HashMap Core.Text Core.Text))
createQueue_tags = Lens.lens (\CreateQueue' {tags} -> tags) (\s@CreateQueue' {} a -> s {tags = a} :: CreateQueue) Core.. Lens.mapping Lens._Coerce

-- | The description of the queue.
createQueue_description :: Lens.Lens' CreateQueue (Core.Maybe Core.Text)
createQueue_description = Lens.lens (\CreateQueue' {description} -> description) (\s@CreateQueue' {} a -> s {description = a} :: CreateQueue)

-- | The outbound caller ID name, number, and outbound whisper flow.
createQueue_outboundCallerConfig :: Lens.Lens' CreateQueue (Core.Maybe OutboundCallerConfig)
createQueue_outboundCallerConfig = Lens.lens (\CreateQueue' {outboundCallerConfig} -> outboundCallerConfig) (\s@CreateQueue' {} a -> s {outboundCallerConfig = a} :: CreateQueue)

-- | The quick connects available to agents who are working the queue.
createQueue_quickConnectIds :: Lens.Lens' CreateQueue (Core.Maybe (Core.NonEmpty Core.Text))
createQueue_quickConnectIds = Lens.lens (\CreateQueue' {quickConnectIds} -> quickConnectIds) (\s@CreateQueue' {} a -> s {quickConnectIds = a} :: CreateQueue) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the Amazon Connect instance.
createQueue_instanceId :: Lens.Lens' CreateQueue Core.Text
createQueue_instanceId = Lens.lens (\CreateQueue' {instanceId} -> instanceId) (\s@CreateQueue' {} a -> s {instanceId = a} :: CreateQueue)

-- | The name of the queue.
createQueue_name :: Lens.Lens' CreateQueue Core.Text
createQueue_name = Lens.lens (\CreateQueue' {name} -> name) (\s@CreateQueue' {} a -> s {name = a} :: CreateQueue)

-- | The identifier for the hours of operation.
createQueue_hoursOfOperationId :: Lens.Lens' CreateQueue Core.Text
createQueue_hoursOfOperationId = Lens.lens (\CreateQueue' {hoursOfOperationId} -> hoursOfOperationId) (\s@CreateQueue' {} a -> s {hoursOfOperationId = a} :: CreateQueue)

instance Core.AWSRequest CreateQueue where
  type AWSResponse CreateQueue = CreateQueueResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateQueueResponse'
            Core.<$> (x Core..?> "QueueId")
            Core.<*> (x Core..?> "QueueArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateQueue

instance Core.NFData CreateQueue

instance Core.ToHeaders CreateQueue where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateQueue where
  toJSON CreateQueue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxContacts" Core..=) Core.<$> maxContacts,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            ("OutboundCallerConfig" Core..=)
              Core.<$> outboundCallerConfig,
            ("QuickConnectIds" Core..=) Core.<$> quickConnectIds,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("HoursOfOperationId" Core..= hoursOfOperationId)
          ]
      )

instance Core.ToPath CreateQueue where
  toPath CreateQueue' {..} =
    Core.mconcat ["/queues/", Core.toBS instanceId]

instance Core.ToQuery CreateQueue where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
  { -- | The identifier for the queue.
    queueId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the queue.
    queueArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueId', 'createQueueResponse_queueId' - The identifier for the queue.
--
-- 'queueArn', 'createQueueResponse_queueArn' - The Amazon Resource Name (ARN) of the queue.
--
-- 'httpStatus', 'createQueueResponse_httpStatus' - The response's http status code.
newCreateQueueResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateQueueResponse
newCreateQueueResponse pHttpStatus_ =
  CreateQueueResponse'
    { queueId = Core.Nothing,
      queueArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the queue.
createQueueResponse_queueId :: Lens.Lens' CreateQueueResponse (Core.Maybe Core.Text)
createQueueResponse_queueId = Lens.lens (\CreateQueueResponse' {queueId} -> queueId) (\s@CreateQueueResponse' {} a -> s {queueId = a} :: CreateQueueResponse)

-- | The Amazon Resource Name (ARN) of the queue.
createQueueResponse_queueArn :: Lens.Lens' CreateQueueResponse (Core.Maybe Core.Text)
createQueueResponse_queueArn = Lens.lens (\CreateQueueResponse' {queueArn} -> queueArn) (\s@CreateQueueResponse' {} a -> s {queueArn = a} :: CreateQueueResponse)

-- | The response's http status code.
createQueueResponse_httpStatus :: Lens.Lens' CreateQueueResponse Core.Int
createQueueResponse_httpStatus = Lens.lens (\CreateQueueResponse' {httpStatus} -> httpStatus) (\s@CreateQueueResponse' {} a -> s {httpStatus = a} :: CreateQueueResponse)

instance Core.NFData CreateQueueResponse
