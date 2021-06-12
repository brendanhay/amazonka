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
-- Module      : Network.AWS.Connect.UpdateQueueName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the name and description of a queue. At least @Name@ or
-- @Description@ must be provided.
module Network.AWS.Connect.UpdateQueueName
  ( -- * Creating a Request
    UpdateQueueName (..),
    newUpdateQueueName,

    -- * Request Lenses
    updateQueueName_name,
    updateQueueName_description,
    updateQueueName_instanceId,
    updateQueueName_queueId,

    -- * Destructuring the Response
    UpdateQueueNameResponse (..),
    newUpdateQueueNameResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQueueName' smart constructor.
data UpdateQueueName = UpdateQueueName'
  { -- | The name of the queue.
    name :: Core.Maybe Core.Text,
    -- | The description of the queue.
    description :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the queue.
    queueId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQueueName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateQueueName_name' - The name of the queue.
--
-- 'description', 'updateQueueName_description' - The description of the queue.
--
-- 'instanceId', 'updateQueueName_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'queueId', 'updateQueueName_queueId' - The identifier for the queue.
newUpdateQueueName ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'queueId'
  Core.Text ->
  UpdateQueueName
newUpdateQueueName pInstanceId_ pQueueId_ =
  UpdateQueueName'
    { name = Core.Nothing,
      description = Core.Nothing,
      instanceId = pInstanceId_,
      queueId = pQueueId_
    }

-- | The name of the queue.
updateQueueName_name :: Lens.Lens' UpdateQueueName (Core.Maybe Core.Text)
updateQueueName_name = Lens.lens (\UpdateQueueName' {name} -> name) (\s@UpdateQueueName' {} a -> s {name = a} :: UpdateQueueName)

-- | The description of the queue.
updateQueueName_description :: Lens.Lens' UpdateQueueName (Core.Maybe Core.Text)
updateQueueName_description = Lens.lens (\UpdateQueueName' {description} -> description) (\s@UpdateQueueName' {} a -> s {description = a} :: UpdateQueueName)

-- | The identifier of the Amazon Connect instance.
updateQueueName_instanceId :: Lens.Lens' UpdateQueueName Core.Text
updateQueueName_instanceId = Lens.lens (\UpdateQueueName' {instanceId} -> instanceId) (\s@UpdateQueueName' {} a -> s {instanceId = a} :: UpdateQueueName)

-- | The identifier for the queue.
updateQueueName_queueId :: Lens.Lens' UpdateQueueName Core.Text
updateQueueName_queueId = Lens.lens (\UpdateQueueName' {queueId} -> queueId) (\s@UpdateQueueName' {} a -> s {queueId = a} :: UpdateQueueName)

instance Core.AWSRequest UpdateQueueName where
  type
    AWSResponse UpdateQueueName =
      UpdateQueueNameResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateQueueNameResponse'

instance Core.Hashable UpdateQueueName

instance Core.NFData UpdateQueueName

instance Core.ToHeaders UpdateQueueName where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateQueueName where
  toJSON UpdateQueueName' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.ToPath UpdateQueueName where
  toPath UpdateQueueName' {..} =
    Core.mconcat
      [ "/queues/",
        Core.toBS instanceId,
        "/",
        Core.toBS queueId,
        "/name"
      ]

instance Core.ToQuery UpdateQueueName where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateQueueNameResponse' smart constructor.
data UpdateQueueNameResponse = UpdateQueueNameResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQueueNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQueueNameResponse ::
  UpdateQueueNameResponse
newUpdateQueueNameResponse = UpdateQueueNameResponse'

instance Core.NFData UpdateQueueNameResponse
