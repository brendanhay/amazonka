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
-- Module      : Amazonka.Connect.UpdateQueueName
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Connect.UpdateQueueName
  ( -- * Creating a Request
    UpdateQueueName (..),
    newUpdateQueueName,

    -- * Request Lenses
    updateQueueName_description,
    updateQueueName_name,
    updateQueueName_instanceId,
    updateQueueName_queueId,

    -- * Destructuring the Response
    UpdateQueueNameResponse (..),
    newUpdateQueueNameResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateQueueName' smart constructor.
data UpdateQueueName = UpdateQueueName'
  { -- | The description of the queue.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the queue.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateQueueName_description' - The description of the queue.
--
-- 'name', 'updateQueueName_name' - The name of the queue.
--
-- 'instanceId', 'updateQueueName_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'queueId', 'updateQueueName_queueId' - The identifier for the queue.
newUpdateQueueName ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
  UpdateQueueName
newUpdateQueueName pInstanceId_ pQueueId_ =
  UpdateQueueName'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      instanceId = pInstanceId_,
      queueId = pQueueId_
    }

-- | The description of the queue.
updateQueueName_description :: Lens.Lens' UpdateQueueName (Prelude.Maybe Prelude.Text)
updateQueueName_description = Lens.lens (\UpdateQueueName' {description} -> description) (\s@UpdateQueueName' {} a -> s {description = a} :: UpdateQueueName)

-- | The name of the queue.
updateQueueName_name :: Lens.Lens' UpdateQueueName (Prelude.Maybe Prelude.Text)
updateQueueName_name = Lens.lens (\UpdateQueueName' {name} -> name) (\s@UpdateQueueName' {} a -> s {name = a} :: UpdateQueueName)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateQueueName_instanceId :: Lens.Lens' UpdateQueueName Prelude.Text
updateQueueName_instanceId = Lens.lens (\UpdateQueueName' {instanceId} -> instanceId) (\s@UpdateQueueName' {} a -> s {instanceId = a} :: UpdateQueueName)

-- | The identifier for the queue.
updateQueueName_queueId :: Lens.Lens' UpdateQueueName Prelude.Text
updateQueueName_queueId = Lens.lens (\UpdateQueueName' {queueId} -> queueId) (\s@UpdateQueueName' {} a -> s {queueId = a} :: UpdateQueueName)

instance Core.AWSRequest UpdateQueueName where
  type
    AWSResponse UpdateQueueName =
      UpdateQueueNameResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateQueueNameResponse'

instance Prelude.Hashable UpdateQueueName where
  hashWithSalt _salt UpdateQueueName' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` queueId

instance Prelude.NFData UpdateQueueName where
  rnf UpdateQueueName' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf queueId

instance Data.ToHeaders UpdateQueueName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateQueueName where
  toJSON UpdateQueueName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateQueueName where
  toPath UpdateQueueName' {..} =
    Prelude.mconcat
      [ "/queues/",
        Data.toBS instanceId,
        "/",
        Data.toBS queueId,
        "/name"
      ]

instance Data.ToQuery UpdateQueueName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQueueNameResponse' smart constructor.
data UpdateQueueNameResponse = UpdateQueueNameResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQueueNameResponse ::
  UpdateQueueNameResponse
newUpdateQueueNameResponse = UpdateQueueNameResponse'

instance Prelude.NFData UpdateQueueNameResponse where
  rnf _ = ()
