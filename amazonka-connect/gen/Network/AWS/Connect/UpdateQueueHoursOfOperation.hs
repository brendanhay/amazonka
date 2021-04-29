{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.UpdateQueueHoursOfOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the hours of operation for the specified queue.
module Network.AWS.Connect.UpdateQueueHoursOfOperation
  ( -- * Creating a Request
    UpdateQueueHoursOfOperation (..),
    newUpdateQueueHoursOfOperation,

    -- * Request Lenses
    updateQueueHoursOfOperation_instanceId,
    updateQueueHoursOfOperation_queueId,
    updateQueueHoursOfOperation_hoursOfOperationId,

    -- * Destructuring the Response
    UpdateQueueHoursOfOperationResponse (..),
    newUpdateQueueHoursOfOperationResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQueueHoursOfOperation' smart constructor.
data UpdateQueueHoursOfOperation = UpdateQueueHoursOfOperation'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueHoursOfOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateQueueHoursOfOperation_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'queueId', 'updateQueueHoursOfOperation_queueId' - The identifier for the queue.
--
-- 'hoursOfOperationId', 'updateQueueHoursOfOperation_hoursOfOperationId' - The identifier for the hours of operation.
newUpdateQueueHoursOfOperation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
  -- | 'hoursOfOperationId'
  Prelude.Text ->
  UpdateQueueHoursOfOperation
newUpdateQueueHoursOfOperation
  pInstanceId_
  pQueueId_
  pHoursOfOperationId_ =
    UpdateQueueHoursOfOperation'
      { instanceId =
          pInstanceId_,
        queueId = pQueueId_,
        hoursOfOperationId = pHoursOfOperationId_
      }

-- | The identifier of the Amazon Connect instance.
updateQueueHoursOfOperation_instanceId :: Lens.Lens' UpdateQueueHoursOfOperation Prelude.Text
updateQueueHoursOfOperation_instanceId = Lens.lens (\UpdateQueueHoursOfOperation' {instanceId} -> instanceId) (\s@UpdateQueueHoursOfOperation' {} a -> s {instanceId = a} :: UpdateQueueHoursOfOperation)

-- | The identifier for the queue.
updateQueueHoursOfOperation_queueId :: Lens.Lens' UpdateQueueHoursOfOperation Prelude.Text
updateQueueHoursOfOperation_queueId = Lens.lens (\UpdateQueueHoursOfOperation' {queueId} -> queueId) (\s@UpdateQueueHoursOfOperation' {} a -> s {queueId = a} :: UpdateQueueHoursOfOperation)

-- | The identifier for the hours of operation.
updateQueueHoursOfOperation_hoursOfOperationId :: Lens.Lens' UpdateQueueHoursOfOperation Prelude.Text
updateQueueHoursOfOperation_hoursOfOperationId = Lens.lens (\UpdateQueueHoursOfOperation' {hoursOfOperationId} -> hoursOfOperationId) (\s@UpdateQueueHoursOfOperation' {} a -> s {hoursOfOperationId = a} :: UpdateQueueHoursOfOperation)

instance
  Prelude.AWSRequest
    UpdateQueueHoursOfOperation
  where
  type
    Rs UpdateQueueHoursOfOperation =
      UpdateQueueHoursOfOperationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateQueueHoursOfOperationResponse'

instance Prelude.Hashable UpdateQueueHoursOfOperation

instance Prelude.NFData UpdateQueueHoursOfOperation

instance
  Prelude.ToHeaders
    UpdateQueueHoursOfOperation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateQueueHoursOfOperation where
  toJSON UpdateQueueHoursOfOperation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HoursOfOperationId"
                  Prelude..= hoursOfOperationId
              )
          ]
      )

instance Prelude.ToPath UpdateQueueHoursOfOperation where
  toPath UpdateQueueHoursOfOperation' {..} =
    Prelude.mconcat
      [ "/queues/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS queueId,
        "/hours-of-operation"
      ]

instance Prelude.ToQuery UpdateQueueHoursOfOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQueueHoursOfOperationResponse' smart constructor.
data UpdateQueueHoursOfOperationResponse = UpdateQueueHoursOfOperationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueHoursOfOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQueueHoursOfOperationResponse ::
  UpdateQueueHoursOfOperationResponse
newUpdateQueueHoursOfOperationResponse =
  UpdateQueueHoursOfOperationResponse'

instance
  Prelude.NFData
    UpdateQueueHoursOfOperationResponse
