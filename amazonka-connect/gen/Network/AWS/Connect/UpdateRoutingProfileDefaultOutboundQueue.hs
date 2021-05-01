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
-- Module      : Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default outbound queue of a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
  ( -- * Creating a Request
    UpdateRoutingProfileDefaultOutboundQueue (..),
    newUpdateRoutingProfileDefaultOutboundQueue,

    -- * Request Lenses
    updateRoutingProfileDefaultOutboundQueue_instanceId,
    updateRoutingProfileDefaultOutboundQueue_routingProfileId,
    updateRoutingProfileDefaultOutboundQueue_defaultOutboundQueueId,

    -- * Destructuring the Response
    UpdateRoutingProfileDefaultOutboundQueueResponse (..),
    newUpdateRoutingProfileDefaultOutboundQueueResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRoutingProfileDefaultOutboundQueue' smart constructor.
data UpdateRoutingProfileDefaultOutboundQueue = UpdateRoutingProfileDefaultOutboundQueue'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Text,
    -- | The identifier for the default outbound queue.
    defaultOutboundQueueId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingProfileDefaultOutboundQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateRoutingProfileDefaultOutboundQueue_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'routingProfileId', 'updateRoutingProfileDefaultOutboundQueue_routingProfileId' - The identifier of the routing profile.
--
-- 'defaultOutboundQueueId', 'updateRoutingProfileDefaultOutboundQueue_defaultOutboundQueueId' - The identifier for the default outbound queue.
newUpdateRoutingProfileDefaultOutboundQueue ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'routingProfileId'
  Prelude.Text ->
  -- | 'defaultOutboundQueueId'
  Prelude.Text ->
  UpdateRoutingProfileDefaultOutboundQueue
newUpdateRoutingProfileDefaultOutboundQueue
  pInstanceId_
  pRoutingProfileId_
  pDefaultOutboundQueueId_ =
    UpdateRoutingProfileDefaultOutboundQueue'
      { instanceId =
          pInstanceId_,
        routingProfileId =
          pRoutingProfileId_,
        defaultOutboundQueueId =
          pDefaultOutboundQueueId_
      }

-- | The identifier of the Amazon Connect instance.
updateRoutingProfileDefaultOutboundQueue_instanceId :: Lens.Lens' UpdateRoutingProfileDefaultOutboundQueue Prelude.Text
updateRoutingProfileDefaultOutboundQueue_instanceId = Lens.lens (\UpdateRoutingProfileDefaultOutboundQueue' {instanceId} -> instanceId) (\s@UpdateRoutingProfileDefaultOutboundQueue' {} a -> s {instanceId = a} :: UpdateRoutingProfileDefaultOutboundQueue)

-- | The identifier of the routing profile.
updateRoutingProfileDefaultOutboundQueue_routingProfileId :: Lens.Lens' UpdateRoutingProfileDefaultOutboundQueue Prelude.Text
updateRoutingProfileDefaultOutboundQueue_routingProfileId = Lens.lens (\UpdateRoutingProfileDefaultOutboundQueue' {routingProfileId} -> routingProfileId) (\s@UpdateRoutingProfileDefaultOutboundQueue' {} a -> s {routingProfileId = a} :: UpdateRoutingProfileDefaultOutboundQueue)

-- | The identifier for the default outbound queue.
updateRoutingProfileDefaultOutboundQueue_defaultOutboundQueueId :: Lens.Lens' UpdateRoutingProfileDefaultOutboundQueue Prelude.Text
updateRoutingProfileDefaultOutboundQueue_defaultOutboundQueueId = Lens.lens (\UpdateRoutingProfileDefaultOutboundQueue' {defaultOutboundQueueId} -> defaultOutboundQueueId) (\s@UpdateRoutingProfileDefaultOutboundQueue' {} a -> s {defaultOutboundQueueId = a} :: UpdateRoutingProfileDefaultOutboundQueue)

instance
  Prelude.AWSRequest
    UpdateRoutingProfileDefaultOutboundQueue
  where
  type
    Rs UpdateRoutingProfileDefaultOutboundQueue =
      UpdateRoutingProfileDefaultOutboundQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateRoutingProfileDefaultOutboundQueueResponse'

instance
  Prelude.Hashable
    UpdateRoutingProfileDefaultOutboundQueue

instance
  Prelude.NFData
    UpdateRoutingProfileDefaultOutboundQueue

instance
  Prelude.ToHeaders
    UpdateRoutingProfileDefaultOutboundQueue
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

instance
  Prelude.ToJSON
    UpdateRoutingProfileDefaultOutboundQueue
  where
  toJSON UpdateRoutingProfileDefaultOutboundQueue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DefaultOutboundQueueId"
                  Prelude..= defaultOutboundQueueId
              )
          ]
      )

instance
  Prelude.ToPath
    UpdateRoutingProfileDefaultOutboundQueue
  where
  toPath UpdateRoutingProfileDefaultOutboundQueue' {..} =
    Prelude.mconcat
      [ "/routing-profiles/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS routingProfileId,
        "/default-outbound-queue"
      ]

instance
  Prelude.ToQuery
    UpdateRoutingProfileDefaultOutboundQueue
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoutingProfileDefaultOutboundQueueResponse' smart constructor.
data UpdateRoutingProfileDefaultOutboundQueueResponse = UpdateRoutingProfileDefaultOutboundQueueResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingProfileDefaultOutboundQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRoutingProfileDefaultOutboundQueueResponse ::
  UpdateRoutingProfileDefaultOutboundQueueResponse
newUpdateRoutingProfileDefaultOutboundQueueResponse =
  UpdateRoutingProfileDefaultOutboundQueueResponse'

instance
  Prelude.NFData
    UpdateRoutingProfileDefaultOutboundQueueResponse
