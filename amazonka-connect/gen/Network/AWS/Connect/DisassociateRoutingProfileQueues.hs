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
-- Module      : Network.AWS.Connect.DisassociateRoutingProfileQueues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a set of queues from a routing profile.
module Network.AWS.Connect.DisassociateRoutingProfileQueues
  ( -- * Creating a Request
    DisassociateRoutingProfileQueues (..),
    newDisassociateRoutingProfileQueues,

    -- * Request Lenses
    disassociateRoutingProfileQueues_instanceId,
    disassociateRoutingProfileQueues_routingProfileId,
    disassociateRoutingProfileQueues_queueReferences,

    -- * Destructuring the Response
    DisassociateRoutingProfileQueuesResponse (..),
    newDisassociateRoutingProfileQueuesResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateRoutingProfileQueues' smart constructor.
data DisassociateRoutingProfileQueues = DisassociateRoutingProfileQueues'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Core.Text,
    -- | The queues to disassociate from this routing profile.
    queueReferences :: [RoutingProfileQueueReference]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateRoutingProfileQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateRoutingProfileQueues_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'routingProfileId', 'disassociateRoutingProfileQueues_routingProfileId' - The identifier of the routing profile.
--
-- 'queueReferences', 'disassociateRoutingProfileQueues_queueReferences' - The queues to disassociate from this routing profile.
newDisassociateRoutingProfileQueues ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'routingProfileId'
  Core.Text ->
  DisassociateRoutingProfileQueues
newDisassociateRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_ =
    DisassociateRoutingProfileQueues'
      { instanceId =
          pInstanceId_,
        routingProfileId = pRoutingProfileId_,
        queueReferences = Core.mempty
      }

-- | The identifier of the Amazon Connect instance.
disassociateRoutingProfileQueues_instanceId :: Lens.Lens' DisassociateRoutingProfileQueues Core.Text
disassociateRoutingProfileQueues_instanceId = Lens.lens (\DisassociateRoutingProfileQueues' {instanceId} -> instanceId) (\s@DisassociateRoutingProfileQueues' {} a -> s {instanceId = a} :: DisassociateRoutingProfileQueues)

-- | The identifier of the routing profile.
disassociateRoutingProfileQueues_routingProfileId :: Lens.Lens' DisassociateRoutingProfileQueues Core.Text
disassociateRoutingProfileQueues_routingProfileId = Lens.lens (\DisassociateRoutingProfileQueues' {routingProfileId} -> routingProfileId) (\s@DisassociateRoutingProfileQueues' {} a -> s {routingProfileId = a} :: DisassociateRoutingProfileQueues)

-- | The queues to disassociate from this routing profile.
disassociateRoutingProfileQueues_queueReferences :: Lens.Lens' DisassociateRoutingProfileQueues [RoutingProfileQueueReference]
disassociateRoutingProfileQueues_queueReferences = Lens.lens (\DisassociateRoutingProfileQueues' {queueReferences} -> queueReferences) (\s@DisassociateRoutingProfileQueues' {} a -> s {queueReferences = a} :: DisassociateRoutingProfileQueues) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DisassociateRoutingProfileQueues
  where
  type
    AWSResponse DisassociateRoutingProfileQueues =
      DisassociateRoutingProfileQueuesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DisassociateRoutingProfileQueuesResponse'

instance
  Core.Hashable
    DisassociateRoutingProfileQueues

instance Core.NFData DisassociateRoutingProfileQueues

instance
  Core.ToHeaders
    DisassociateRoutingProfileQueues
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateRoutingProfileQueues where
  toJSON DisassociateRoutingProfileQueues' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QueueReferences" Core..= queueReferences)
          ]
      )

instance Core.ToPath DisassociateRoutingProfileQueues where
  toPath DisassociateRoutingProfileQueues' {..} =
    Core.mconcat
      [ "/routing-profiles/",
        Core.toBS instanceId,
        "/",
        Core.toBS routingProfileId,
        "/disassociate-queues"
      ]

instance
  Core.ToQuery
    DisassociateRoutingProfileQueues
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateRoutingProfileQueuesResponse' smart constructor.
data DisassociateRoutingProfileQueuesResponse = DisassociateRoutingProfileQueuesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateRoutingProfileQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateRoutingProfileQueuesResponse ::
  DisassociateRoutingProfileQueuesResponse
newDisassociateRoutingProfileQueuesResponse =
  DisassociateRoutingProfileQueuesResponse'

instance
  Core.NFData
    DisassociateRoutingProfileQueuesResponse
