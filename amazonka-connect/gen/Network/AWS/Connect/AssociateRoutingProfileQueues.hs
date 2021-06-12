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
-- Module      : Network.AWS.Connect.AssociateRoutingProfileQueues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of queues with a routing profile.
module Network.AWS.Connect.AssociateRoutingProfileQueues
  ( -- * Creating a Request
    AssociateRoutingProfileQueues (..),
    newAssociateRoutingProfileQueues,

    -- * Request Lenses
    associateRoutingProfileQueues_instanceId,
    associateRoutingProfileQueues_routingProfileId,
    associateRoutingProfileQueues_queueConfigs,

    -- * Destructuring the Response
    AssociateRoutingProfileQueuesResponse (..),
    newAssociateRoutingProfileQueuesResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateRoutingProfileQueues' smart constructor.
data AssociateRoutingProfileQueues = AssociateRoutingProfileQueues'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Core.Text,
    -- | The queues to associate with this routing profile.
    queueConfigs :: Core.NonEmpty RoutingProfileQueueConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateRoutingProfileQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateRoutingProfileQueues_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'routingProfileId', 'associateRoutingProfileQueues_routingProfileId' - The identifier of the routing profile.
--
-- 'queueConfigs', 'associateRoutingProfileQueues_queueConfigs' - The queues to associate with this routing profile.
newAssociateRoutingProfileQueues ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'routingProfileId'
  Core.Text ->
  -- | 'queueConfigs'
  Core.NonEmpty RoutingProfileQueueConfig ->
  AssociateRoutingProfileQueues
newAssociateRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_
  pQueueConfigs_ =
    AssociateRoutingProfileQueues'
      { instanceId =
          pInstanceId_,
        routingProfileId = pRoutingProfileId_,
        queueConfigs =
          Lens._Coerce Lens.# pQueueConfigs_
      }

-- | The identifier of the Amazon Connect instance.
associateRoutingProfileQueues_instanceId :: Lens.Lens' AssociateRoutingProfileQueues Core.Text
associateRoutingProfileQueues_instanceId = Lens.lens (\AssociateRoutingProfileQueues' {instanceId} -> instanceId) (\s@AssociateRoutingProfileQueues' {} a -> s {instanceId = a} :: AssociateRoutingProfileQueues)

-- | The identifier of the routing profile.
associateRoutingProfileQueues_routingProfileId :: Lens.Lens' AssociateRoutingProfileQueues Core.Text
associateRoutingProfileQueues_routingProfileId = Lens.lens (\AssociateRoutingProfileQueues' {routingProfileId} -> routingProfileId) (\s@AssociateRoutingProfileQueues' {} a -> s {routingProfileId = a} :: AssociateRoutingProfileQueues)

-- | The queues to associate with this routing profile.
associateRoutingProfileQueues_queueConfigs :: Lens.Lens' AssociateRoutingProfileQueues (Core.NonEmpty RoutingProfileQueueConfig)
associateRoutingProfileQueues_queueConfigs = Lens.lens (\AssociateRoutingProfileQueues' {queueConfigs} -> queueConfigs) (\s@AssociateRoutingProfileQueues' {} a -> s {queueConfigs = a} :: AssociateRoutingProfileQueues) Core.. Lens._Coerce

instance
  Core.AWSRequest
    AssociateRoutingProfileQueues
  where
  type
    AWSResponse AssociateRoutingProfileQueues =
      AssociateRoutingProfileQueuesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      AssociateRoutingProfileQueuesResponse'

instance Core.Hashable AssociateRoutingProfileQueues

instance Core.NFData AssociateRoutingProfileQueues

instance Core.ToHeaders AssociateRoutingProfileQueues where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateRoutingProfileQueues where
  toJSON AssociateRoutingProfileQueues' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("QueueConfigs" Core..= queueConfigs)]
      )

instance Core.ToPath AssociateRoutingProfileQueues where
  toPath AssociateRoutingProfileQueues' {..} =
    Core.mconcat
      [ "/routing-profiles/",
        Core.toBS instanceId,
        "/",
        Core.toBS routingProfileId,
        "/associate-queues"
      ]

instance Core.ToQuery AssociateRoutingProfileQueues where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateRoutingProfileQueuesResponse' smart constructor.
data AssociateRoutingProfileQueuesResponse = AssociateRoutingProfileQueuesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateRoutingProfileQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateRoutingProfileQueuesResponse ::
  AssociateRoutingProfileQueuesResponse
newAssociateRoutingProfileQueuesResponse =
  AssociateRoutingProfileQueuesResponse'

instance
  Core.NFData
    AssociateRoutingProfileQueuesResponse
