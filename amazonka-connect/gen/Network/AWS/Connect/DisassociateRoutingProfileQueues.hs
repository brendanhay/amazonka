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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateRoutingProfileQueues' smart constructor.
data DisassociateRoutingProfileQueues = DisassociateRoutingProfileQueues'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Text,
    -- | The queues to disassociate from this routing profile.
    queueReferences :: [RoutingProfileQueueReference]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'routingProfileId'
  Prelude.Text ->
  DisassociateRoutingProfileQueues
newDisassociateRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_ =
    DisassociateRoutingProfileQueues'
      { instanceId =
          pInstanceId_,
        routingProfileId = pRoutingProfileId_,
        queueReferences = Prelude.mempty
      }

-- | The identifier of the Amazon Connect instance.
disassociateRoutingProfileQueues_instanceId :: Lens.Lens' DisassociateRoutingProfileQueues Prelude.Text
disassociateRoutingProfileQueues_instanceId = Lens.lens (\DisassociateRoutingProfileQueues' {instanceId} -> instanceId) (\s@DisassociateRoutingProfileQueues' {} a -> s {instanceId = a} :: DisassociateRoutingProfileQueues)

-- | The identifier of the routing profile.
disassociateRoutingProfileQueues_routingProfileId :: Lens.Lens' DisassociateRoutingProfileQueues Prelude.Text
disassociateRoutingProfileQueues_routingProfileId = Lens.lens (\DisassociateRoutingProfileQueues' {routingProfileId} -> routingProfileId) (\s@DisassociateRoutingProfileQueues' {} a -> s {routingProfileId = a} :: DisassociateRoutingProfileQueues)

-- | The queues to disassociate from this routing profile.
disassociateRoutingProfileQueues_queueReferences :: Lens.Lens' DisassociateRoutingProfileQueues [RoutingProfileQueueReference]
disassociateRoutingProfileQueues_queueReferences = Lens.lens (\DisassociateRoutingProfileQueues' {queueReferences} -> queueReferences) (\s@DisassociateRoutingProfileQueues' {} a -> s {queueReferences = a} :: DisassociateRoutingProfileQueues) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    DisassociateRoutingProfileQueues
  where
  type
    Rs DisassociateRoutingProfileQueues =
      DisassociateRoutingProfileQueuesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DisassociateRoutingProfileQueuesResponse'

instance
  Prelude.Hashable
    DisassociateRoutingProfileQueues

instance
  Prelude.NFData
    DisassociateRoutingProfileQueues

instance
  Prelude.ToHeaders
    DisassociateRoutingProfileQueues
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
    DisassociateRoutingProfileQueues
  where
  toJSON DisassociateRoutingProfileQueues' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QueueReferences" Prelude..= queueReferences)
          ]
      )

instance
  Prelude.ToPath
    DisassociateRoutingProfileQueues
  where
  toPath DisassociateRoutingProfileQueues' {..} =
    Prelude.mconcat
      [ "/routing-profiles/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS routingProfileId,
        "/disassociate-queues"
      ]

instance
  Prelude.ToQuery
    DisassociateRoutingProfileQueues
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateRoutingProfileQueuesResponse' smart constructor.
data DisassociateRoutingProfileQueuesResponse = DisassociateRoutingProfileQueuesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateRoutingProfileQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateRoutingProfileQueuesResponse ::
  DisassociateRoutingProfileQueuesResponse
newDisassociateRoutingProfileQueuesResponse =
  DisassociateRoutingProfileQueuesResponse'

instance
  Prelude.NFData
    DisassociateRoutingProfileQueuesResponse
