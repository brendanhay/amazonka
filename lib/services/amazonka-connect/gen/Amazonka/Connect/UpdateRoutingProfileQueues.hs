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
-- Module      : Amazonka.Connect.UpdateRoutingProfileQueues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties associated with a set of queues for a routing
-- profile.
module Amazonka.Connect.UpdateRoutingProfileQueues
  ( -- * Creating a Request
    UpdateRoutingProfileQueues (..),
    newUpdateRoutingProfileQueues,

    -- * Request Lenses
    updateRoutingProfileQueues_instanceId,
    updateRoutingProfileQueues_routingProfileId,
    updateRoutingProfileQueues_queueConfigs,

    -- * Destructuring the Response
    UpdateRoutingProfileQueuesResponse (..),
    newUpdateRoutingProfileQueuesResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRoutingProfileQueues' smart constructor.
data UpdateRoutingProfileQueues = UpdateRoutingProfileQueues'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Text,
    -- | The queues to be updated for this routing profile. Queues must first be
    -- associated to the routing profile. You can do this using
    -- AssociateRoutingProfileQueues.
    queueConfigs :: Prelude.NonEmpty RoutingProfileQueueConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingProfileQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateRoutingProfileQueues_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'routingProfileId', 'updateRoutingProfileQueues_routingProfileId' - The identifier of the routing profile.
--
-- 'queueConfigs', 'updateRoutingProfileQueues_queueConfigs' - The queues to be updated for this routing profile. Queues must first be
-- associated to the routing profile. You can do this using
-- AssociateRoutingProfileQueues.
newUpdateRoutingProfileQueues ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'routingProfileId'
  Prelude.Text ->
  -- | 'queueConfigs'
  Prelude.NonEmpty RoutingProfileQueueConfig ->
  UpdateRoutingProfileQueues
newUpdateRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_
  pQueueConfigs_ =
    UpdateRoutingProfileQueues'
      { instanceId =
          pInstanceId_,
        routingProfileId = pRoutingProfileId_,
        queueConfigs =
          Lens.coerced Lens.# pQueueConfigs_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateRoutingProfileQueues_instanceId :: Lens.Lens' UpdateRoutingProfileQueues Prelude.Text
updateRoutingProfileQueues_instanceId = Lens.lens (\UpdateRoutingProfileQueues' {instanceId} -> instanceId) (\s@UpdateRoutingProfileQueues' {} a -> s {instanceId = a} :: UpdateRoutingProfileQueues)

-- | The identifier of the routing profile.
updateRoutingProfileQueues_routingProfileId :: Lens.Lens' UpdateRoutingProfileQueues Prelude.Text
updateRoutingProfileQueues_routingProfileId = Lens.lens (\UpdateRoutingProfileQueues' {routingProfileId} -> routingProfileId) (\s@UpdateRoutingProfileQueues' {} a -> s {routingProfileId = a} :: UpdateRoutingProfileQueues)

-- | The queues to be updated for this routing profile. Queues must first be
-- associated to the routing profile. You can do this using
-- AssociateRoutingProfileQueues.
updateRoutingProfileQueues_queueConfigs :: Lens.Lens' UpdateRoutingProfileQueues (Prelude.NonEmpty RoutingProfileQueueConfig)
updateRoutingProfileQueues_queueConfigs = Lens.lens (\UpdateRoutingProfileQueues' {queueConfigs} -> queueConfigs) (\s@UpdateRoutingProfileQueues' {} a -> s {queueConfigs = a} :: UpdateRoutingProfileQueues) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateRoutingProfileQueues where
  type
    AWSResponse UpdateRoutingProfileQueues =
      UpdateRoutingProfileQueuesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateRoutingProfileQueuesResponse'

instance Prelude.Hashable UpdateRoutingProfileQueues where
  hashWithSalt _salt UpdateRoutingProfileQueues' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` routingProfileId
      `Prelude.hashWithSalt` queueConfigs

instance Prelude.NFData UpdateRoutingProfileQueues where
  rnf UpdateRoutingProfileQueues' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf routingProfileId
      `Prelude.seq` Prelude.rnf queueConfigs

instance Data.ToHeaders UpdateRoutingProfileQueues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoutingProfileQueues where
  toJSON UpdateRoutingProfileQueues' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("QueueConfigs" Data..= queueConfigs)]
      )

instance Data.ToPath UpdateRoutingProfileQueues where
  toPath UpdateRoutingProfileQueues' {..} =
    Prelude.mconcat
      [ "/routing-profiles/",
        Data.toBS instanceId,
        "/",
        Data.toBS routingProfileId,
        "/queues"
      ]

instance Data.ToQuery UpdateRoutingProfileQueues where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoutingProfileQueuesResponse' smart constructor.
data UpdateRoutingProfileQueuesResponse = UpdateRoutingProfileQueuesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingProfileQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRoutingProfileQueuesResponse ::
  UpdateRoutingProfileQueuesResponse
newUpdateRoutingProfileQueuesResponse =
  UpdateRoutingProfileQueuesResponse'

instance
  Prelude.NFData
    UpdateRoutingProfileQueuesResponse
  where
  rnf _ = ()
