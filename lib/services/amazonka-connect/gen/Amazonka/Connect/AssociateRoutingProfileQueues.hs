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
-- Module      : Amazonka.Connect.AssociateRoutingProfileQueues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of queues with a routing profile.
module Amazonka.Connect.AssociateRoutingProfileQueues
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateRoutingProfileQueues' smart constructor.
data AssociateRoutingProfileQueues = AssociateRoutingProfileQueues'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Text,
    -- | The queues to associate with this routing profile.
    queueConfigs :: Prelude.NonEmpty RoutingProfileQueueConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateRoutingProfileQueues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateRoutingProfileQueues_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'routingProfileId', 'associateRoutingProfileQueues_routingProfileId' - The identifier of the routing profile.
--
-- 'queueConfigs', 'associateRoutingProfileQueues_queueConfigs' - The queues to associate with this routing profile.
newAssociateRoutingProfileQueues ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'routingProfileId'
  Prelude.Text ->
  -- | 'queueConfigs'
  Prelude.NonEmpty RoutingProfileQueueConfig ->
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
          Lens.coerced Lens.# pQueueConfigs_
      }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
associateRoutingProfileQueues_instanceId :: Lens.Lens' AssociateRoutingProfileQueues Prelude.Text
associateRoutingProfileQueues_instanceId = Lens.lens (\AssociateRoutingProfileQueues' {instanceId} -> instanceId) (\s@AssociateRoutingProfileQueues' {} a -> s {instanceId = a} :: AssociateRoutingProfileQueues)

-- | The identifier of the routing profile.
associateRoutingProfileQueues_routingProfileId :: Lens.Lens' AssociateRoutingProfileQueues Prelude.Text
associateRoutingProfileQueues_routingProfileId = Lens.lens (\AssociateRoutingProfileQueues' {routingProfileId} -> routingProfileId) (\s@AssociateRoutingProfileQueues' {} a -> s {routingProfileId = a} :: AssociateRoutingProfileQueues)

-- | The queues to associate with this routing profile.
associateRoutingProfileQueues_queueConfigs :: Lens.Lens' AssociateRoutingProfileQueues (Prelude.NonEmpty RoutingProfileQueueConfig)
associateRoutingProfileQueues_queueConfigs = Lens.lens (\AssociateRoutingProfileQueues' {queueConfigs} -> queueConfigs) (\s@AssociateRoutingProfileQueues' {} a -> s {queueConfigs = a} :: AssociateRoutingProfileQueues) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    AssociateRoutingProfileQueues
  where
  type
    AWSResponse AssociateRoutingProfileQueues =
      AssociateRoutingProfileQueuesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      AssociateRoutingProfileQueuesResponse'

instance
  Prelude.Hashable
    AssociateRoutingProfileQueues
  where
  hashWithSalt _salt AssociateRoutingProfileQueues' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` routingProfileId
      `Prelude.hashWithSalt` queueConfigs

instance Prelude.NFData AssociateRoutingProfileQueues where
  rnf AssociateRoutingProfileQueues' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf routingProfileId
      `Prelude.seq` Prelude.rnf queueConfigs

instance Data.ToHeaders AssociateRoutingProfileQueues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateRoutingProfileQueues where
  toJSON AssociateRoutingProfileQueues' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("QueueConfigs" Data..= queueConfigs)]
      )

instance Data.ToPath AssociateRoutingProfileQueues where
  toPath AssociateRoutingProfileQueues' {..} =
    Prelude.mconcat
      [ "/routing-profiles/",
        Data.toBS instanceId,
        "/",
        Data.toBS routingProfileId,
        "/associate-queues"
      ]

instance Data.ToQuery AssociateRoutingProfileQueues where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateRoutingProfileQueuesResponse' smart constructor.
data AssociateRoutingProfileQueuesResponse = AssociateRoutingProfileQueuesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateRoutingProfileQueuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateRoutingProfileQueuesResponse ::
  AssociateRoutingProfileQueuesResponse
newAssociateRoutingProfileQueuesResponse =
  AssociateRoutingProfileQueuesResponse'

instance
  Prelude.NFData
    AssociateRoutingProfileQueuesResponse
  where
  rnf _ = ()
