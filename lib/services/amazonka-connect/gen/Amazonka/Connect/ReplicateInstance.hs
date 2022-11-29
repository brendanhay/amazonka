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
-- Module      : Amazonka.Connect.ReplicateInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replicates an Amazon Connect instance in the specified Amazon Web
-- Services Region.
--
-- For more information about replicating an Amazon Connect instance, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/create-replica-connect-instance.html Create a replica of your existing Amazon Connect instance>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.ReplicateInstance
  ( -- * Creating a Request
    ReplicateInstance (..),
    newReplicateInstance,

    -- * Request Lenses
    replicateInstance_clientToken,
    replicateInstance_instanceId,
    replicateInstance_replicaRegion,
    replicateInstance_replicaAlias,

    -- * Destructuring the Response
    ReplicateInstanceResponse (..),
    newReplicateInstanceResponse,

    -- * Response Lenses
    replicateInstanceResponse_arn,
    replicateInstanceResponse_id,
    replicateInstanceResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReplicateInstance' smart constructor.
data ReplicateInstance = ReplicateInstance'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance. You can provide the @InstanceId@,
    -- or the entire ARN.
    instanceId :: Prelude.Text,
    -- | The Amazon Web Services Region where to replicate the Amazon Connect
    -- instance.
    replicaRegion :: Prelude.Text,
    -- | The alias for the replicated instance. The @ReplicaAlias@ must be
    -- unique.
    replicaAlias :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicateInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'replicateInstance_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'instanceId', 'replicateInstance_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance. You can provide the @InstanceId@,
-- or the entire ARN.
--
-- 'replicaRegion', 'replicateInstance_replicaRegion' - The Amazon Web Services Region where to replicate the Amazon Connect
-- instance.
--
-- 'replicaAlias', 'replicateInstance_replicaAlias' - The alias for the replicated instance. The @ReplicaAlias@ must be
-- unique.
newReplicateInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'replicaRegion'
  Prelude.Text ->
  -- | 'replicaAlias'
  Prelude.Text ->
  ReplicateInstance
newReplicateInstance
  pInstanceId_
  pReplicaRegion_
  pReplicaAlias_ =
    ReplicateInstance'
      { clientToken = Prelude.Nothing,
        instanceId = pInstanceId_,
        replicaRegion = pReplicaRegion_,
        replicaAlias = Core._Sensitive Lens.# pReplicaAlias_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
replicateInstance_clientToken :: Lens.Lens' ReplicateInstance (Prelude.Maybe Prelude.Text)
replicateInstance_clientToken = Lens.lens (\ReplicateInstance' {clientToken} -> clientToken) (\s@ReplicateInstance' {} a -> s {clientToken = a} :: ReplicateInstance)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance. You can provide the @InstanceId@,
-- or the entire ARN.
replicateInstance_instanceId :: Lens.Lens' ReplicateInstance Prelude.Text
replicateInstance_instanceId = Lens.lens (\ReplicateInstance' {instanceId} -> instanceId) (\s@ReplicateInstance' {} a -> s {instanceId = a} :: ReplicateInstance)

-- | The Amazon Web Services Region where to replicate the Amazon Connect
-- instance.
replicateInstance_replicaRegion :: Lens.Lens' ReplicateInstance Prelude.Text
replicateInstance_replicaRegion = Lens.lens (\ReplicateInstance' {replicaRegion} -> replicaRegion) (\s@ReplicateInstance' {} a -> s {replicaRegion = a} :: ReplicateInstance)

-- | The alias for the replicated instance. The @ReplicaAlias@ must be
-- unique.
replicateInstance_replicaAlias :: Lens.Lens' ReplicateInstance Prelude.Text
replicateInstance_replicaAlias = Lens.lens (\ReplicateInstance' {replicaAlias} -> replicaAlias) (\s@ReplicateInstance' {} a -> s {replicaAlias = a} :: ReplicateInstance) Prelude.. Core._Sensitive

instance Core.AWSRequest ReplicateInstance where
  type
    AWSResponse ReplicateInstance =
      ReplicateInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReplicateInstanceResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReplicateInstance where
  hashWithSalt _salt ReplicateInstance' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` replicaRegion
      `Prelude.hashWithSalt` replicaAlias

instance Prelude.NFData ReplicateInstance where
  rnf ReplicateInstance' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf replicaRegion
      `Prelude.seq` Prelude.rnf replicaAlias

instance Core.ToHeaders ReplicateInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ReplicateInstance where
  toJSON ReplicateInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("ReplicaRegion" Core..= replicaRegion),
            Prelude.Just ("ReplicaAlias" Core..= replicaAlias)
          ]
      )

instance Core.ToPath ReplicateInstance where
  toPath ReplicateInstance' {..} =
    Prelude.mconcat
      ["/instance/", Core.toBS instanceId, "/replicate"]

instance Core.ToQuery ReplicateInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReplicateInstanceResponse' smart constructor.
data ReplicateInstanceResponse = ReplicateInstanceResponse'
  { -- | The Amazon Resource Name (ARN) of the replicated instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the replicated instance. You can find the @instanceId@
    -- in the ARN of the instance. The replicated instance has the same
    -- identifier as the instance it was replicated from.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicateInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'replicateInstanceResponse_arn' - The Amazon Resource Name (ARN) of the replicated instance.
--
-- 'id', 'replicateInstanceResponse_id' - The identifier of the replicated instance. You can find the @instanceId@
-- in the ARN of the instance. The replicated instance has the same
-- identifier as the instance it was replicated from.
--
-- 'httpStatus', 'replicateInstanceResponse_httpStatus' - The response's http status code.
newReplicateInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReplicateInstanceResponse
newReplicateInstanceResponse pHttpStatus_ =
  ReplicateInstanceResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the replicated instance.
replicateInstanceResponse_arn :: Lens.Lens' ReplicateInstanceResponse (Prelude.Maybe Prelude.Text)
replicateInstanceResponse_arn = Lens.lens (\ReplicateInstanceResponse' {arn} -> arn) (\s@ReplicateInstanceResponse' {} a -> s {arn = a} :: ReplicateInstanceResponse)

-- | The identifier of the replicated instance. You can find the @instanceId@
-- in the ARN of the instance. The replicated instance has the same
-- identifier as the instance it was replicated from.
replicateInstanceResponse_id :: Lens.Lens' ReplicateInstanceResponse (Prelude.Maybe Prelude.Text)
replicateInstanceResponse_id = Lens.lens (\ReplicateInstanceResponse' {id} -> id) (\s@ReplicateInstanceResponse' {} a -> s {id = a} :: ReplicateInstanceResponse)

-- | The response's http status code.
replicateInstanceResponse_httpStatus :: Lens.Lens' ReplicateInstanceResponse Prelude.Int
replicateInstanceResponse_httpStatus = Lens.lens (\ReplicateInstanceResponse' {httpStatus} -> httpStatus) (\s@ReplicateInstanceResponse' {} a -> s {httpStatus = a} :: ReplicateInstanceResponse)

instance Prelude.NFData ReplicateInstanceResponse where
  rnf ReplicateInstanceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
