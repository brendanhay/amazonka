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
-- Module      : Network.AWS.DMS.RebootReplicationInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a replication instance. Rebooting results in a momentary outage,
-- until the replication instance becomes available again.
module Network.AWS.DMS.RebootReplicationInstance
  ( -- * Creating a Request
    RebootReplicationInstance (..),
    newRebootReplicationInstance,

    -- * Request Lenses
    rebootReplicationInstance_forceFailover,
    rebootReplicationInstance_replicationInstanceArn,

    -- * Destructuring the Response
    RebootReplicationInstanceResponse (..),
    newRebootReplicationInstanceResponse,

    -- * Response Lenses
    rebootReplicationInstanceResponse_replicationInstance,
    rebootReplicationInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRebootReplicationInstance' smart constructor.
data RebootReplicationInstance = RebootReplicationInstance'
  { -- | If this parameter is @true@, the reboot is conducted through a Multi-AZ
    -- failover. (If the instance isn\'t configured for Multi-AZ, then you
    -- can\'t specify @true@.)
    forceFailover :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootReplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceFailover', 'rebootReplicationInstance_forceFailover' - If this parameter is @true@, the reboot is conducted through a Multi-AZ
-- failover. (If the instance isn\'t configured for Multi-AZ, then you
-- can\'t specify @true@.)
--
-- 'replicationInstanceArn', 'rebootReplicationInstance_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
newRebootReplicationInstance ::
  -- | 'replicationInstanceArn'
  Prelude.Text ->
  RebootReplicationInstance
newRebootReplicationInstance pReplicationInstanceArn_ =
  RebootReplicationInstance'
    { forceFailover =
        Prelude.Nothing,
      replicationInstanceArn =
        pReplicationInstanceArn_
    }

-- | If this parameter is @true@, the reboot is conducted through a Multi-AZ
-- failover. (If the instance isn\'t configured for Multi-AZ, then you
-- can\'t specify @true@.)
rebootReplicationInstance_forceFailover :: Lens.Lens' RebootReplicationInstance (Prelude.Maybe Prelude.Bool)
rebootReplicationInstance_forceFailover = Lens.lens (\RebootReplicationInstance' {forceFailover} -> forceFailover) (\s@RebootReplicationInstance' {} a -> s {forceFailover = a} :: RebootReplicationInstance)

-- | The Amazon Resource Name (ARN) of the replication instance.
rebootReplicationInstance_replicationInstanceArn :: Lens.Lens' RebootReplicationInstance Prelude.Text
rebootReplicationInstance_replicationInstanceArn = Lens.lens (\RebootReplicationInstance' {replicationInstanceArn} -> replicationInstanceArn) (\s@RebootReplicationInstance' {} a -> s {replicationInstanceArn = a} :: RebootReplicationInstance)

instance Core.AWSRequest RebootReplicationInstance where
  type
    AWSResponse RebootReplicationInstance =
      RebootReplicationInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootReplicationInstanceResponse'
            Prelude.<$> (x Core..?> "ReplicationInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootReplicationInstance

instance Prelude.NFData RebootReplicationInstance

instance Core.ToHeaders RebootReplicationInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.RebootReplicationInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RebootReplicationInstance where
  toJSON RebootReplicationInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ForceFailover" Core..=) Prelude.<$> forceFailover,
            Prelude.Just
              ( "ReplicationInstanceArn"
                  Core..= replicationInstanceArn
              )
          ]
      )

instance Core.ToPath RebootReplicationInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery RebootReplicationInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebootReplicationInstanceResponse' smart constructor.
data RebootReplicationInstanceResponse = RebootReplicationInstanceResponse'
  { -- | The replication instance that is being rebooted.
    replicationInstance :: Prelude.Maybe ReplicationInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootReplicationInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationInstance', 'rebootReplicationInstanceResponse_replicationInstance' - The replication instance that is being rebooted.
--
-- 'httpStatus', 'rebootReplicationInstanceResponse_httpStatus' - The response's http status code.
newRebootReplicationInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootReplicationInstanceResponse
newRebootReplicationInstanceResponse pHttpStatus_ =
  RebootReplicationInstanceResponse'
    { replicationInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication instance that is being rebooted.
rebootReplicationInstanceResponse_replicationInstance :: Lens.Lens' RebootReplicationInstanceResponse (Prelude.Maybe ReplicationInstance)
rebootReplicationInstanceResponse_replicationInstance = Lens.lens (\RebootReplicationInstanceResponse' {replicationInstance} -> replicationInstance) (\s@RebootReplicationInstanceResponse' {} a -> s {replicationInstance = a} :: RebootReplicationInstanceResponse)

-- | The response's http status code.
rebootReplicationInstanceResponse_httpStatus :: Lens.Lens' RebootReplicationInstanceResponse Prelude.Int
rebootReplicationInstanceResponse_httpStatus = Lens.lens (\RebootReplicationInstanceResponse' {httpStatus} -> httpStatus) (\s@RebootReplicationInstanceResponse' {} a -> s {httpStatus = a} :: RebootReplicationInstanceResponse)

instance
  Prelude.NFData
    RebootReplicationInstanceResponse
