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
-- Module      : Amazonka.EC2.DeleteIpamResourceDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an IPAM resource discovery. A resource discovery is an IPAM
-- component that enables IPAM to manage and monitor resources that belong
-- to the owning account.
module Amazonka.EC2.DeleteIpamResourceDiscovery
  ( -- * Creating a Request
    DeleteIpamResourceDiscovery (..),
    newDeleteIpamResourceDiscovery,

    -- * Request Lenses
    deleteIpamResourceDiscovery_dryRun,
    deleteIpamResourceDiscovery_ipamResourceDiscoveryId,

    -- * Destructuring the Response
    DeleteIpamResourceDiscoveryResponse (..),
    newDeleteIpamResourceDiscoveryResponse,

    -- * Response Lenses
    deleteIpamResourceDiscoveryResponse_ipamResourceDiscovery,
    deleteIpamResourceDiscoveryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIpamResourceDiscovery' smart constructor.
data DeleteIpamResourceDiscovery = DeleteIpamResourceDiscovery'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IPAM resource discovery ID.
    ipamResourceDiscoveryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpamResourceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteIpamResourceDiscovery_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamResourceDiscoveryId', 'deleteIpamResourceDiscovery_ipamResourceDiscoveryId' - The IPAM resource discovery ID.
newDeleteIpamResourceDiscovery ::
  -- | 'ipamResourceDiscoveryId'
  Prelude.Text ->
  DeleteIpamResourceDiscovery
newDeleteIpamResourceDiscovery
  pIpamResourceDiscoveryId_ =
    DeleteIpamResourceDiscovery'
      { dryRun =
          Prelude.Nothing,
        ipamResourceDiscoveryId =
          pIpamResourceDiscoveryId_
      }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
deleteIpamResourceDiscovery_dryRun :: Lens.Lens' DeleteIpamResourceDiscovery (Prelude.Maybe Prelude.Bool)
deleteIpamResourceDiscovery_dryRun = Lens.lens (\DeleteIpamResourceDiscovery' {dryRun} -> dryRun) (\s@DeleteIpamResourceDiscovery' {} a -> s {dryRun = a} :: DeleteIpamResourceDiscovery)

-- | The IPAM resource discovery ID.
deleteIpamResourceDiscovery_ipamResourceDiscoveryId :: Lens.Lens' DeleteIpamResourceDiscovery Prelude.Text
deleteIpamResourceDiscovery_ipamResourceDiscoveryId = Lens.lens (\DeleteIpamResourceDiscovery' {ipamResourceDiscoveryId} -> ipamResourceDiscoveryId) (\s@DeleteIpamResourceDiscovery' {} a -> s {ipamResourceDiscoveryId = a} :: DeleteIpamResourceDiscovery)

instance Core.AWSRequest DeleteIpamResourceDiscovery where
  type
    AWSResponse DeleteIpamResourceDiscovery =
      DeleteIpamResourceDiscoveryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteIpamResourceDiscoveryResponse'
            Prelude.<$> (x Data..@? "ipamResourceDiscovery")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIpamResourceDiscovery where
  hashWithSalt _salt DeleteIpamResourceDiscovery' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipamResourceDiscoveryId

instance Prelude.NFData DeleteIpamResourceDiscovery where
  rnf DeleteIpamResourceDiscovery' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryId

instance Data.ToHeaders DeleteIpamResourceDiscovery where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteIpamResourceDiscovery where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIpamResourceDiscovery where
  toQuery DeleteIpamResourceDiscovery' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteIpamResourceDiscovery" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "IpamResourceDiscoveryId"
          Data.=: ipamResourceDiscoveryId
      ]

-- | /See:/ 'newDeleteIpamResourceDiscoveryResponse' smart constructor.
data DeleteIpamResourceDiscoveryResponse = DeleteIpamResourceDiscoveryResponse'
  { -- | The IPAM resource discovery.
    ipamResourceDiscovery :: Prelude.Maybe IpamResourceDiscovery,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpamResourceDiscoveryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamResourceDiscovery', 'deleteIpamResourceDiscoveryResponse_ipamResourceDiscovery' - The IPAM resource discovery.
--
-- 'httpStatus', 'deleteIpamResourceDiscoveryResponse_httpStatus' - The response's http status code.
newDeleteIpamResourceDiscoveryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIpamResourceDiscoveryResponse
newDeleteIpamResourceDiscoveryResponse pHttpStatus_ =
  DeleteIpamResourceDiscoveryResponse'
    { ipamResourceDiscovery =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IPAM resource discovery.
deleteIpamResourceDiscoveryResponse_ipamResourceDiscovery :: Lens.Lens' DeleteIpamResourceDiscoveryResponse (Prelude.Maybe IpamResourceDiscovery)
deleteIpamResourceDiscoveryResponse_ipamResourceDiscovery = Lens.lens (\DeleteIpamResourceDiscoveryResponse' {ipamResourceDiscovery} -> ipamResourceDiscovery) (\s@DeleteIpamResourceDiscoveryResponse' {} a -> s {ipamResourceDiscovery = a} :: DeleteIpamResourceDiscoveryResponse)

-- | The response's http status code.
deleteIpamResourceDiscoveryResponse_httpStatus :: Lens.Lens' DeleteIpamResourceDiscoveryResponse Prelude.Int
deleteIpamResourceDiscoveryResponse_httpStatus = Lens.lens (\DeleteIpamResourceDiscoveryResponse' {httpStatus} -> httpStatus) (\s@DeleteIpamResourceDiscoveryResponse' {} a -> s {httpStatus = a} :: DeleteIpamResourceDiscoveryResponse)

instance
  Prelude.NFData
    DeleteIpamResourceDiscoveryResponse
  where
  rnf DeleteIpamResourceDiscoveryResponse' {..} =
    Prelude.rnf ipamResourceDiscovery
      `Prelude.seq` Prelude.rnf httpStatus
