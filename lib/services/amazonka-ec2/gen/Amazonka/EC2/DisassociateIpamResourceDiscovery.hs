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
-- Module      : Amazonka.EC2.DisassociateIpamResourceDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource discovery from an Amazon VPC IPAM. A resource
-- discovery is an IPAM component that enables IPAM to manage and monitor
-- resources that belong to the owning account.
module Amazonka.EC2.DisassociateIpamResourceDiscovery
  ( -- * Creating a Request
    DisassociateIpamResourceDiscovery (..),
    newDisassociateIpamResourceDiscovery,

    -- * Request Lenses
    disassociateIpamResourceDiscovery_dryRun,
    disassociateIpamResourceDiscovery_ipamResourceDiscoveryAssociationId,

    -- * Destructuring the Response
    DisassociateIpamResourceDiscoveryResponse (..),
    newDisassociateIpamResourceDiscoveryResponse,

    -- * Response Lenses
    disassociateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation,
    disassociateIpamResourceDiscoveryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateIpamResourceDiscovery' smart constructor.
data DisassociateIpamResourceDiscovery = DisassociateIpamResourceDiscovery'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A resource discovery association ID.
    ipamResourceDiscoveryAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateIpamResourceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disassociateIpamResourceDiscovery_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipamResourceDiscoveryAssociationId', 'disassociateIpamResourceDiscovery_ipamResourceDiscoveryAssociationId' - A resource discovery association ID.
newDisassociateIpamResourceDiscovery ::
  -- | 'ipamResourceDiscoveryAssociationId'
  Prelude.Text ->
  DisassociateIpamResourceDiscovery
newDisassociateIpamResourceDiscovery
  pIpamResourceDiscoveryAssociationId_ =
    DisassociateIpamResourceDiscovery'
      { dryRun =
          Prelude.Nothing,
        ipamResourceDiscoveryAssociationId =
          pIpamResourceDiscoveryAssociationId_
      }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
disassociateIpamResourceDiscovery_dryRun :: Lens.Lens' DisassociateIpamResourceDiscovery (Prelude.Maybe Prelude.Bool)
disassociateIpamResourceDiscovery_dryRun = Lens.lens (\DisassociateIpamResourceDiscovery' {dryRun} -> dryRun) (\s@DisassociateIpamResourceDiscovery' {} a -> s {dryRun = a} :: DisassociateIpamResourceDiscovery)

-- | A resource discovery association ID.
disassociateIpamResourceDiscovery_ipamResourceDiscoveryAssociationId :: Lens.Lens' DisassociateIpamResourceDiscovery Prelude.Text
disassociateIpamResourceDiscovery_ipamResourceDiscoveryAssociationId = Lens.lens (\DisassociateIpamResourceDiscovery' {ipamResourceDiscoveryAssociationId} -> ipamResourceDiscoveryAssociationId) (\s@DisassociateIpamResourceDiscovery' {} a -> s {ipamResourceDiscoveryAssociationId = a} :: DisassociateIpamResourceDiscovery)

instance
  Core.AWSRequest
    DisassociateIpamResourceDiscovery
  where
  type
    AWSResponse DisassociateIpamResourceDiscovery =
      DisassociateIpamResourceDiscoveryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateIpamResourceDiscoveryResponse'
            Prelude.<$> (x Data..@? "ipamResourceDiscoveryAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateIpamResourceDiscovery
  where
  hashWithSalt
    _salt
    DisassociateIpamResourceDiscovery' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` ipamResourceDiscoveryAssociationId

instance
  Prelude.NFData
    DisassociateIpamResourceDiscovery
  where
  rnf DisassociateIpamResourceDiscovery' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryAssociationId

instance
  Data.ToHeaders
    DisassociateIpamResourceDiscovery
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DisassociateIpamResourceDiscovery
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateIpamResourceDiscovery
  where
  toQuery DisassociateIpamResourceDiscovery' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DisassociateIpamResourceDiscovery" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "IpamResourceDiscoveryAssociationId"
          Data.=: ipamResourceDiscoveryAssociationId
      ]

-- | /See:/ 'newDisassociateIpamResourceDiscoveryResponse' smart constructor.
data DisassociateIpamResourceDiscoveryResponse = DisassociateIpamResourceDiscoveryResponse'
  { -- | A resource discovery association.
    ipamResourceDiscoveryAssociation :: Prelude.Maybe IpamResourceDiscoveryAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateIpamResourceDiscoveryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamResourceDiscoveryAssociation', 'disassociateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation' - A resource discovery association.
--
-- 'httpStatus', 'disassociateIpamResourceDiscoveryResponse_httpStatus' - The response's http status code.
newDisassociateIpamResourceDiscoveryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateIpamResourceDiscoveryResponse
newDisassociateIpamResourceDiscoveryResponse
  pHttpStatus_ =
    DisassociateIpamResourceDiscoveryResponse'
      { ipamResourceDiscoveryAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A resource discovery association.
disassociateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation :: Lens.Lens' DisassociateIpamResourceDiscoveryResponse (Prelude.Maybe IpamResourceDiscoveryAssociation)
disassociateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation = Lens.lens (\DisassociateIpamResourceDiscoveryResponse' {ipamResourceDiscoveryAssociation} -> ipamResourceDiscoveryAssociation) (\s@DisassociateIpamResourceDiscoveryResponse' {} a -> s {ipamResourceDiscoveryAssociation = a} :: DisassociateIpamResourceDiscoveryResponse)

-- | The response's http status code.
disassociateIpamResourceDiscoveryResponse_httpStatus :: Lens.Lens' DisassociateIpamResourceDiscoveryResponse Prelude.Int
disassociateIpamResourceDiscoveryResponse_httpStatus = Lens.lens (\DisassociateIpamResourceDiscoveryResponse' {httpStatus} -> httpStatus) (\s@DisassociateIpamResourceDiscoveryResponse' {} a -> s {httpStatus = a} :: DisassociateIpamResourceDiscoveryResponse)

instance
  Prelude.NFData
    DisassociateIpamResourceDiscoveryResponse
  where
  rnf DisassociateIpamResourceDiscoveryResponse' {..} =
    Prelude.rnf ipamResourceDiscoveryAssociation
      `Prelude.seq` Prelude.rnf httpStatus
