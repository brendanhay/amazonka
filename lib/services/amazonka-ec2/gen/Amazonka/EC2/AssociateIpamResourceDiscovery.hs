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
-- Module      : Amazonka.EC2.AssociateIpamResourceDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an IPAM resource discovery with an Amazon VPC IPAM. A
-- resource discovery is an IPAM component that enables IPAM to manage and
-- monitor resources that belong to the owning account.
module Amazonka.EC2.AssociateIpamResourceDiscovery
  ( -- * Creating a Request
    AssociateIpamResourceDiscovery (..),
    newAssociateIpamResourceDiscovery,

    -- * Request Lenses
    associateIpamResourceDiscovery_clientToken,
    associateIpamResourceDiscovery_dryRun,
    associateIpamResourceDiscovery_tagSpecifications,
    associateIpamResourceDiscovery_ipamId,
    associateIpamResourceDiscovery_ipamResourceDiscoveryId,

    -- * Destructuring the Response
    AssociateIpamResourceDiscoveryResponse (..),
    newAssociateIpamResourceDiscoveryResponse,

    -- * Response Lenses
    associateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation,
    associateIpamResourceDiscoveryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateIpamResourceDiscovery' smart constructor.
data AssociateIpamResourceDiscovery = AssociateIpamResourceDiscovery'
  { -- | A client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Tag specifications.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | An IPAM ID.
    ipamId :: Prelude.Text,
    -- | A resource discovery ID.
    ipamResourceDiscoveryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateIpamResourceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateIpamResourceDiscovery_clientToken' - A client token.
--
-- 'dryRun', 'associateIpamResourceDiscovery_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'associateIpamResourceDiscovery_tagSpecifications' - Tag specifications.
--
-- 'ipamId', 'associateIpamResourceDiscovery_ipamId' - An IPAM ID.
--
-- 'ipamResourceDiscoveryId', 'associateIpamResourceDiscovery_ipamResourceDiscoveryId' - A resource discovery ID.
newAssociateIpamResourceDiscovery ::
  -- | 'ipamId'
  Prelude.Text ->
  -- | 'ipamResourceDiscoveryId'
  Prelude.Text ->
  AssociateIpamResourceDiscovery
newAssociateIpamResourceDiscovery
  pIpamId_
  pIpamResourceDiscoveryId_ =
    AssociateIpamResourceDiscovery'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        ipamId = pIpamId_,
        ipamResourceDiscoveryId =
          pIpamResourceDiscoveryId_
      }

-- | A client token.
associateIpamResourceDiscovery_clientToken :: Lens.Lens' AssociateIpamResourceDiscovery (Prelude.Maybe Prelude.Text)
associateIpamResourceDiscovery_clientToken = Lens.lens (\AssociateIpamResourceDiscovery' {clientToken} -> clientToken) (\s@AssociateIpamResourceDiscovery' {} a -> s {clientToken = a} :: AssociateIpamResourceDiscovery)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
associateIpamResourceDiscovery_dryRun :: Lens.Lens' AssociateIpamResourceDiscovery (Prelude.Maybe Prelude.Bool)
associateIpamResourceDiscovery_dryRun = Lens.lens (\AssociateIpamResourceDiscovery' {dryRun} -> dryRun) (\s@AssociateIpamResourceDiscovery' {} a -> s {dryRun = a} :: AssociateIpamResourceDiscovery)

-- | Tag specifications.
associateIpamResourceDiscovery_tagSpecifications :: Lens.Lens' AssociateIpamResourceDiscovery (Prelude.Maybe [TagSpecification])
associateIpamResourceDiscovery_tagSpecifications = Lens.lens (\AssociateIpamResourceDiscovery' {tagSpecifications} -> tagSpecifications) (\s@AssociateIpamResourceDiscovery' {} a -> s {tagSpecifications = a} :: AssociateIpamResourceDiscovery) Prelude.. Lens.mapping Lens.coerced

-- | An IPAM ID.
associateIpamResourceDiscovery_ipamId :: Lens.Lens' AssociateIpamResourceDiscovery Prelude.Text
associateIpamResourceDiscovery_ipamId = Lens.lens (\AssociateIpamResourceDiscovery' {ipamId} -> ipamId) (\s@AssociateIpamResourceDiscovery' {} a -> s {ipamId = a} :: AssociateIpamResourceDiscovery)

-- | A resource discovery ID.
associateIpamResourceDiscovery_ipamResourceDiscoveryId :: Lens.Lens' AssociateIpamResourceDiscovery Prelude.Text
associateIpamResourceDiscovery_ipamResourceDiscoveryId = Lens.lens (\AssociateIpamResourceDiscovery' {ipamResourceDiscoveryId} -> ipamResourceDiscoveryId) (\s@AssociateIpamResourceDiscovery' {} a -> s {ipamResourceDiscoveryId = a} :: AssociateIpamResourceDiscovery)

instance
  Core.AWSRequest
    AssociateIpamResourceDiscovery
  where
  type
    AWSResponse AssociateIpamResourceDiscovery =
      AssociateIpamResourceDiscoveryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateIpamResourceDiscoveryResponse'
            Prelude.<$> (x Data..@? "ipamResourceDiscoveryAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateIpamResourceDiscovery
  where
  hashWithSalt
    _salt
    AssociateIpamResourceDiscovery' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` ipamId
        `Prelude.hashWithSalt` ipamResourceDiscoveryId

instance
  Prelude.NFData
    AssociateIpamResourceDiscovery
  where
  rnf AssociateIpamResourceDiscovery' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf ipamId
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryId

instance
  Data.ToHeaders
    AssociateIpamResourceDiscovery
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateIpamResourceDiscovery where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateIpamResourceDiscovery where
  toQuery AssociateIpamResourceDiscovery' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AssociateIpamResourceDiscovery" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "IpamId" Data.=: ipamId,
        "IpamResourceDiscoveryId"
          Data.=: ipamResourceDiscoveryId
      ]

-- | /See:/ 'newAssociateIpamResourceDiscoveryResponse' smart constructor.
data AssociateIpamResourceDiscoveryResponse = AssociateIpamResourceDiscoveryResponse'
  { -- | A resource discovery association. An associated resource discovery is a
    -- resource discovery that has been associated with an IPAM.
    ipamResourceDiscoveryAssociation :: Prelude.Maybe IpamResourceDiscoveryAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateIpamResourceDiscoveryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamResourceDiscoveryAssociation', 'associateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation' - A resource discovery association. An associated resource discovery is a
-- resource discovery that has been associated with an IPAM.
--
-- 'httpStatus', 'associateIpamResourceDiscoveryResponse_httpStatus' - The response's http status code.
newAssociateIpamResourceDiscoveryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateIpamResourceDiscoveryResponse
newAssociateIpamResourceDiscoveryResponse
  pHttpStatus_ =
    AssociateIpamResourceDiscoveryResponse'
      { ipamResourceDiscoveryAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A resource discovery association. An associated resource discovery is a
-- resource discovery that has been associated with an IPAM.
associateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation :: Lens.Lens' AssociateIpamResourceDiscoveryResponse (Prelude.Maybe IpamResourceDiscoveryAssociation)
associateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation = Lens.lens (\AssociateIpamResourceDiscoveryResponse' {ipamResourceDiscoveryAssociation} -> ipamResourceDiscoveryAssociation) (\s@AssociateIpamResourceDiscoveryResponse' {} a -> s {ipamResourceDiscoveryAssociation = a} :: AssociateIpamResourceDiscoveryResponse)

-- | The response's http status code.
associateIpamResourceDiscoveryResponse_httpStatus :: Lens.Lens' AssociateIpamResourceDiscoveryResponse Prelude.Int
associateIpamResourceDiscoveryResponse_httpStatus = Lens.lens (\AssociateIpamResourceDiscoveryResponse' {httpStatus} -> httpStatus) (\s@AssociateIpamResourceDiscoveryResponse' {} a -> s {httpStatus = a} :: AssociateIpamResourceDiscoveryResponse)

instance
  Prelude.NFData
    AssociateIpamResourceDiscoveryResponse
  where
  rnf AssociateIpamResourceDiscoveryResponse' {..} =
    Prelude.rnf ipamResourceDiscoveryAssociation
      `Prelude.seq` Prelude.rnf httpStatus
