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
-- Module      : Amazonka.EC2.ModifyIpamResourceCidr
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a resource CIDR. You can use this action to transfer resource
-- CIDRs between scopes and ignore resource CIDRs that you do not want to
-- manage. If set to false, the resource will not be tracked for overlap,
-- it cannot be auto-imported into a pool, and it will be removed from any
-- pool it has an allocation in.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/move-resource-ipam.html Move resource CIDRs between scopes>
-- and
-- <https://docs.aws.amazon.com/vpc/latest/ipam/change-monitoring-state-ipam.html Change the monitoring state of resource CIDRs>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.ModifyIpamResourceCidr
  ( -- * Creating a Request
    ModifyIpamResourceCidr (..),
    newModifyIpamResourceCidr,

    -- * Request Lenses
    modifyIpamResourceCidr_destinationIpamScopeId,
    modifyIpamResourceCidr_dryRun,
    modifyIpamResourceCidr_resourceId,
    modifyIpamResourceCidr_resourceCidr,
    modifyIpamResourceCidr_resourceRegion,
    modifyIpamResourceCidr_currentIpamScopeId,
    modifyIpamResourceCidr_monitored,

    -- * Destructuring the Response
    ModifyIpamResourceCidrResponse (..),
    newModifyIpamResourceCidrResponse,

    -- * Response Lenses
    modifyIpamResourceCidrResponse_ipamResourceCidr,
    modifyIpamResourceCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyIpamResourceCidr' smart constructor.
data ModifyIpamResourceCidr = ModifyIpamResourceCidr'
  { -- | The ID of the scope you want to transfer the resource CIDR to.
    destinationIpamScopeId :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the resource you want to modify.
    resourceId :: Prelude.Text,
    -- | The CIDR of the resource you want to modify.
    resourceCidr :: Prelude.Text,
    -- | The Amazon Web Services Region of the resource you want to modify.
    resourceRegion :: Prelude.Text,
    -- | The ID of the current scope that the resource CIDR is in.
    currentIpamScopeId :: Prelude.Text,
    -- | Determines if the resource is monitored by IPAM. If a resource is
    -- monitored, the resource is discovered by IPAM and you can view details
    -- about the resource’s CIDR.
    monitored :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpamResourceCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationIpamScopeId', 'modifyIpamResourceCidr_destinationIpamScopeId' - The ID of the scope you want to transfer the resource CIDR to.
--
-- 'dryRun', 'modifyIpamResourceCidr_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'resourceId', 'modifyIpamResourceCidr_resourceId' - The ID of the resource you want to modify.
--
-- 'resourceCidr', 'modifyIpamResourceCidr_resourceCidr' - The CIDR of the resource you want to modify.
--
-- 'resourceRegion', 'modifyIpamResourceCidr_resourceRegion' - The Amazon Web Services Region of the resource you want to modify.
--
-- 'currentIpamScopeId', 'modifyIpamResourceCidr_currentIpamScopeId' - The ID of the current scope that the resource CIDR is in.
--
-- 'monitored', 'modifyIpamResourceCidr_monitored' - Determines if the resource is monitored by IPAM. If a resource is
-- monitored, the resource is discovered by IPAM and you can view details
-- about the resource’s CIDR.
newModifyIpamResourceCidr ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceCidr'
  Prelude.Text ->
  -- | 'resourceRegion'
  Prelude.Text ->
  -- | 'currentIpamScopeId'
  Prelude.Text ->
  -- | 'monitored'
  Prelude.Bool ->
  ModifyIpamResourceCidr
newModifyIpamResourceCidr
  pResourceId_
  pResourceCidr_
  pResourceRegion_
  pCurrentIpamScopeId_
  pMonitored_ =
    ModifyIpamResourceCidr'
      { destinationIpamScopeId =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        resourceId = pResourceId_,
        resourceCidr = pResourceCidr_,
        resourceRegion = pResourceRegion_,
        currentIpamScopeId = pCurrentIpamScopeId_,
        monitored = pMonitored_
      }

-- | The ID of the scope you want to transfer the resource CIDR to.
modifyIpamResourceCidr_destinationIpamScopeId :: Lens.Lens' ModifyIpamResourceCidr (Prelude.Maybe Prelude.Text)
modifyIpamResourceCidr_destinationIpamScopeId = Lens.lens (\ModifyIpamResourceCidr' {destinationIpamScopeId} -> destinationIpamScopeId) (\s@ModifyIpamResourceCidr' {} a -> s {destinationIpamScopeId = a} :: ModifyIpamResourceCidr)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
modifyIpamResourceCidr_dryRun :: Lens.Lens' ModifyIpamResourceCidr (Prelude.Maybe Prelude.Bool)
modifyIpamResourceCidr_dryRun = Lens.lens (\ModifyIpamResourceCidr' {dryRun} -> dryRun) (\s@ModifyIpamResourceCidr' {} a -> s {dryRun = a} :: ModifyIpamResourceCidr)

-- | The ID of the resource you want to modify.
modifyIpamResourceCidr_resourceId :: Lens.Lens' ModifyIpamResourceCidr Prelude.Text
modifyIpamResourceCidr_resourceId = Lens.lens (\ModifyIpamResourceCidr' {resourceId} -> resourceId) (\s@ModifyIpamResourceCidr' {} a -> s {resourceId = a} :: ModifyIpamResourceCidr)

-- | The CIDR of the resource you want to modify.
modifyIpamResourceCidr_resourceCidr :: Lens.Lens' ModifyIpamResourceCidr Prelude.Text
modifyIpamResourceCidr_resourceCidr = Lens.lens (\ModifyIpamResourceCidr' {resourceCidr} -> resourceCidr) (\s@ModifyIpamResourceCidr' {} a -> s {resourceCidr = a} :: ModifyIpamResourceCidr)

-- | The Amazon Web Services Region of the resource you want to modify.
modifyIpamResourceCidr_resourceRegion :: Lens.Lens' ModifyIpamResourceCidr Prelude.Text
modifyIpamResourceCidr_resourceRegion = Lens.lens (\ModifyIpamResourceCidr' {resourceRegion} -> resourceRegion) (\s@ModifyIpamResourceCidr' {} a -> s {resourceRegion = a} :: ModifyIpamResourceCidr)

-- | The ID of the current scope that the resource CIDR is in.
modifyIpamResourceCidr_currentIpamScopeId :: Lens.Lens' ModifyIpamResourceCidr Prelude.Text
modifyIpamResourceCidr_currentIpamScopeId = Lens.lens (\ModifyIpamResourceCidr' {currentIpamScopeId} -> currentIpamScopeId) (\s@ModifyIpamResourceCidr' {} a -> s {currentIpamScopeId = a} :: ModifyIpamResourceCidr)

-- | Determines if the resource is monitored by IPAM. If a resource is
-- monitored, the resource is discovered by IPAM and you can view details
-- about the resource’s CIDR.
modifyIpamResourceCidr_monitored :: Lens.Lens' ModifyIpamResourceCidr Prelude.Bool
modifyIpamResourceCidr_monitored = Lens.lens (\ModifyIpamResourceCidr' {monitored} -> monitored) (\s@ModifyIpamResourceCidr' {} a -> s {monitored = a} :: ModifyIpamResourceCidr)

instance Core.AWSRequest ModifyIpamResourceCidr where
  type
    AWSResponse ModifyIpamResourceCidr =
      ModifyIpamResourceCidrResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyIpamResourceCidrResponse'
            Prelude.<$> (x Data..@? "ipamResourceCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyIpamResourceCidr where
  hashWithSalt _salt ModifyIpamResourceCidr' {..} =
    _salt `Prelude.hashWithSalt` destinationIpamScopeId
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceCidr
      `Prelude.hashWithSalt` resourceRegion
      `Prelude.hashWithSalt` currentIpamScopeId
      `Prelude.hashWithSalt` monitored

instance Prelude.NFData ModifyIpamResourceCidr where
  rnf ModifyIpamResourceCidr' {..} =
    Prelude.rnf destinationIpamScopeId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceCidr
      `Prelude.seq` Prelude.rnf resourceRegion
      `Prelude.seq` Prelude.rnf currentIpamScopeId
      `Prelude.seq` Prelude.rnf monitored

instance Data.ToHeaders ModifyIpamResourceCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyIpamResourceCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyIpamResourceCidr where
  toQuery ModifyIpamResourceCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyIpamResourceCidr" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DestinationIpamScopeId"
          Data.=: destinationIpamScopeId,
        "DryRun" Data.=: dryRun,
        "ResourceId" Data.=: resourceId,
        "ResourceCidr" Data.=: resourceCidr,
        "ResourceRegion" Data.=: resourceRegion,
        "CurrentIpamScopeId" Data.=: currentIpamScopeId,
        "Monitored" Data.=: monitored
      ]

-- | /See:/ 'newModifyIpamResourceCidrResponse' smart constructor.
data ModifyIpamResourceCidrResponse = ModifyIpamResourceCidrResponse'
  { -- | The CIDR of the resource.
    ipamResourceCidr :: Prelude.Maybe IpamResourceCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpamResourceCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamResourceCidr', 'modifyIpamResourceCidrResponse_ipamResourceCidr' - The CIDR of the resource.
--
-- 'httpStatus', 'modifyIpamResourceCidrResponse_httpStatus' - The response's http status code.
newModifyIpamResourceCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyIpamResourceCidrResponse
newModifyIpamResourceCidrResponse pHttpStatus_ =
  ModifyIpamResourceCidrResponse'
    { ipamResourceCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The CIDR of the resource.
modifyIpamResourceCidrResponse_ipamResourceCidr :: Lens.Lens' ModifyIpamResourceCidrResponse (Prelude.Maybe IpamResourceCidr)
modifyIpamResourceCidrResponse_ipamResourceCidr = Lens.lens (\ModifyIpamResourceCidrResponse' {ipamResourceCidr} -> ipamResourceCidr) (\s@ModifyIpamResourceCidrResponse' {} a -> s {ipamResourceCidr = a} :: ModifyIpamResourceCidrResponse)

-- | The response's http status code.
modifyIpamResourceCidrResponse_httpStatus :: Lens.Lens' ModifyIpamResourceCidrResponse Prelude.Int
modifyIpamResourceCidrResponse_httpStatus = Lens.lens (\ModifyIpamResourceCidrResponse' {httpStatus} -> httpStatus) (\s@ModifyIpamResourceCidrResponse' {} a -> s {httpStatus = a} :: ModifyIpamResourceCidrResponse)

instance
  Prelude.NFData
    ModifyIpamResourceCidrResponse
  where
  rnf ModifyIpamResourceCidrResponse' {..} =
    Prelude.rnf ipamResourceCidr
      `Prelude.seq` Prelude.rnf httpStatus
