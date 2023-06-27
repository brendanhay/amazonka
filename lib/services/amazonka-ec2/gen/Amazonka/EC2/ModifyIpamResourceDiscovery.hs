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
-- Module      : Amazonka.EC2.ModifyIpamResourceDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a resource discovery. A resource discovery is an IPAM component
-- that enables IPAM to manage and monitor resources that belong to the
-- owning account.
module Amazonka.EC2.ModifyIpamResourceDiscovery
  ( -- * Creating a Request
    ModifyIpamResourceDiscovery (..),
    newModifyIpamResourceDiscovery,

    -- * Request Lenses
    modifyIpamResourceDiscovery_addOperatingRegions,
    modifyIpamResourceDiscovery_description,
    modifyIpamResourceDiscovery_dryRun,
    modifyIpamResourceDiscovery_removeOperatingRegions,
    modifyIpamResourceDiscovery_ipamResourceDiscoveryId,

    -- * Destructuring the Response
    ModifyIpamResourceDiscoveryResponse (..),
    newModifyIpamResourceDiscoveryResponse,

    -- * Response Lenses
    modifyIpamResourceDiscoveryResponse_ipamResourceDiscovery,
    modifyIpamResourceDiscoveryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyIpamResourceDiscovery' smart constructor.
data ModifyIpamResourceDiscovery = ModifyIpamResourceDiscovery'
  { -- | Add operating Regions to the resource discovery. Operating Regions are
    -- Amazon Web Services Regions where the IPAM is allowed to manage IP
    -- address CIDRs. IPAM only discovers and monitors resources in the Amazon
    -- Web Services Regions you select as operating Regions.
    addOperatingRegions :: Prelude.Maybe [AddIpamOperatingRegion],
    -- | A resource discovery description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Remove operating Regions.
    removeOperatingRegions :: Prelude.Maybe [RemoveIpamOperatingRegion],
    -- | A resource discovery ID.
    ipamResourceDiscoveryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpamResourceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addOperatingRegions', 'modifyIpamResourceDiscovery_addOperatingRegions' - Add operating Regions to the resource discovery. Operating Regions are
-- Amazon Web Services Regions where the IPAM is allowed to manage IP
-- address CIDRs. IPAM only discovers and monitors resources in the Amazon
-- Web Services Regions you select as operating Regions.
--
-- 'description', 'modifyIpamResourceDiscovery_description' - A resource discovery description.
--
-- 'dryRun', 'modifyIpamResourceDiscovery_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'removeOperatingRegions', 'modifyIpamResourceDiscovery_removeOperatingRegions' - Remove operating Regions.
--
-- 'ipamResourceDiscoveryId', 'modifyIpamResourceDiscovery_ipamResourceDiscoveryId' - A resource discovery ID.
newModifyIpamResourceDiscovery ::
  -- | 'ipamResourceDiscoveryId'
  Prelude.Text ->
  ModifyIpamResourceDiscovery
newModifyIpamResourceDiscovery
  pIpamResourceDiscoveryId_ =
    ModifyIpamResourceDiscovery'
      { addOperatingRegions =
          Prelude.Nothing,
        description = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        removeOperatingRegions = Prelude.Nothing,
        ipamResourceDiscoveryId =
          pIpamResourceDiscoveryId_
      }

-- | Add operating Regions to the resource discovery. Operating Regions are
-- Amazon Web Services Regions where the IPAM is allowed to manage IP
-- address CIDRs. IPAM only discovers and monitors resources in the Amazon
-- Web Services Regions you select as operating Regions.
modifyIpamResourceDiscovery_addOperatingRegions :: Lens.Lens' ModifyIpamResourceDiscovery (Prelude.Maybe [AddIpamOperatingRegion])
modifyIpamResourceDiscovery_addOperatingRegions = Lens.lens (\ModifyIpamResourceDiscovery' {addOperatingRegions} -> addOperatingRegions) (\s@ModifyIpamResourceDiscovery' {} a -> s {addOperatingRegions = a} :: ModifyIpamResourceDiscovery) Prelude.. Lens.mapping Lens.coerced

-- | A resource discovery description.
modifyIpamResourceDiscovery_description :: Lens.Lens' ModifyIpamResourceDiscovery (Prelude.Maybe Prelude.Text)
modifyIpamResourceDiscovery_description = Lens.lens (\ModifyIpamResourceDiscovery' {description} -> description) (\s@ModifyIpamResourceDiscovery' {} a -> s {description = a} :: ModifyIpamResourceDiscovery)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
modifyIpamResourceDiscovery_dryRun :: Lens.Lens' ModifyIpamResourceDiscovery (Prelude.Maybe Prelude.Bool)
modifyIpamResourceDiscovery_dryRun = Lens.lens (\ModifyIpamResourceDiscovery' {dryRun} -> dryRun) (\s@ModifyIpamResourceDiscovery' {} a -> s {dryRun = a} :: ModifyIpamResourceDiscovery)

-- | Remove operating Regions.
modifyIpamResourceDiscovery_removeOperatingRegions :: Lens.Lens' ModifyIpamResourceDiscovery (Prelude.Maybe [RemoveIpamOperatingRegion])
modifyIpamResourceDiscovery_removeOperatingRegions = Lens.lens (\ModifyIpamResourceDiscovery' {removeOperatingRegions} -> removeOperatingRegions) (\s@ModifyIpamResourceDiscovery' {} a -> s {removeOperatingRegions = a} :: ModifyIpamResourceDiscovery) Prelude.. Lens.mapping Lens.coerced

-- | A resource discovery ID.
modifyIpamResourceDiscovery_ipamResourceDiscoveryId :: Lens.Lens' ModifyIpamResourceDiscovery Prelude.Text
modifyIpamResourceDiscovery_ipamResourceDiscoveryId = Lens.lens (\ModifyIpamResourceDiscovery' {ipamResourceDiscoveryId} -> ipamResourceDiscoveryId) (\s@ModifyIpamResourceDiscovery' {} a -> s {ipamResourceDiscoveryId = a} :: ModifyIpamResourceDiscovery)

instance Core.AWSRequest ModifyIpamResourceDiscovery where
  type
    AWSResponse ModifyIpamResourceDiscovery =
      ModifyIpamResourceDiscoveryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyIpamResourceDiscoveryResponse'
            Prelude.<$> (x Data..@? "ipamResourceDiscovery")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyIpamResourceDiscovery where
  hashWithSalt _salt ModifyIpamResourceDiscovery' {..} =
    _salt
      `Prelude.hashWithSalt` addOperatingRegions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` removeOperatingRegions
      `Prelude.hashWithSalt` ipamResourceDiscoveryId

instance Prelude.NFData ModifyIpamResourceDiscovery where
  rnf ModifyIpamResourceDiscovery' {..} =
    Prelude.rnf addOperatingRegions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf removeOperatingRegions
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryId

instance Data.ToHeaders ModifyIpamResourceDiscovery where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyIpamResourceDiscovery where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyIpamResourceDiscovery where
  toQuery ModifyIpamResourceDiscovery' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyIpamResourceDiscovery" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AddOperatingRegion"
              Prelude.<$> addOperatingRegions
          ),
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "RemoveOperatingRegion"
              Prelude.<$> removeOperatingRegions
          ),
        "IpamResourceDiscoveryId"
          Data.=: ipamResourceDiscoveryId
      ]

-- | /See:/ 'newModifyIpamResourceDiscoveryResponse' smart constructor.
data ModifyIpamResourceDiscoveryResponse = ModifyIpamResourceDiscoveryResponse'
  { -- | A resource discovery.
    ipamResourceDiscovery :: Prelude.Maybe IpamResourceDiscovery,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpamResourceDiscoveryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamResourceDiscovery', 'modifyIpamResourceDiscoveryResponse_ipamResourceDiscovery' - A resource discovery.
--
-- 'httpStatus', 'modifyIpamResourceDiscoveryResponse_httpStatus' - The response's http status code.
newModifyIpamResourceDiscoveryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyIpamResourceDiscoveryResponse
newModifyIpamResourceDiscoveryResponse pHttpStatus_ =
  ModifyIpamResourceDiscoveryResponse'
    { ipamResourceDiscovery =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A resource discovery.
modifyIpamResourceDiscoveryResponse_ipamResourceDiscovery :: Lens.Lens' ModifyIpamResourceDiscoveryResponse (Prelude.Maybe IpamResourceDiscovery)
modifyIpamResourceDiscoveryResponse_ipamResourceDiscovery = Lens.lens (\ModifyIpamResourceDiscoveryResponse' {ipamResourceDiscovery} -> ipamResourceDiscovery) (\s@ModifyIpamResourceDiscoveryResponse' {} a -> s {ipamResourceDiscovery = a} :: ModifyIpamResourceDiscoveryResponse)

-- | The response's http status code.
modifyIpamResourceDiscoveryResponse_httpStatus :: Lens.Lens' ModifyIpamResourceDiscoveryResponse Prelude.Int
modifyIpamResourceDiscoveryResponse_httpStatus = Lens.lens (\ModifyIpamResourceDiscoveryResponse' {httpStatus} -> httpStatus) (\s@ModifyIpamResourceDiscoveryResponse' {} a -> s {httpStatus = a} :: ModifyIpamResourceDiscoveryResponse)

instance
  Prelude.NFData
    ModifyIpamResourceDiscoveryResponse
  where
  rnf ModifyIpamResourceDiscoveryResponse' {..} =
    Prelude.rnf ipamResourceDiscovery
      `Prelude.seq` Prelude.rnf httpStatus
