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
-- Module      : Amazonka.EC2.ModifyIpam
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the configurations of an IPAM.
module Amazonka.EC2.ModifyIpam
  ( -- * Creating a Request
    ModifyIpam (..),
    newModifyIpam,

    -- * Request Lenses
    modifyIpam_addOperatingRegions,
    modifyIpam_description,
    modifyIpam_dryRun,
    modifyIpam_removeOperatingRegions,
    modifyIpam_ipamId,

    -- * Destructuring the Response
    ModifyIpamResponse (..),
    newModifyIpamResponse,

    -- * Response Lenses
    modifyIpamResponse_ipam,
    modifyIpamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyIpam' smart constructor.
data ModifyIpam = ModifyIpam'
  { -- | Choose the operating Regions for the IPAM. Operating Regions are Amazon
    -- Web Services Regions where the IPAM is allowed to manage IP address
    -- CIDRs. IPAM only discovers and monitors resources in the Amazon Web
    -- Services Regions you select as operating Regions.
    --
    -- For more information about operating Regions, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
    -- in the /Amazon VPC IPAM User Guide/.
    addOperatingRegions :: Prelude.Maybe [AddIpamOperatingRegion],
    -- | The description of the IPAM you want to modify.
    description :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The operating Regions to remove.
    removeOperatingRegions :: Prelude.Maybe [RemoveIpamOperatingRegion],
    -- | The ID of the IPAM you want to modify.
    ipamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addOperatingRegions', 'modifyIpam_addOperatingRegions' - Choose the operating Regions for the IPAM. Operating Regions are Amazon
-- Web Services Regions where the IPAM is allowed to manage IP address
-- CIDRs. IPAM only discovers and monitors resources in the Amazon Web
-- Services Regions you select as operating Regions.
--
-- For more information about operating Regions, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'description', 'modifyIpam_description' - The description of the IPAM you want to modify.
--
-- 'dryRun', 'modifyIpam_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'removeOperatingRegions', 'modifyIpam_removeOperatingRegions' - The operating Regions to remove.
--
-- 'ipamId', 'modifyIpam_ipamId' - The ID of the IPAM you want to modify.
newModifyIpam ::
  -- | 'ipamId'
  Prelude.Text ->
  ModifyIpam
newModifyIpam pIpamId_ =
  ModifyIpam'
    { addOperatingRegions = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      removeOperatingRegions = Prelude.Nothing,
      ipamId = pIpamId_
    }

-- | Choose the operating Regions for the IPAM. Operating Regions are Amazon
-- Web Services Regions where the IPAM is allowed to manage IP address
-- CIDRs. IPAM only discovers and monitors resources in the Amazon Web
-- Services Regions you select as operating Regions.
--
-- For more information about operating Regions, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-ipam.html Create an IPAM>
-- in the /Amazon VPC IPAM User Guide/.
modifyIpam_addOperatingRegions :: Lens.Lens' ModifyIpam (Prelude.Maybe [AddIpamOperatingRegion])
modifyIpam_addOperatingRegions = Lens.lens (\ModifyIpam' {addOperatingRegions} -> addOperatingRegions) (\s@ModifyIpam' {} a -> s {addOperatingRegions = a} :: ModifyIpam) Prelude.. Lens.mapping Lens.coerced

-- | The description of the IPAM you want to modify.
modifyIpam_description :: Lens.Lens' ModifyIpam (Prelude.Maybe Prelude.Text)
modifyIpam_description = Lens.lens (\ModifyIpam' {description} -> description) (\s@ModifyIpam' {} a -> s {description = a} :: ModifyIpam)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
modifyIpam_dryRun :: Lens.Lens' ModifyIpam (Prelude.Maybe Prelude.Bool)
modifyIpam_dryRun = Lens.lens (\ModifyIpam' {dryRun} -> dryRun) (\s@ModifyIpam' {} a -> s {dryRun = a} :: ModifyIpam)

-- | The operating Regions to remove.
modifyIpam_removeOperatingRegions :: Lens.Lens' ModifyIpam (Prelude.Maybe [RemoveIpamOperatingRegion])
modifyIpam_removeOperatingRegions = Lens.lens (\ModifyIpam' {removeOperatingRegions} -> removeOperatingRegions) (\s@ModifyIpam' {} a -> s {removeOperatingRegions = a} :: ModifyIpam) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the IPAM you want to modify.
modifyIpam_ipamId :: Lens.Lens' ModifyIpam Prelude.Text
modifyIpam_ipamId = Lens.lens (\ModifyIpam' {ipamId} -> ipamId) (\s@ModifyIpam' {} a -> s {ipamId = a} :: ModifyIpam)

instance Core.AWSRequest ModifyIpam where
  type AWSResponse ModifyIpam = ModifyIpamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyIpamResponse'
            Prelude.<$> (x Data..@? "ipam")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyIpam where
  hashWithSalt _salt ModifyIpam' {..} =
    _salt `Prelude.hashWithSalt` addOperatingRegions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` removeOperatingRegions
      `Prelude.hashWithSalt` ipamId

instance Prelude.NFData ModifyIpam where
  rnf ModifyIpam' {..} =
    Prelude.rnf addOperatingRegions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf removeOperatingRegions
      `Prelude.seq` Prelude.rnf ipamId

instance Data.ToHeaders ModifyIpam where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyIpam where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyIpam where
  toQuery ModifyIpam' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyIpam" :: Prelude.ByteString),
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
        "IpamId" Data.=: ipamId
      ]

-- | /See:/ 'newModifyIpamResponse' smart constructor.
data ModifyIpamResponse = ModifyIpamResponse'
  { -- | The results of the modification.
    ipam :: Prelude.Maybe Ipam,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipam', 'modifyIpamResponse_ipam' - The results of the modification.
--
-- 'httpStatus', 'modifyIpamResponse_httpStatus' - The response's http status code.
newModifyIpamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyIpamResponse
newModifyIpamResponse pHttpStatus_ =
  ModifyIpamResponse'
    { ipam = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The results of the modification.
modifyIpamResponse_ipam :: Lens.Lens' ModifyIpamResponse (Prelude.Maybe Ipam)
modifyIpamResponse_ipam = Lens.lens (\ModifyIpamResponse' {ipam} -> ipam) (\s@ModifyIpamResponse' {} a -> s {ipam = a} :: ModifyIpamResponse)

-- | The response's http status code.
modifyIpamResponse_httpStatus :: Lens.Lens' ModifyIpamResponse Prelude.Int
modifyIpamResponse_httpStatus = Lens.lens (\ModifyIpamResponse' {httpStatus} -> httpStatus) (\s@ModifyIpamResponse' {} a -> s {httpStatus = a} :: ModifyIpamResponse)

instance Prelude.NFData ModifyIpamResponse where
  rnf ModifyIpamResponse' {..} =
    Prelude.rnf ipam
      `Prelude.seq` Prelude.rnf httpStatus
