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
-- Module      : Amazonka.EC2.ModifyIpamPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the configurations of an IPAM pool.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/mod-pool-ipam.html Modify a pool>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.ModifyIpamPool
  ( -- * Creating a Request
    ModifyIpamPool (..),
    newModifyIpamPool,

    -- * Request Lenses
    modifyIpamPool_addAllocationResourceTags,
    modifyIpamPool_allocationDefaultNetmaskLength,
    modifyIpamPool_allocationMaxNetmaskLength,
    modifyIpamPool_allocationMinNetmaskLength,
    modifyIpamPool_autoImport,
    modifyIpamPool_clearAllocationDefaultNetmaskLength,
    modifyIpamPool_description,
    modifyIpamPool_dryRun,
    modifyIpamPool_removeAllocationResourceTags,
    modifyIpamPool_ipamPoolId,

    -- * Destructuring the Response
    ModifyIpamPoolResponse (..),
    newModifyIpamPoolResponse,

    -- * Response Lenses
    modifyIpamPoolResponse_ipamPool,
    modifyIpamPoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyIpamPool' smart constructor.
data ModifyIpamPool = ModifyIpamPool'
  { -- | Add tag allocation rules to a pool. For more information about
    -- allocation rules, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/create-top-ipam.html Create a top-level pool>
    -- in the /Amazon VPC IPAM User Guide/.
    addAllocationResourceTags :: Prelude.Maybe [RequestIpamResourceTag],
    -- | The default netmask length for allocations added to this pool. If, for
    -- example, the CIDR assigned to this pool is 10.0.0.0\/8 and you enter 16
    -- here, new allocations will default to 10.0.0.0\/16.
    allocationDefaultNetmaskLength :: Prelude.Maybe Prelude.Natural,
    -- | The maximum netmask length possible for CIDR allocations in this IPAM
    -- pool to be compliant. Possible netmask lengths for IPv4 addresses are 0
    -- - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.The
    -- maximum netmask length must be greater than the minimum netmask length.
    allocationMaxNetmaskLength :: Prelude.Maybe Prelude.Natural,
    -- | The minimum netmask length required for CIDR allocations in this IPAM
    -- pool to be compliant. Possible netmask lengths for IPv4 addresses are 0
    -- - 32. Possible netmask lengths for IPv6 addresses are 0 - 128. The
    -- minimum netmask length must be less than the maximum netmask length.
    allocationMinNetmaskLength :: Prelude.Maybe Prelude.Natural,
    -- | If true, IPAM will continuously look for resources within the CIDR range
    -- of this pool and automatically import them as allocations into your
    -- IPAM. The CIDRs that will be allocated for these resources must not
    -- already be allocated to other resources in order for the import to
    -- succeed. IPAM will import a CIDR regardless of its compliance with the
    -- pool\'s allocation rules, so a resource might be imported and
    -- subsequently marked as noncompliant. If IPAM discovers multiple CIDRs
    -- that overlap, IPAM will import the largest CIDR only. If IPAM discovers
    -- multiple CIDRs with matching CIDRs, IPAM will randomly import one of
    -- them only.
    --
    -- A locale must be set on the pool for this feature to work.
    autoImport :: Prelude.Maybe Prelude.Bool,
    -- | Clear the default netmask length allocation rule for this pool.
    clearAllocationDefaultNetmaskLength :: Prelude.Maybe Prelude.Bool,
    -- | The description of the IPAM pool you want to modify.
    description :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Remove tag allocation rules from a pool.
    removeAllocationResourceTags :: Prelude.Maybe [RequestIpamResourceTag],
    -- | The ID of the IPAM pool you want to modify.
    ipamPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpamPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addAllocationResourceTags', 'modifyIpamPool_addAllocationResourceTags' - Add tag allocation rules to a pool. For more information about
-- allocation rules, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-top-ipam.html Create a top-level pool>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'allocationDefaultNetmaskLength', 'modifyIpamPool_allocationDefaultNetmaskLength' - The default netmask length for allocations added to this pool. If, for
-- example, the CIDR assigned to this pool is 10.0.0.0\/8 and you enter 16
-- here, new allocations will default to 10.0.0.0\/16.
--
-- 'allocationMaxNetmaskLength', 'modifyIpamPool_allocationMaxNetmaskLength' - The maximum netmask length possible for CIDR allocations in this IPAM
-- pool to be compliant. Possible netmask lengths for IPv4 addresses are 0
-- - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.The
-- maximum netmask length must be greater than the minimum netmask length.
--
-- 'allocationMinNetmaskLength', 'modifyIpamPool_allocationMinNetmaskLength' - The minimum netmask length required for CIDR allocations in this IPAM
-- pool to be compliant. Possible netmask lengths for IPv4 addresses are 0
-- - 32. Possible netmask lengths for IPv6 addresses are 0 - 128. The
-- minimum netmask length must be less than the maximum netmask length.
--
-- 'autoImport', 'modifyIpamPool_autoImport' - If true, IPAM will continuously look for resources within the CIDR range
-- of this pool and automatically import them as allocations into your
-- IPAM. The CIDRs that will be allocated for these resources must not
-- already be allocated to other resources in order for the import to
-- succeed. IPAM will import a CIDR regardless of its compliance with the
-- pool\'s allocation rules, so a resource might be imported and
-- subsequently marked as noncompliant. If IPAM discovers multiple CIDRs
-- that overlap, IPAM will import the largest CIDR only. If IPAM discovers
-- multiple CIDRs with matching CIDRs, IPAM will randomly import one of
-- them only.
--
-- A locale must be set on the pool for this feature to work.
--
-- 'clearAllocationDefaultNetmaskLength', 'modifyIpamPool_clearAllocationDefaultNetmaskLength' - Clear the default netmask length allocation rule for this pool.
--
-- 'description', 'modifyIpamPool_description' - The description of the IPAM pool you want to modify.
--
-- 'dryRun', 'modifyIpamPool_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'removeAllocationResourceTags', 'modifyIpamPool_removeAllocationResourceTags' - Remove tag allocation rules from a pool.
--
-- 'ipamPoolId', 'modifyIpamPool_ipamPoolId' - The ID of the IPAM pool you want to modify.
newModifyIpamPool ::
  -- | 'ipamPoolId'
  Prelude.Text ->
  ModifyIpamPool
newModifyIpamPool pIpamPoolId_ =
  ModifyIpamPool'
    { addAllocationResourceTags =
        Prelude.Nothing,
      allocationDefaultNetmaskLength = Prelude.Nothing,
      allocationMaxNetmaskLength = Prelude.Nothing,
      allocationMinNetmaskLength = Prelude.Nothing,
      autoImport = Prelude.Nothing,
      clearAllocationDefaultNetmaskLength =
        Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      removeAllocationResourceTags = Prelude.Nothing,
      ipamPoolId = pIpamPoolId_
    }

-- | Add tag allocation rules to a pool. For more information about
-- allocation rules, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-top-ipam.html Create a top-level pool>
-- in the /Amazon VPC IPAM User Guide/.
modifyIpamPool_addAllocationResourceTags :: Lens.Lens' ModifyIpamPool (Prelude.Maybe [RequestIpamResourceTag])
modifyIpamPool_addAllocationResourceTags = Lens.lens (\ModifyIpamPool' {addAllocationResourceTags} -> addAllocationResourceTags) (\s@ModifyIpamPool' {} a -> s {addAllocationResourceTags = a} :: ModifyIpamPool) Prelude.. Lens.mapping Lens.coerced

-- | The default netmask length for allocations added to this pool. If, for
-- example, the CIDR assigned to this pool is 10.0.0.0\/8 and you enter 16
-- here, new allocations will default to 10.0.0.0\/16.
modifyIpamPool_allocationDefaultNetmaskLength :: Lens.Lens' ModifyIpamPool (Prelude.Maybe Prelude.Natural)
modifyIpamPool_allocationDefaultNetmaskLength = Lens.lens (\ModifyIpamPool' {allocationDefaultNetmaskLength} -> allocationDefaultNetmaskLength) (\s@ModifyIpamPool' {} a -> s {allocationDefaultNetmaskLength = a} :: ModifyIpamPool)

-- | The maximum netmask length possible for CIDR allocations in this IPAM
-- pool to be compliant. Possible netmask lengths for IPv4 addresses are 0
-- - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.The
-- maximum netmask length must be greater than the minimum netmask length.
modifyIpamPool_allocationMaxNetmaskLength :: Lens.Lens' ModifyIpamPool (Prelude.Maybe Prelude.Natural)
modifyIpamPool_allocationMaxNetmaskLength = Lens.lens (\ModifyIpamPool' {allocationMaxNetmaskLength} -> allocationMaxNetmaskLength) (\s@ModifyIpamPool' {} a -> s {allocationMaxNetmaskLength = a} :: ModifyIpamPool)

-- | The minimum netmask length required for CIDR allocations in this IPAM
-- pool to be compliant. Possible netmask lengths for IPv4 addresses are 0
-- - 32. Possible netmask lengths for IPv6 addresses are 0 - 128. The
-- minimum netmask length must be less than the maximum netmask length.
modifyIpamPool_allocationMinNetmaskLength :: Lens.Lens' ModifyIpamPool (Prelude.Maybe Prelude.Natural)
modifyIpamPool_allocationMinNetmaskLength = Lens.lens (\ModifyIpamPool' {allocationMinNetmaskLength} -> allocationMinNetmaskLength) (\s@ModifyIpamPool' {} a -> s {allocationMinNetmaskLength = a} :: ModifyIpamPool)

-- | If true, IPAM will continuously look for resources within the CIDR range
-- of this pool and automatically import them as allocations into your
-- IPAM. The CIDRs that will be allocated for these resources must not
-- already be allocated to other resources in order for the import to
-- succeed. IPAM will import a CIDR regardless of its compliance with the
-- pool\'s allocation rules, so a resource might be imported and
-- subsequently marked as noncompliant. If IPAM discovers multiple CIDRs
-- that overlap, IPAM will import the largest CIDR only. If IPAM discovers
-- multiple CIDRs with matching CIDRs, IPAM will randomly import one of
-- them only.
--
-- A locale must be set on the pool for this feature to work.
modifyIpamPool_autoImport :: Lens.Lens' ModifyIpamPool (Prelude.Maybe Prelude.Bool)
modifyIpamPool_autoImport = Lens.lens (\ModifyIpamPool' {autoImport} -> autoImport) (\s@ModifyIpamPool' {} a -> s {autoImport = a} :: ModifyIpamPool)

-- | Clear the default netmask length allocation rule for this pool.
modifyIpamPool_clearAllocationDefaultNetmaskLength :: Lens.Lens' ModifyIpamPool (Prelude.Maybe Prelude.Bool)
modifyIpamPool_clearAllocationDefaultNetmaskLength = Lens.lens (\ModifyIpamPool' {clearAllocationDefaultNetmaskLength} -> clearAllocationDefaultNetmaskLength) (\s@ModifyIpamPool' {} a -> s {clearAllocationDefaultNetmaskLength = a} :: ModifyIpamPool)

-- | The description of the IPAM pool you want to modify.
modifyIpamPool_description :: Lens.Lens' ModifyIpamPool (Prelude.Maybe Prelude.Text)
modifyIpamPool_description = Lens.lens (\ModifyIpamPool' {description} -> description) (\s@ModifyIpamPool' {} a -> s {description = a} :: ModifyIpamPool)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
modifyIpamPool_dryRun :: Lens.Lens' ModifyIpamPool (Prelude.Maybe Prelude.Bool)
modifyIpamPool_dryRun = Lens.lens (\ModifyIpamPool' {dryRun} -> dryRun) (\s@ModifyIpamPool' {} a -> s {dryRun = a} :: ModifyIpamPool)

-- | Remove tag allocation rules from a pool.
modifyIpamPool_removeAllocationResourceTags :: Lens.Lens' ModifyIpamPool (Prelude.Maybe [RequestIpamResourceTag])
modifyIpamPool_removeAllocationResourceTags = Lens.lens (\ModifyIpamPool' {removeAllocationResourceTags} -> removeAllocationResourceTags) (\s@ModifyIpamPool' {} a -> s {removeAllocationResourceTags = a} :: ModifyIpamPool) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the IPAM pool you want to modify.
modifyIpamPool_ipamPoolId :: Lens.Lens' ModifyIpamPool Prelude.Text
modifyIpamPool_ipamPoolId = Lens.lens (\ModifyIpamPool' {ipamPoolId} -> ipamPoolId) (\s@ModifyIpamPool' {} a -> s {ipamPoolId = a} :: ModifyIpamPool)

instance Core.AWSRequest ModifyIpamPool where
  type
    AWSResponse ModifyIpamPool =
      ModifyIpamPoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyIpamPoolResponse'
            Prelude.<$> (x Data..@? "ipamPool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyIpamPool where
  hashWithSalt _salt ModifyIpamPool' {..} =
    _salt
      `Prelude.hashWithSalt` addAllocationResourceTags
      `Prelude.hashWithSalt` allocationDefaultNetmaskLength
      `Prelude.hashWithSalt` allocationMaxNetmaskLength
      `Prelude.hashWithSalt` allocationMinNetmaskLength
      `Prelude.hashWithSalt` autoImport
      `Prelude.hashWithSalt` clearAllocationDefaultNetmaskLength
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` removeAllocationResourceTags
      `Prelude.hashWithSalt` ipamPoolId

instance Prelude.NFData ModifyIpamPool where
  rnf ModifyIpamPool' {..} =
    Prelude.rnf addAllocationResourceTags
      `Prelude.seq` Prelude.rnf allocationDefaultNetmaskLength
      `Prelude.seq` Prelude.rnf allocationMaxNetmaskLength
      `Prelude.seq` Prelude.rnf allocationMinNetmaskLength
      `Prelude.seq` Prelude.rnf autoImport
      `Prelude.seq` Prelude.rnf clearAllocationDefaultNetmaskLength
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf removeAllocationResourceTags
      `Prelude.seq` Prelude.rnf ipamPoolId

instance Data.ToHeaders ModifyIpamPool where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyIpamPool where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyIpamPool where
  toQuery ModifyIpamPool' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyIpamPool" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AddAllocationResourceTag"
              Prelude.<$> addAllocationResourceTags
          ),
        "AllocationDefaultNetmaskLength"
          Data.=: allocationDefaultNetmaskLength,
        "AllocationMaxNetmaskLength"
          Data.=: allocationMaxNetmaskLength,
        "AllocationMinNetmaskLength"
          Data.=: allocationMinNetmaskLength,
        "AutoImport" Data.=: autoImport,
        "ClearAllocationDefaultNetmaskLength"
          Data.=: clearAllocationDefaultNetmaskLength,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "RemoveAllocationResourceTag"
              Prelude.<$> removeAllocationResourceTags
          ),
        "IpamPoolId" Data.=: ipamPoolId
      ]

-- | /See:/ 'newModifyIpamPoolResponse' smart constructor.
data ModifyIpamPoolResponse = ModifyIpamPoolResponse'
  { -- | The results of the modification.
    ipamPool :: Prelude.Maybe IpamPool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIpamPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamPool', 'modifyIpamPoolResponse_ipamPool' - The results of the modification.
--
-- 'httpStatus', 'modifyIpamPoolResponse_httpStatus' - The response's http status code.
newModifyIpamPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyIpamPoolResponse
newModifyIpamPoolResponse pHttpStatus_ =
  ModifyIpamPoolResponse'
    { ipamPool = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The results of the modification.
modifyIpamPoolResponse_ipamPool :: Lens.Lens' ModifyIpamPoolResponse (Prelude.Maybe IpamPool)
modifyIpamPoolResponse_ipamPool = Lens.lens (\ModifyIpamPoolResponse' {ipamPool} -> ipamPool) (\s@ModifyIpamPoolResponse' {} a -> s {ipamPool = a} :: ModifyIpamPoolResponse)

-- | The response's http status code.
modifyIpamPoolResponse_httpStatus :: Lens.Lens' ModifyIpamPoolResponse Prelude.Int
modifyIpamPoolResponse_httpStatus = Lens.lens (\ModifyIpamPoolResponse' {httpStatus} -> httpStatus) (\s@ModifyIpamPoolResponse' {} a -> s {httpStatus = a} :: ModifyIpamPoolResponse)

instance Prelude.NFData ModifyIpamPoolResponse where
  rnf ModifyIpamPoolResponse' {..} =
    Prelude.rnf ipamPool
      `Prelude.seq` Prelude.rnf httpStatus
