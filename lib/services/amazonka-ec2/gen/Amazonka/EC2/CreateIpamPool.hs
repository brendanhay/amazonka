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
-- Module      : Amazonka.EC2.CreateIpamPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an IP address pool for Amazon VPC IP Address Manager (IPAM). In
-- IPAM, a pool is a collection of contiguous IP addresses CIDRs. Pools
-- enable you to organize your IP addresses according to your routing and
-- security needs. For example, if you have separate routing and security
-- needs for development and production applications, you can create a pool
-- for each.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/create-top-ipam.html Create a top-level pool>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.CreateIpamPool
  ( -- * Creating a Request
    CreateIpamPool (..),
    newCreateIpamPool,

    -- * Request Lenses
    createIpamPool_allocationDefaultNetmaskLength,
    createIpamPool_allocationMaxNetmaskLength,
    createIpamPool_allocationMinNetmaskLength,
    createIpamPool_allocationResourceTags,
    createIpamPool_autoImport,
    createIpamPool_awsService,
    createIpamPool_clientToken,
    createIpamPool_description,
    createIpamPool_dryRun,
    createIpamPool_locale,
    createIpamPool_publicIpSource,
    createIpamPool_publiclyAdvertisable,
    createIpamPool_sourceIpamPoolId,
    createIpamPool_tagSpecifications,
    createIpamPool_ipamScopeId,
    createIpamPool_addressFamily,

    -- * Destructuring the Response
    CreateIpamPoolResponse (..),
    newCreateIpamPoolResponse,

    -- * Response Lenses
    createIpamPoolResponse_ipamPool,
    createIpamPoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIpamPool' smart constructor.
data CreateIpamPool = CreateIpamPool'
  { -- | The default netmask length for allocations added to this pool. If, for
    -- example, the CIDR assigned to this pool is 10.0.0.0\/8 and you enter 16
    -- here, new allocations will default to 10.0.0.0\/16.
    allocationDefaultNetmaskLength :: Prelude.Maybe Prelude.Natural,
    -- | The maximum netmask length possible for CIDR allocations in this IPAM
    -- pool to be compliant. The maximum netmask length must be greater than
    -- the minimum netmask length. Possible netmask lengths for IPv4 addresses
    -- are 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
    allocationMaxNetmaskLength :: Prelude.Maybe Prelude.Natural,
    -- | The minimum netmask length required for CIDR allocations in this IPAM
    -- pool to be compliant. The minimum netmask length must be less than the
    -- maximum netmask length. Possible netmask lengths for IPv4 addresses are
    -- 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
    allocationMinNetmaskLength :: Prelude.Maybe Prelude.Natural,
    -- | Tags that are required for resources that use CIDRs from this IPAM pool.
    -- Resources that do not have these tags will not be allowed to allocate
    -- space from the pool. If the resources have their tags changed after they
    -- have allocated space or if the allocation tagging requirements are
    -- changed on the pool, the resource may be marked as noncompliant.
    allocationResourceTags :: Prelude.Maybe [RequestIpamResourceTag],
    -- | If selected, IPAM will continuously look for resources within the CIDR
    -- range of this pool and automatically import them as allocations into
    -- your IPAM. The CIDRs that will be allocated for these resources must not
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
    -- | Limits which service in Amazon Web Services that the pool can be used
    -- in. \"ec2\", for example, allows users to use space for Elastic IP
    -- addresses and VPCs.
    awsService :: Prelude.Maybe IpamPoolAwsService,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the IPAM pool.
    description :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | In IPAM, the locale is the Amazon Web Services Region where you want to
    -- make an IPAM pool available for allocations. Only resources in the same
    -- Region as the locale of the pool can get IP address allocations from the
    -- pool. You can only allocate a CIDR for a VPC, for example, from an IPAM
    -- pool that shares a locale with the VPC’s Region. Note that once you
    -- choose a Locale for a pool, you cannot modify it. If you do not choose a
    -- locale, resources in Regions others than the IPAM\'s home region cannot
    -- use CIDRs from this pool.
    --
    -- Possible values: Any Amazon Web Services Region, such as us-east-1.
    locale :: Prelude.Maybe Prelude.Text,
    -- | The IP address source for pools in the public scope. Only used for
    -- provisioning IP address CIDRs to pools in the public scope. Default is
    -- @byoip@. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/intro-create-ipv6-pools.html Create IPv6 pools>
    -- in the /Amazon VPC IPAM User Guide/. By default, you can add only one
    -- Amazon-provided IPv6 CIDR block to a top-level IPv6 pool if
    -- PublicIpSource is @amazon@. For information on increasing the default
    -- limit, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/quotas-ipam.html Quotas for your IPAM>
    -- in the /Amazon VPC IPAM User Guide/.
    publicIpSource :: Prelude.Maybe IpamPoolPublicIpSource,
    -- | Determines if the pool is publicly advertisable. This option is not
    -- available for pools with AddressFamily set to @ipv4@.
    publiclyAdvertisable :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the source IPAM pool. Use this option to create a pool within
    -- an existing pool. Note that the CIDR you provision for the pool within
    -- the source pool must be available in the source pool\'s CIDR range.
    sourceIpamPoolId :: Prelude.Maybe Prelude.Text,
    -- | The key\/value combination of a tag assigned to the resource. Use the
    -- tag key in the filter name and the tag value as the filter value. For
    -- example, to find all resources that have a tag with the key @Owner@ and
    -- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
    -- for the filter value.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the scope in which you would like to create the IPAM pool.
    ipamScopeId :: Prelude.Text,
    -- | The IP protocol assigned to this IPAM pool. You must choose either IPv4
    -- or IPv6 protocol for a pool.
    addressFamily :: AddressFamily
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpamPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationDefaultNetmaskLength', 'createIpamPool_allocationDefaultNetmaskLength' - The default netmask length for allocations added to this pool. If, for
-- example, the CIDR assigned to this pool is 10.0.0.0\/8 and you enter 16
-- here, new allocations will default to 10.0.0.0\/16.
--
-- 'allocationMaxNetmaskLength', 'createIpamPool_allocationMaxNetmaskLength' - The maximum netmask length possible for CIDR allocations in this IPAM
-- pool to be compliant. The maximum netmask length must be greater than
-- the minimum netmask length. Possible netmask lengths for IPv4 addresses
-- are 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
--
-- 'allocationMinNetmaskLength', 'createIpamPool_allocationMinNetmaskLength' - The minimum netmask length required for CIDR allocations in this IPAM
-- pool to be compliant. The minimum netmask length must be less than the
-- maximum netmask length. Possible netmask lengths for IPv4 addresses are
-- 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
--
-- 'allocationResourceTags', 'createIpamPool_allocationResourceTags' - Tags that are required for resources that use CIDRs from this IPAM pool.
-- Resources that do not have these tags will not be allowed to allocate
-- space from the pool. If the resources have their tags changed after they
-- have allocated space or if the allocation tagging requirements are
-- changed on the pool, the resource may be marked as noncompliant.
--
-- 'autoImport', 'createIpamPool_autoImport' - If selected, IPAM will continuously look for resources within the CIDR
-- range of this pool and automatically import them as allocations into
-- your IPAM. The CIDRs that will be allocated for these resources must not
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
-- 'awsService', 'createIpamPool_awsService' - Limits which service in Amazon Web Services that the pool can be used
-- in. \"ec2\", for example, allows users to use space for Elastic IP
-- addresses and VPCs.
--
-- 'clientToken', 'createIpamPool_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'createIpamPool_description' - A description for the IPAM pool.
--
-- 'dryRun', 'createIpamPool_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'locale', 'createIpamPool_locale' - In IPAM, the locale is the Amazon Web Services Region where you want to
-- make an IPAM pool available for allocations. Only resources in the same
-- Region as the locale of the pool can get IP address allocations from the
-- pool. You can only allocate a CIDR for a VPC, for example, from an IPAM
-- pool that shares a locale with the VPC’s Region. Note that once you
-- choose a Locale for a pool, you cannot modify it. If you do not choose a
-- locale, resources in Regions others than the IPAM\'s home region cannot
-- use CIDRs from this pool.
--
-- Possible values: Any Amazon Web Services Region, such as us-east-1.
--
-- 'publicIpSource', 'createIpamPool_publicIpSource' - The IP address source for pools in the public scope. Only used for
-- provisioning IP address CIDRs to pools in the public scope. Default is
-- @byoip@. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/intro-create-ipv6-pools.html Create IPv6 pools>
-- in the /Amazon VPC IPAM User Guide/. By default, you can add only one
-- Amazon-provided IPv6 CIDR block to a top-level IPv6 pool if
-- PublicIpSource is @amazon@. For information on increasing the default
-- limit, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/quotas-ipam.html Quotas for your IPAM>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'publiclyAdvertisable', 'createIpamPool_publiclyAdvertisable' - Determines if the pool is publicly advertisable. This option is not
-- available for pools with AddressFamily set to @ipv4@.
--
-- 'sourceIpamPoolId', 'createIpamPool_sourceIpamPoolId' - The ID of the source IPAM pool. Use this option to create a pool within
-- an existing pool. Note that the CIDR you provision for the pool within
-- the source pool must be available in the source pool\'s CIDR range.
--
-- 'tagSpecifications', 'createIpamPool_tagSpecifications' - The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
--
-- 'ipamScopeId', 'createIpamPool_ipamScopeId' - The ID of the scope in which you would like to create the IPAM pool.
--
-- 'addressFamily', 'createIpamPool_addressFamily' - The IP protocol assigned to this IPAM pool. You must choose either IPv4
-- or IPv6 protocol for a pool.
newCreateIpamPool ::
  -- | 'ipamScopeId'
  Prelude.Text ->
  -- | 'addressFamily'
  AddressFamily ->
  CreateIpamPool
newCreateIpamPool pIpamScopeId_ pAddressFamily_ =
  CreateIpamPool'
    { allocationDefaultNetmaskLength =
        Prelude.Nothing,
      allocationMaxNetmaskLength = Prelude.Nothing,
      allocationMinNetmaskLength = Prelude.Nothing,
      allocationResourceTags = Prelude.Nothing,
      autoImport = Prelude.Nothing,
      awsService = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      locale = Prelude.Nothing,
      publicIpSource = Prelude.Nothing,
      publiclyAdvertisable = Prelude.Nothing,
      sourceIpamPoolId = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      ipamScopeId = pIpamScopeId_,
      addressFamily = pAddressFamily_
    }

-- | The default netmask length for allocations added to this pool. If, for
-- example, the CIDR assigned to this pool is 10.0.0.0\/8 and you enter 16
-- here, new allocations will default to 10.0.0.0\/16.
createIpamPool_allocationDefaultNetmaskLength :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Natural)
createIpamPool_allocationDefaultNetmaskLength = Lens.lens (\CreateIpamPool' {allocationDefaultNetmaskLength} -> allocationDefaultNetmaskLength) (\s@CreateIpamPool' {} a -> s {allocationDefaultNetmaskLength = a} :: CreateIpamPool)

-- | The maximum netmask length possible for CIDR allocations in this IPAM
-- pool to be compliant. The maximum netmask length must be greater than
-- the minimum netmask length. Possible netmask lengths for IPv4 addresses
-- are 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
createIpamPool_allocationMaxNetmaskLength :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Natural)
createIpamPool_allocationMaxNetmaskLength = Lens.lens (\CreateIpamPool' {allocationMaxNetmaskLength} -> allocationMaxNetmaskLength) (\s@CreateIpamPool' {} a -> s {allocationMaxNetmaskLength = a} :: CreateIpamPool)

-- | The minimum netmask length required for CIDR allocations in this IPAM
-- pool to be compliant. The minimum netmask length must be less than the
-- maximum netmask length. Possible netmask lengths for IPv4 addresses are
-- 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
createIpamPool_allocationMinNetmaskLength :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Natural)
createIpamPool_allocationMinNetmaskLength = Lens.lens (\CreateIpamPool' {allocationMinNetmaskLength} -> allocationMinNetmaskLength) (\s@CreateIpamPool' {} a -> s {allocationMinNetmaskLength = a} :: CreateIpamPool)

-- | Tags that are required for resources that use CIDRs from this IPAM pool.
-- Resources that do not have these tags will not be allowed to allocate
-- space from the pool. If the resources have their tags changed after they
-- have allocated space or if the allocation tagging requirements are
-- changed on the pool, the resource may be marked as noncompliant.
createIpamPool_allocationResourceTags :: Lens.Lens' CreateIpamPool (Prelude.Maybe [RequestIpamResourceTag])
createIpamPool_allocationResourceTags = Lens.lens (\CreateIpamPool' {allocationResourceTags} -> allocationResourceTags) (\s@CreateIpamPool' {} a -> s {allocationResourceTags = a} :: CreateIpamPool) Prelude.. Lens.mapping Lens.coerced

-- | If selected, IPAM will continuously look for resources within the CIDR
-- range of this pool and automatically import them as allocations into
-- your IPAM. The CIDRs that will be allocated for these resources must not
-- already be allocated to other resources in order for the import to
-- succeed. IPAM will import a CIDR regardless of its compliance with the
-- pool\'s allocation rules, so a resource might be imported and
-- subsequently marked as noncompliant. If IPAM discovers multiple CIDRs
-- that overlap, IPAM will import the largest CIDR only. If IPAM discovers
-- multiple CIDRs with matching CIDRs, IPAM will randomly import one of
-- them only.
--
-- A locale must be set on the pool for this feature to work.
createIpamPool_autoImport :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Bool)
createIpamPool_autoImport = Lens.lens (\CreateIpamPool' {autoImport} -> autoImport) (\s@CreateIpamPool' {} a -> s {autoImport = a} :: CreateIpamPool)

-- | Limits which service in Amazon Web Services that the pool can be used
-- in. \"ec2\", for example, allows users to use space for Elastic IP
-- addresses and VPCs.
createIpamPool_awsService :: Lens.Lens' CreateIpamPool (Prelude.Maybe IpamPoolAwsService)
createIpamPool_awsService = Lens.lens (\CreateIpamPool' {awsService} -> awsService) (\s@CreateIpamPool' {} a -> s {awsService = a} :: CreateIpamPool)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
createIpamPool_clientToken :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Text)
createIpamPool_clientToken = Lens.lens (\CreateIpamPool' {clientToken} -> clientToken) (\s@CreateIpamPool' {} a -> s {clientToken = a} :: CreateIpamPool)

-- | A description for the IPAM pool.
createIpamPool_description :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Text)
createIpamPool_description = Lens.lens (\CreateIpamPool' {description} -> description) (\s@CreateIpamPool' {} a -> s {description = a} :: CreateIpamPool)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
createIpamPool_dryRun :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Bool)
createIpamPool_dryRun = Lens.lens (\CreateIpamPool' {dryRun} -> dryRun) (\s@CreateIpamPool' {} a -> s {dryRun = a} :: CreateIpamPool)

-- | In IPAM, the locale is the Amazon Web Services Region where you want to
-- make an IPAM pool available for allocations. Only resources in the same
-- Region as the locale of the pool can get IP address allocations from the
-- pool. You can only allocate a CIDR for a VPC, for example, from an IPAM
-- pool that shares a locale with the VPC’s Region. Note that once you
-- choose a Locale for a pool, you cannot modify it. If you do not choose a
-- locale, resources in Regions others than the IPAM\'s home region cannot
-- use CIDRs from this pool.
--
-- Possible values: Any Amazon Web Services Region, such as us-east-1.
createIpamPool_locale :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Text)
createIpamPool_locale = Lens.lens (\CreateIpamPool' {locale} -> locale) (\s@CreateIpamPool' {} a -> s {locale = a} :: CreateIpamPool)

-- | The IP address source for pools in the public scope. Only used for
-- provisioning IP address CIDRs to pools in the public scope. Default is
-- @byoip@. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/intro-create-ipv6-pools.html Create IPv6 pools>
-- in the /Amazon VPC IPAM User Guide/. By default, you can add only one
-- Amazon-provided IPv6 CIDR block to a top-level IPv6 pool if
-- PublicIpSource is @amazon@. For information on increasing the default
-- limit, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/quotas-ipam.html Quotas for your IPAM>
-- in the /Amazon VPC IPAM User Guide/.
createIpamPool_publicIpSource :: Lens.Lens' CreateIpamPool (Prelude.Maybe IpamPoolPublicIpSource)
createIpamPool_publicIpSource = Lens.lens (\CreateIpamPool' {publicIpSource} -> publicIpSource) (\s@CreateIpamPool' {} a -> s {publicIpSource = a} :: CreateIpamPool)

-- | Determines if the pool is publicly advertisable. This option is not
-- available for pools with AddressFamily set to @ipv4@.
createIpamPool_publiclyAdvertisable :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Bool)
createIpamPool_publiclyAdvertisable = Lens.lens (\CreateIpamPool' {publiclyAdvertisable} -> publiclyAdvertisable) (\s@CreateIpamPool' {} a -> s {publiclyAdvertisable = a} :: CreateIpamPool)

-- | The ID of the source IPAM pool. Use this option to create a pool within
-- an existing pool. Note that the CIDR you provision for the pool within
-- the source pool must be available in the source pool\'s CIDR range.
createIpamPool_sourceIpamPoolId :: Lens.Lens' CreateIpamPool (Prelude.Maybe Prelude.Text)
createIpamPool_sourceIpamPoolId = Lens.lens (\CreateIpamPool' {sourceIpamPoolId} -> sourceIpamPoolId) (\s@CreateIpamPool' {} a -> s {sourceIpamPoolId = a} :: CreateIpamPool)

-- | The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
createIpamPool_tagSpecifications :: Lens.Lens' CreateIpamPool (Prelude.Maybe [TagSpecification])
createIpamPool_tagSpecifications = Lens.lens (\CreateIpamPool' {tagSpecifications} -> tagSpecifications) (\s@CreateIpamPool' {} a -> s {tagSpecifications = a} :: CreateIpamPool) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the scope in which you would like to create the IPAM pool.
createIpamPool_ipamScopeId :: Lens.Lens' CreateIpamPool Prelude.Text
createIpamPool_ipamScopeId = Lens.lens (\CreateIpamPool' {ipamScopeId} -> ipamScopeId) (\s@CreateIpamPool' {} a -> s {ipamScopeId = a} :: CreateIpamPool)

-- | The IP protocol assigned to this IPAM pool. You must choose either IPv4
-- or IPv6 protocol for a pool.
createIpamPool_addressFamily :: Lens.Lens' CreateIpamPool AddressFamily
createIpamPool_addressFamily = Lens.lens (\CreateIpamPool' {addressFamily} -> addressFamily) (\s@CreateIpamPool' {} a -> s {addressFamily = a} :: CreateIpamPool)

instance Core.AWSRequest CreateIpamPool where
  type
    AWSResponse CreateIpamPool =
      CreateIpamPoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateIpamPoolResponse'
            Prelude.<$> (x Data..@? "ipamPool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIpamPool where
  hashWithSalt _salt CreateIpamPool' {..} =
    _salt
      `Prelude.hashWithSalt` allocationDefaultNetmaskLength
      `Prelude.hashWithSalt` allocationMaxNetmaskLength
      `Prelude.hashWithSalt` allocationMinNetmaskLength
      `Prelude.hashWithSalt` allocationResourceTags
      `Prelude.hashWithSalt` autoImport
      `Prelude.hashWithSalt` awsService
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` publicIpSource
      `Prelude.hashWithSalt` publiclyAdvertisable
      `Prelude.hashWithSalt` sourceIpamPoolId
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` ipamScopeId
      `Prelude.hashWithSalt` addressFamily

instance Prelude.NFData CreateIpamPool where
  rnf CreateIpamPool' {..} =
    Prelude.rnf allocationDefaultNetmaskLength
      `Prelude.seq` Prelude.rnf allocationMaxNetmaskLength
      `Prelude.seq` Prelude.rnf allocationMinNetmaskLength
      `Prelude.seq` Prelude.rnf allocationResourceTags
      `Prelude.seq` Prelude.rnf autoImport
      `Prelude.seq` Prelude.rnf awsService
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf publicIpSource
      `Prelude.seq` Prelude.rnf publiclyAdvertisable
      `Prelude.seq` Prelude.rnf sourceIpamPoolId
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf ipamScopeId
      `Prelude.seq` Prelude.rnf addressFamily

instance Data.ToHeaders CreateIpamPool where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateIpamPool where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateIpamPool where
  toQuery CreateIpamPool' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateIpamPool" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AllocationDefaultNetmaskLength"
          Data.=: allocationDefaultNetmaskLength,
        "AllocationMaxNetmaskLength"
          Data.=: allocationMaxNetmaskLength,
        "AllocationMinNetmaskLength"
          Data.=: allocationMinNetmaskLength,
        Data.toQuery
          ( Data.toQueryList "AllocationResourceTag"
              Prelude.<$> allocationResourceTags
          ),
        "AutoImport" Data.=: autoImport,
        "AwsService" Data.=: awsService,
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "Locale" Data.=: locale,
        "PublicIpSource" Data.=: publicIpSource,
        "PubliclyAdvertisable" Data.=: publiclyAdvertisable,
        "SourceIpamPoolId" Data.=: sourceIpamPoolId,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "IpamScopeId" Data.=: ipamScopeId,
        "AddressFamily" Data.=: addressFamily
      ]

-- | /See:/ 'newCreateIpamPoolResponse' smart constructor.
data CreateIpamPoolResponse = CreateIpamPoolResponse'
  { -- | Information about the IPAM pool created.
    ipamPool :: Prelude.Maybe IpamPool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpamPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamPool', 'createIpamPoolResponse_ipamPool' - Information about the IPAM pool created.
--
-- 'httpStatus', 'createIpamPoolResponse_httpStatus' - The response's http status code.
newCreateIpamPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIpamPoolResponse
newCreateIpamPoolResponse pHttpStatus_ =
  CreateIpamPoolResponse'
    { ipamPool = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPAM pool created.
createIpamPoolResponse_ipamPool :: Lens.Lens' CreateIpamPoolResponse (Prelude.Maybe IpamPool)
createIpamPoolResponse_ipamPool = Lens.lens (\CreateIpamPoolResponse' {ipamPool} -> ipamPool) (\s@CreateIpamPoolResponse' {} a -> s {ipamPool = a} :: CreateIpamPoolResponse)

-- | The response's http status code.
createIpamPoolResponse_httpStatus :: Lens.Lens' CreateIpamPoolResponse Prelude.Int
createIpamPoolResponse_httpStatus = Lens.lens (\CreateIpamPoolResponse' {httpStatus} -> httpStatus) (\s@CreateIpamPoolResponse' {} a -> s {httpStatus = a} :: CreateIpamPoolResponse)

instance Prelude.NFData CreateIpamPoolResponse where
  rnf CreateIpamPoolResponse' {..} =
    Prelude.rnf ipamPool
      `Prelude.seq` Prelude.rnf httpStatus
