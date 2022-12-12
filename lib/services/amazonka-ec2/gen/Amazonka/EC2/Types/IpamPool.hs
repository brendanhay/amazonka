{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.IpamPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AddressFamily
import Amazonka.EC2.Types.IpamPoolAwsService
import Amazonka.EC2.Types.IpamPoolState
import Amazonka.EC2.Types.IpamResourceTag
import Amazonka.EC2.Types.IpamScopeType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | In IPAM, a pool is a collection of contiguous IP addresses CIDRs. Pools
-- enable you to organize your IP addresses according to your routing and
-- security needs. For example, if you have separate routing and security
-- needs for development and production applications, you can create a pool
-- for each.
--
-- /See:/ 'newIpamPool' smart constructor.
data IpamPool = IpamPool'
  { -- | The address family of the pool.
    addressFamily :: Prelude.Maybe AddressFamily,
    -- | The default netmask length for allocations added to this pool. If, for
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
    allocationResourceTags :: Prelude.Maybe [IpamResourceTag],
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
    -- | The description of the IPAM pool.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IPAM.
    ipamArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IPAM pool.
    ipamPoolArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IPAM pool.
    ipamPoolId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the IPAM pool.
    ipamRegion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the scope of the IPAM pool.
    ipamScopeArn :: Prelude.Maybe Prelude.Text,
    -- | In IPAM, a scope is the highest-level container within IPAM. An IPAM
    -- contains two default scopes. Each scope represents the IP space for a
    -- single network. The private scope is intended for all private IP address
    -- space. The public scope is intended for all public IP address space.
    -- Scopes enable you to reuse IP addresses across multiple unconnected
    -- networks without causing IP address overlap or conflict.
    ipamScopeType :: Prelude.Maybe IpamScopeType,
    -- | The locale of the IPAM pool. In IPAM, the locale is the Amazon Web
    -- Services Region where you want to make an IPAM pool available for
    -- allocations. Only resources in the same Region as the locale of the pool
    -- can get IP address allocations from the pool. You can only allocate a
    -- CIDR for a VPC, for example, from an IPAM pool that shares a locale with
    -- the VPC’s Region. Note that once you choose a Locale for a pool, you
    -- cannot modify it. If you choose an Amazon Web Services Region for locale
    -- that has not been configured as an operating Region for the IPAM,
    -- you\'ll get an error.
    locale :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the IPAM pool.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The depth of pools in your IPAM pool. The pool depth quota is 10. For
    -- more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/ipam/quotas-ipam.html Quotas in IPAM>
    -- in the /Amazon VPC IPAM User Guide/.
    poolDepth :: Prelude.Maybe Prelude.Int,
    -- | Determines if a pool is publicly advertisable. This option is not
    -- available for pools with AddressFamily set to @ipv4@.
    publiclyAdvertisable :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the source IPAM pool. You can use this option to create an
    -- IPAM pool within an existing source pool.
    sourceIpamPoolId :: Prelude.Maybe Prelude.Text,
    -- | The state of the IPAM pool.
    state :: Prelude.Maybe IpamPoolState,
    -- | A message related to the failed creation of an IPAM pool.
    stateMessage :: Prelude.Maybe Prelude.Text,
    -- | The key\/value combination of a tag assigned to the resource. Use the
    -- tag key in the filter name and the tag value as the filter value. For
    -- example, to find all resources that have a tag with the key @Owner@ and
    -- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
    -- for the filter value.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressFamily', 'ipamPool_addressFamily' - The address family of the pool.
--
-- 'allocationDefaultNetmaskLength', 'ipamPool_allocationDefaultNetmaskLength' - The default netmask length for allocations added to this pool. If, for
-- example, the CIDR assigned to this pool is 10.0.0.0\/8 and you enter 16
-- here, new allocations will default to 10.0.0.0\/16.
--
-- 'allocationMaxNetmaskLength', 'ipamPool_allocationMaxNetmaskLength' - The maximum netmask length possible for CIDR allocations in this IPAM
-- pool to be compliant. The maximum netmask length must be greater than
-- the minimum netmask length. Possible netmask lengths for IPv4 addresses
-- are 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
--
-- 'allocationMinNetmaskLength', 'ipamPool_allocationMinNetmaskLength' - The minimum netmask length required for CIDR allocations in this IPAM
-- pool to be compliant. The minimum netmask length must be less than the
-- maximum netmask length. Possible netmask lengths for IPv4 addresses are
-- 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
--
-- 'allocationResourceTags', 'ipamPool_allocationResourceTags' - Tags that are required for resources that use CIDRs from this IPAM pool.
-- Resources that do not have these tags will not be allowed to allocate
-- space from the pool. If the resources have their tags changed after they
-- have allocated space or if the allocation tagging requirements are
-- changed on the pool, the resource may be marked as noncompliant.
--
-- 'autoImport', 'ipamPool_autoImport' - If selected, IPAM will continuously look for resources within the CIDR
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
-- 'awsService', 'ipamPool_awsService' - Limits which service in Amazon Web Services that the pool can be used
-- in. \"ec2\", for example, allows users to use space for Elastic IP
-- addresses and VPCs.
--
-- 'description', 'ipamPool_description' - The description of the IPAM pool.
--
-- 'ipamArn', 'ipamPool_ipamArn' - The ARN of the IPAM.
--
-- 'ipamPoolArn', 'ipamPool_ipamPoolArn' - The ARN of the IPAM pool.
--
-- 'ipamPoolId', 'ipamPool_ipamPoolId' - The ID of the IPAM pool.
--
-- 'ipamRegion', 'ipamPool_ipamRegion' - The Amazon Web Services Region of the IPAM pool.
--
-- 'ipamScopeArn', 'ipamPool_ipamScopeArn' - The ARN of the scope of the IPAM pool.
--
-- 'ipamScopeType', 'ipamPool_ipamScopeType' - In IPAM, a scope is the highest-level container within IPAM. An IPAM
-- contains two default scopes. Each scope represents the IP space for a
-- single network. The private scope is intended for all private IP address
-- space. The public scope is intended for all public IP address space.
-- Scopes enable you to reuse IP addresses across multiple unconnected
-- networks without causing IP address overlap or conflict.
--
-- 'locale', 'ipamPool_locale' - The locale of the IPAM pool. In IPAM, the locale is the Amazon Web
-- Services Region where you want to make an IPAM pool available for
-- allocations. Only resources in the same Region as the locale of the pool
-- can get IP address allocations from the pool. You can only allocate a
-- CIDR for a VPC, for example, from an IPAM pool that shares a locale with
-- the VPC’s Region. Note that once you choose a Locale for a pool, you
-- cannot modify it. If you choose an Amazon Web Services Region for locale
-- that has not been configured as an operating Region for the IPAM,
-- you\'ll get an error.
--
-- 'ownerId', 'ipamPool_ownerId' - The Amazon Web Services account ID of the owner of the IPAM pool.
--
-- 'poolDepth', 'ipamPool_poolDepth' - The depth of pools in your IPAM pool. The pool depth quota is 10. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/quotas-ipam.html Quotas in IPAM>
-- in the /Amazon VPC IPAM User Guide/.
--
-- 'publiclyAdvertisable', 'ipamPool_publiclyAdvertisable' - Determines if a pool is publicly advertisable. This option is not
-- available for pools with AddressFamily set to @ipv4@.
--
-- 'sourceIpamPoolId', 'ipamPool_sourceIpamPoolId' - The ID of the source IPAM pool. You can use this option to create an
-- IPAM pool within an existing source pool.
--
-- 'state', 'ipamPool_state' - The state of the IPAM pool.
--
-- 'stateMessage', 'ipamPool_stateMessage' - A message related to the failed creation of an IPAM pool.
--
-- 'tags', 'ipamPool_tags' - The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
newIpamPool ::
  IpamPool
newIpamPool =
  IpamPool'
    { addressFamily = Prelude.Nothing,
      allocationDefaultNetmaskLength = Prelude.Nothing,
      allocationMaxNetmaskLength = Prelude.Nothing,
      allocationMinNetmaskLength = Prelude.Nothing,
      allocationResourceTags = Prelude.Nothing,
      autoImport = Prelude.Nothing,
      awsService = Prelude.Nothing,
      description = Prelude.Nothing,
      ipamArn = Prelude.Nothing,
      ipamPoolArn = Prelude.Nothing,
      ipamPoolId = Prelude.Nothing,
      ipamRegion = Prelude.Nothing,
      ipamScopeArn = Prelude.Nothing,
      ipamScopeType = Prelude.Nothing,
      locale = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      poolDepth = Prelude.Nothing,
      publiclyAdvertisable = Prelude.Nothing,
      sourceIpamPoolId = Prelude.Nothing,
      state = Prelude.Nothing,
      stateMessage = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The address family of the pool.
ipamPool_addressFamily :: Lens.Lens' IpamPool (Prelude.Maybe AddressFamily)
ipamPool_addressFamily = Lens.lens (\IpamPool' {addressFamily} -> addressFamily) (\s@IpamPool' {} a -> s {addressFamily = a} :: IpamPool)

-- | The default netmask length for allocations added to this pool. If, for
-- example, the CIDR assigned to this pool is 10.0.0.0\/8 and you enter 16
-- here, new allocations will default to 10.0.0.0\/16.
ipamPool_allocationDefaultNetmaskLength :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Natural)
ipamPool_allocationDefaultNetmaskLength = Lens.lens (\IpamPool' {allocationDefaultNetmaskLength} -> allocationDefaultNetmaskLength) (\s@IpamPool' {} a -> s {allocationDefaultNetmaskLength = a} :: IpamPool)

-- | The maximum netmask length possible for CIDR allocations in this IPAM
-- pool to be compliant. The maximum netmask length must be greater than
-- the minimum netmask length. Possible netmask lengths for IPv4 addresses
-- are 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
ipamPool_allocationMaxNetmaskLength :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Natural)
ipamPool_allocationMaxNetmaskLength = Lens.lens (\IpamPool' {allocationMaxNetmaskLength} -> allocationMaxNetmaskLength) (\s@IpamPool' {} a -> s {allocationMaxNetmaskLength = a} :: IpamPool)

-- | The minimum netmask length required for CIDR allocations in this IPAM
-- pool to be compliant. The minimum netmask length must be less than the
-- maximum netmask length. Possible netmask lengths for IPv4 addresses are
-- 0 - 32. Possible netmask lengths for IPv6 addresses are 0 - 128.
ipamPool_allocationMinNetmaskLength :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Natural)
ipamPool_allocationMinNetmaskLength = Lens.lens (\IpamPool' {allocationMinNetmaskLength} -> allocationMinNetmaskLength) (\s@IpamPool' {} a -> s {allocationMinNetmaskLength = a} :: IpamPool)

-- | Tags that are required for resources that use CIDRs from this IPAM pool.
-- Resources that do not have these tags will not be allowed to allocate
-- space from the pool. If the resources have their tags changed after they
-- have allocated space or if the allocation tagging requirements are
-- changed on the pool, the resource may be marked as noncompliant.
ipamPool_allocationResourceTags :: Lens.Lens' IpamPool (Prelude.Maybe [IpamResourceTag])
ipamPool_allocationResourceTags = Lens.lens (\IpamPool' {allocationResourceTags} -> allocationResourceTags) (\s@IpamPool' {} a -> s {allocationResourceTags = a} :: IpamPool) Prelude.. Lens.mapping Lens.coerced

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
ipamPool_autoImport :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Bool)
ipamPool_autoImport = Lens.lens (\IpamPool' {autoImport} -> autoImport) (\s@IpamPool' {} a -> s {autoImport = a} :: IpamPool)

-- | Limits which service in Amazon Web Services that the pool can be used
-- in. \"ec2\", for example, allows users to use space for Elastic IP
-- addresses and VPCs.
ipamPool_awsService :: Lens.Lens' IpamPool (Prelude.Maybe IpamPoolAwsService)
ipamPool_awsService = Lens.lens (\IpamPool' {awsService} -> awsService) (\s@IpamPool' {} a -> s {awsService = a} :: IpamPool)

-- | The description of the IPAM pool.
ipamPool_description :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_description = Lens.lens (\IpamPool' {description} -> description) (\s@IpamPool' {} a -> s {description = a} :: IpamPool)

-- | The ARN of the IPAM.
ipamPool_ipamArn :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_ipamArn = Lens.lens (\IpamPool' {ipamArn} -> ipamArn) (\s@IpamPool' {} a -> s {ipamArn = a} :: IpamPool)

-- | The ARN of the IPAM pool.
ipamPool_ipamPoolArn :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_ipamPoolArn = Lens.lens (\IpamPool' {ipamPoolArn} -> ipamPoolArn) (\s@IpamPool' {} a -> s {ipamPoolArn = a} :: IpamPool)

-- | The ID of the IPAM pool.
ipamPool_ipamPoolId :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_ipamPoolId = Lens.lens (\IpamPool' {ipamPoolId} -> ipamPoolId) (\s@IpamPool' {} a -> s {ipamPoolId = a} :: IpamPool)

-- | The Amazon Web Services Region of the IPAM pool.
ipamPool_ipamRegion :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_ipamRegion = Lens.lens (\IpamPool' {ipamRegion} -> ipamRegion) (\s@IpamPool' {} a -> s {ipamRegion = a} :: IpamPool)

-- | The ARN of the scope of the IPAM pool.
ipamPool_ipamScopeArn :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_ipamScopeArn = Lens.lens (\IpamPool' {ipamScopeArn} -> ipamScopeArn) (\s@IpamPool' {} a -> s {ipamScopeArn = a} :: IpamPool)

-- | In IPAM, a scope is the highest-level container within IPAM. An IPAM
-- contains two default scopes. Each scope represents the IP space for a
-- single network. The private scope is intended for all private IP address
-- space. The public scope is intended for all public IP address space.
-- Scopes enable you to reuse IP addresses across multiple unconnected
-- networks without causing IP address overlap or conflict.
ipamPool_ipamScopeType :: Lens.Lens' IpamPool (Prelude.Maybe IpamScopeType)
ipamPool_ipamScopeType = Lens.lens (\IpamPool' {ipamScopeType} -> ipamScopeType) (\s@IpamPool' {} a -> s {ipamScopeType = a} :: IpamPool)

-- | The locale of the IPAM pool. In IPAM, the locale is the Amazon Web
-- Services Region where you want to make an IPAM pool available for
-- allocations. Only resources in the same Region as the locale of the pool
-- can get IP address allocations from the pool. You can only allocate a
-- CIDR for a VPC, for example, from an IPAM pool that shares a locale with
-- the VPC’s Region. Note that once you choose a Locale for a pool, you
-- cannot modify it. If you choose an Amazon Web Services Region for locale
-- that has not been configured as an operating Region for the IPAM,
-- you\'ll get an error.
ipamPool_locale :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_locale = Lens.lens (\IpamPool' {locale} -> locale) (\s@IpamPool' {} a -> s {locale = a} :: IpamPool)

-- | The Amazon Web Services account ID of the owner of the IPAM pool.
ipamPool_ownerId :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_ownerId = Lens.lens (\IpamPool' {ownerId} -> ownerId) (\s@IpamPool' {} a -> s {ownerId = a} :: IpamPool)

-- | The depth of pools in your IPAM pool. The pool depth quota is 10. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/quotas-ipam.html Quotas in IPAM>
-- in the /Amazon VPC IPAM User Guide/.
ipamPool_poolDepth :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Int)
ipamPool_poolDepth = Lens.lens (\IpamPool' {poolDepth} -> poolDepth) (\s@IpamPool' {} a -> s {poolDepth = a} :: IpamPool)

-- | Determines if a pool is publicly advertisable. This option is not
-- available for pools with AddressFamily set to @ipv4@.
ipamPool_publiclyAdvertisable :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Bool)
ipamPool_publiclyAdvertisable = Lens.lens (\IpamPool' {publiclyAdvertisable} -> publiclyAdvertisable) (\s@IpamPool' {} a -> s {publiclyAdvertisable = a} :: IpamPool)

-- | The ID of the source IPAM pool. You can use this option to create an
-- IPAM pool within an existing source pool.
ipamPool_sourceIpamPoolId :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_sourceIpamPoolId = Lens.lens (\IpamPool' {sourceIpamPoolId} -> sourceIpamPoolId) (\s@IpamPool' {} a -> s {sourceIpamPoolId = a} :: IpamPool)

-- | The state of the IPAM pool.
ipamPool_state :: Lens.Lens' IpamPool (Prelude.Maybe IpamPoolState)
ipamPool_state = Lens.lens (\IpamPool' {state} -> state) (\s@IpamPool' {} a -> s {state = a} :: IpamPool)

-- | A message related to the failed creation of an IPAM pool.
ipamPool_stateMessage :: Lens.Lens' IpamPool (Prelude.Maybe Prelude.Text)
ipamPool_stateMessage = Lens.lens (\IpamPool' {stateMessage} -> stateMessage) (\s@IpamPool' {} a -> s {stateMessage = a} :: IpamPool)

-- | The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
ipamPool_tags :: Lens.Lens' IpamPool (Prelude.Maybe [Tag])
ipamPool_tags = Lens.lens (\IpamPool' {tags} -> tags) (\s@IpamPool' {} a -> s {tags = a} :: IpamPool) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML IpamPool where
  parseXML x =
    IpamPool'
      Prelude.<$> (x Data..@? "addressFamily")
      Prelude.<*> (x Data..@? "allocationDefaultNetmaskLength")
      Prelude.<*> (x Data..@? "allocationMaxNetmaskLength")
      Prelude.<*> (x Data..@? "allocationMinNetmaskLength")
      Prelude.<*> ( x Data..@? "allocationResourceTagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "autoImport")
      Prelude.<*> (x Data..@? "awsService")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "ipamArn")
      Prelude.<*> (x Data..@? "ipamPoolArn")
      Prelude.<*> (x Data..@? "ipamPoolId")
      Prelude.<*> (x Data..@? "ipamRegion")
      Prelude.<*> (x Data..@? "ipamScopeArn")
      Prelude.<*> (x Data..@? "ipamScopeType")
      Prelude.<*> (x Data..@? "locale")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "poolDepth")
      Prelude.<*> (x Data..@? "publiclyAdvertisable")
      Prelude.<*> (x Data..@? "sourceIpamPoolId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "stateMessage")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable IpamPool where
  hashWithSalt _salt IpamPool' {..} =
    _salt `Prelude.hashWithSalt` addressFamily
      `Prelude.hashWithSalt` allocationDefaultNetmaskLength
      `Prelude.hashWithSalt` allocationMaxNetmaskLength
      `Prelude.hashWithSalt` allocationMinNetmaskLength
      `Prelude.hashWithSalt` allocationResourceTags
      `Prelude.hashWithSalt` autoImport
      `Prelude.hashWithSalt` awsService
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ipamArn
      `Prelude.hashWithSalt` ipamPoolArn
      `Prelude.hashWithSalt` ipamPoolId
      `Prelude.hashWithSalt` ipamRegion
      `Prelude.hashWithSalt` ipamScopeArn
      `Prelude.hashWithSalt` ipamScopeType
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` poolDepth
      `Prelude.hashWithSalt` publiclyAdvertisable
      `Prelude.hashWithSalt` sourceIpamPoolId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateMessage
      `Prelude.hashWithSalt` tags

instance Prelude.NFData IpamPool where
  rnf IpamPool' {..} =
    Prelude.rnf addressFamily
      `Prelude.seq` Prelude.rnf allocationDefaultNetmaskLength
      `Prelude.seq` Prelude.rnf allocationMaxNetmaskLength
      `Prelude.seq` Prelude.rnf allocationMinNetmaskLength
      `Prelude.seq` Prelude.rnf allocationResourceTags
      `Prelude.seq` Prelude.rnf autoImport
      `Prelude.seq` Prelude.rnf awsService
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ipamArn
      `Prelude.seq` Prelude.rnf ipamPoolArn
      `Prelude.seq` Prelude.rnf ipamPoolId
      `Prelude.seq` Prelude.rnf ipamRegion
      `Prelude.seq` Prelude.rnf ipamScopeArn
      `Prelude.seq` Prelude.rnf ipamScopeType
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf poolDepth
      `Prelude.seq` Prelude.rnf publiclyAdvertisable
      `Prelude.seq` Prelude.rnf sourceIpamPoolId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateMessage
      `Prelude.seq` Prelude.rnf tags
