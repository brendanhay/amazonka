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
-- Module      : Amazonka.ElastiCache.Types.CacheSubnetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheSubnetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.NetworkType
import Amazonka.ElastiCache.Types.Subnet
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of one of the following operations:
--
-- -   @CreateCacheSubnetGroup@
--
-- -   @ModifyCacheSubnetGroup@
--
-- /See:/ 'newCacheSubnetGroup' smart constructor.
data CacheSubnetGroup = CacheSubnetGroup'
  { -- | The name of the cache subnet group.
    cacheSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of subnets associated with the cache subnet group.
    subnets :: Prelude.Maybe [Subnet],
    -- | The ARN (Amazon Resource Name) of the cache subnet group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the cache subnet group.
    cacheSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
    -- group.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for workloads
    -- using Redis engine version 6.2 onward or Memcached engine version 1.6.6
    -- on all instances built on the
    -- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
    supportedNetworkTypes :: Prelude.Maybe [NetworkType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSubnetGroupName', 'cacheSubnetGroup_cacheSubnetGroupName' - The name of the cache subnet group.
--
-- 'subnets', 'cacheSubnetGroup_subnets' - A list of subnets associated with the cache subnet group.
--
-- 'arn', 'cacheSubnetGroup_arn' - The ARN (Amazon Resource Name) of the cache subnet group.
--
-- 'cacheSubnetGroupDescription', 'cacheSubnetGroup_cacheSubnetGroupDescription' - The description of the cache subnet group.
--
-- 'vpcId', 'cacheSubnetGroup_vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
--
-- 'supportedNetworkTypes', 'cacheSubnetGroup_supportedNetworkTypes' - Either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for workloads
-- using Redis engine version 6.2 onward or Memcached engine version 1.6.6
-- on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
newCacheSubnetGroup ::
  CacheSubnetGroup
newCacheSubnetGroup =
  CacheSubnetGroup'
    { cacheSubnetGroupName =
        Prelude.Nothing,
      subnets = Prelude.Nothing,
      arn = Prelude.Nothing,
      cacheSubnetGroupDescription = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      supportedNetworkTypes = Prelude.Nothing
    }

-- | The name of the cache subnet group.
cacheSubnetGroup_cacheSubnetGroupName :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe Prelude.Text)
cacheSubnetGroup_cacheSubnetGroupName = Lens.lens (\CacheSubnetGroup' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@CacheSubnetGroup' {} a -> s {cacheSubnetGroupName = a} :: CacheSubnetGroup)

-- | A list of subnets associated with the cache subnet group.
cacheSubnetGroup_subnets :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe [Subnet])
cacheSubnetGroup_subnets = Lens.lens (\CacheSubnetGroup' {subnets} -> subnets) (\s@CacheSubnetGroup' {} a -> s {subnets = a} :: CacheSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ARN (Amazon Resource Name) of the cache subnet group.
cacheSubnetGroup_arn :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe Prelude.Text)
cacheSubnetGroup_arn = Lens.lens (\CacheSubnetGroup' {arn} -> arn) (\s@CacheSubnetGroup' {} a -> s {arn = a} :: CacheSubnetGroup)

-- | The description of the cache subnet group.
cacheSubnetGroup_cacheSubnetGroupDescription :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe Prelude.Text)
cacheSubnetGroup_cacheSubnetGroupDescription = Lens.lens (\CacheSubnetGroup' {cacheSubnetGroupDescription} -> cacheSubnetGroupDescription) (\s@CacheSubnetGroup' {} a -> s {cacheSubnetGroupDescription = a} :: CacheSubnetGroup)

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
cacheSubnetGroup_vpcId :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe Prelude.Text)
cacheSubnetGroup_vpcId = Lens.lens (\CacheSubnetGroup' {vpcId} -> vpcId) (\s@CacheSubnetGroup' {} a -> s {vpcId = a} :: CacheSubnetGroup)

-- | Either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for workloads
-- using Redis engine version 6.2 onward or Memcached engine version 1.6.6
-- on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
cacheSubnetGroup_supportedNetworkTypes :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe [NetworkType])
cacheSubnetGroup_supportedNetworkTypes = Lens.lens (\CacheSubnetGroup' {supportedNetworkTypes} -> supportedNetworkTypes) (\s@CacheSubnetGroup' {} a -> s {supportedNetworkTypes = a} :: CacheSubnetGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML CacheSubnetGroup where
  parseXML x =
    CacheSubnetGroup'
      Prelude.<$> (x Data..@? "CacheSubnetGroupName")
      Prelude.<*> ( x Data..@? "Subnets" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Subnet")
                  )
      Prelude.<*> (x Data..@? "ARN")
      Prelude.<*> (x Data..@? "CacheSubnetGroupDescription")
      Prelude.<*> (x Data..@? "VpcId")
      Prelude.<*> ( x Data..@? "SupportedNetworkTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable CacheSubnetGroup where
  hashWithSalt _salt CacheSubnetGroup' {..} =
    _salt `Prelude.hashWithSalt` cacheSubnetGroupName
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` cacheSubnetGroupDescription
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` supportedNetworkTypes

instance Prelude.NFData CacheSubnetGroup where
  rnf CacheSubnetGroup' {..} =
    Prelude.rnf cacheSubnetGroupName
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf cacheSubnetGroupDescription
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf supportedNetworkTypes
