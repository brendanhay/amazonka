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
-- Module      : Network.AWS.ElastiCache.Types.CacheSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheSubnetGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.Subnet
import qualified Network.AWS.Lens as Lens

-- | Represents the output of one of the following operations:
--
-- -   @CreateCacheSubnetGroup@
--
-- -   @ModifyCacheSubnetGroup@
--
-- /See:/ 'newCacheSubnetGroup' smart constructor.
data CacheSubnetGroup = CacheSubnetGroup'
  { -- | The ARN (Amazon Resource Name) of the cache subnet group.
    arn :: Core.Maybe Core.Text,
    -- | The name of the cache subnet group.
    cacheSubnetGroupName :: Core.Maybe Core.Text,
    -- | The description of the cache subnet group.
    cacheSubnetGroupDescription :: Core.Maybe Core.Text,
    -- | A list of subnets associated with the cache subnet group.
    subnets :: Core.Maybe [Subnet],
    -- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
    -- group.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CacheSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'cacheSubnetGroup_arn' - The ARN (Amazon Resource Name) of the cache subnet group.
--
-- 'cacheSubnetGroupName', 'cacheSubnetGroup_cacheSubnetGroupName' - The name of the cache subnet group.
--
-- 'cacheSubnetGroupDescription', 'cacheSubnetGroup_cacheSubnetGroupDescription' - The description of the cache subnet group.
--
-- 'subnets', 'cacheSubnetGroup_subnets' - A list of subnets associated with the cache subnet group.
--
-- 'vpcId', 'cacheSubnetGroup_vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
newCacheSubnetGroup ::
  CacheSubnetGroup
newCacheSubnetGroup =
  CacheSubnetGroup'
    { arn = Core.Nothing,
      cacheSubnetGroupName = Core.Nothing,
      cacheSubnetGroupDescription = Core.Nothing,
      subnets = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The ARN (Amazon Resource Name) of the cache subnet group.
cacheSubnetGroup_arn :: Lens.Lens' CacheSubnetGroup (Core.Maybe Core.Text)
cacheSubnetGroup_arn = Lens.lens (\CacheSubnetGroup' {arn} -> arn) (\s@CacheSubnetGroup' {} a -> s {arn = a} :: CacheSubnetGroup)

-- | The name of the cache subnet group.
cacheSubnetGroup_cacheSubnetGroupName :: Lens.Lens' CacheSubnetGroup (Core.Maybe Core.Text)
cacheSubnetGroup_cacheSubnetGroupName = Lens.lens (\CacheSubnetGroup' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@CacheSubnetGroup' {} a -> s {cacheSubnetGroupName = a} :: CacheSubnetGroup)

-- | The description of the cache subnet group.
cacheSubnetGroup_cacheSubnetGroupDescription :: Lens.Lens' CacheSubnetGroup (Core.Maybe Core.Text)
cacheSubnetGroup_cacheSubnetGroupDescription = Lens.lens (\CacheSubnetGroup' {cacheSubnetGroupDescription} -> cacheSubnetGroupDescription) (\s@CacheSubnetGroup' {} a -> s {cacheSubnetGroupDescription = a} :: CacheSubnetGroup)

-- | A list of subnets associated with the cache subnet group.
cacheSubnetGroup_subnets :: Lens.Lens' CacheSubnetGroup (Core.Maybe [Subnet])
cacheSubnetGroup_subnets = Lens.lens (\CacheSubnetGroup' {subnets} -> subnets) (\s@CacheSubnetGroup' {} a -> s {subnets = a} :: CacheSubnetGroup) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
cacheSubnetGroup_vpcId :: Lens.Lens' CacheSubnetGroup (Core.Maybe Core.Text)
cacheSubnetGroup_vpcId = Lens.lens (\CacheSubnetGroup' {vpcId} -> vpcId) (\s@CacheSubnetGroup' {} a -> s {vpcId = a} :: CacheSubnetGroup)

instance Core.FromXML CacheSubnetGroup where
  parseXML x =
    CacheSubnetGroup'
      Core.<$> (x Core..@? "ARN")
      Core.<*> (x Core..@? "CacheSubnetGroupName")
      Core.<*> (x Core..@? "CacheSubnetGroupDescription")
      Core.<*> ( x Core..@? "Subnets" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Subnet")
               )
      Core.<*> (x Core..@? "VpcId")

instance Core.Hashable CacheSubnetGroup

instance Core.NFData CacheSubnetGroup
