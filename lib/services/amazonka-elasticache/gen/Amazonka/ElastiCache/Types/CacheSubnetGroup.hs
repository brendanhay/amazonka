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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheSubnetGroup where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.Subnet
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of one of the following operations:
--
-- -   @CreateCacheSubnetGroup@
--
-- -   @ModifyCacheSubnetGroup@
--
-- /See:/ 'newCacheSubnetGroup' smart constructor.
data CacheSubnetGroup = CacheSubnetGroup'
  { -- | The ARN (Amazon Resource Name) of the cache subnet group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
    -- group.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | A list of subnets associated with the cache subnet group.
    subnets :: Prelude.Maybe [Subnet],
    -- | The name of the cache subnet group.
    cacheSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The description of the cache subnet group.
    cacheSubnetGroupDescription :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'cacheSubnetGroup_arn' - The ARN (Amazon Resource Name) of the cache subnet group.
--
-- 'vpcId', 'cacheSubnetGroup_vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
--
-- 'subnets', 'cacheSubnetGroup_subnets' - A list of subnets associated with the cache subnet group.
--
-- 'cacheSubnetGroupName', 'cacheSubnetGroup_cacheSubnetGroupName' - The name of the cache subnet group.
--
-- 'cacheSubnetGroupDescription', 'cacheSubnetGroup_cacheSubnetGroupDescription' - The description of the cache subnet group.
newCacheSubnetGroup ::
  CacheSubnetGroup
newCacheSubnetGroup =
  CacheSubnetGroup'
    { arn = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      subnets = Prelude.Nothing,
      cacheSubnetGroupName = Prelude.Nothing,
      cacheSubnetGroupDescription = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the cache subnet group.
cacheSubnetGroup_arn :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe Prelude.Text)
cacheSubnetGroup_arn = Lens.lens (\CacheSubnetGroup' {arn} -> arn) (\s@CacheSubnetGroup' {} a -> s {arn = a} :: CacheSubnetGroup)

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
cacheSubnetGroup_vpcId :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe Prelude.Text)
cacheSubnetGroup_vpcId = Lens.lens (\CacheSubnetGroup' {vpcId} -> vpcId) (\s@CacheSubnetGroup' {} a -> s {vpcId = a} :: CacheSubnetGroup)

-- | A list of subnets associated with the cache subnet group.
cacheSubnetGroup_subnets :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe [Subnet])
cacheSubnetGroup_subnets = Lens.lens (\CacheSubnetGroup' {subnets} -> subnets) (\s@CacheSubnetGroup' {} a -> s {subnets = a} :: CacheSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cache subnet group.
cacheSubnetGroup_cacheSubnetGroupName :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe Prelude.Text)
cacheSubnetGroup_cacheSubnetGroupName = Lens.lens (\CacheSubnetGroup' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@CacheSubnetGroup' {} a -> s {cacheSubnetGroupName = a} :: CacheSubnetGroup)

-- | The description of the cache subnet group.
cacheSubnetGroup_cacheSubnetGroupDescription :: Lens.Lens' CacheSubnetGroup (Prelude.Maybe Prelude.Text)
cacheSubnetGroup_cacheSubnetGroupDescription = Lens.lens (\CacheSubnetGroup' {cacheSubnetGroupDescription} -> cacheSubnetGroupDescription) (\s@CacheSubnetGroup' {} a -> s {cacheSubnetGroupDescription = a} :: CacheSubnetGroup)

instance Core.FromXML CacheSubnetGroup where
  parseXML x =
    CacheSubnetGroup'
      Prelude.<$> (x Core..@? "ARN")
      Prelude.<*> (x Core..@? "VpcId")
      Prelude.<*> ( x Core..@? "Subnets" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Subnet")
                  )
      Prelude.<*> (x Core..@? "CacheSubnetGroupName")
      Prelude.<*> (x Core..@? "CacheSubnetGroupDescription")

instance Prelude.Hashable CacheSubnetGroup where
  hashWithSalt _salt CacheSubnetGroup' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` cacheSubnetGroupName
      `Prelude.hashWithSalt` cacheSubnetGroupDescription

instance Prelude.NFData CacheSubnetGroup where
  rnf CacheSubnetGroup' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf cacheSubnetGroupName
      `Prelude.seq` Prelude.rnf cacheSubnetGroupDescription
