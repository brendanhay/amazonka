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
-- Module      : Network.AWS.EC2.Types.Ipv6Pool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Ipv6Pool where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PoolCidrBlock
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes an IPv6 address pool.
--
-- /See:/ 'newIpv6Pool' smart constructor.
data Ipv6Pool = Ipv6Pool'
  { -- | The ID of the address pool.
    poolId :: Core.Maybe Core.Text,
    -- | The CIDR blocks for the address pool.
    poolCidrBlocks :: Core.Maybe [PoolCidrBlock],
    -- | Any tags for the address pool.
    tags :: Core.Maybe [Tag],
    -- | The description for the address pool.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Ipv6Pool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolId', 'ipv6Pool_poolId' - The ID of the address pool.
--
-- 'poolCidrBlocks', 'ipv6Pool_poolCidrBlocks' - The CIDR blocks for the address pool.
--
-- 'tags', 'ipv6Pool_tags' - Any tags for the address pool.
--
-- 'description', 'ipv6Pool_description' - The description for the address pool.
newIpv6Pool ::
  Ipv6Pool
newIpv6Pool =
  Ipv6Pool'
    { poolId = Core.Nothing,
      poolCidrBlocks = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing
    }

-- | The ID of the address pool.
ipv6Pool_poolId :: Lens.Lens' Ipv6Pool (Core.Maybe Core.Text)
ipv6Pool_poolId = Lens.lens (\Ipv6Pool' {poolId} -> poolId) (\s@Ipv6Pool' {} a -> s {poolId = a} :: Ipv6Pool)

-- | The CIDR blocks for the address pool.
ipv6Pool_poolCidrBlocks :: Lens.Lens' Ipv6Pool (Core.Maybe [PoolCidrBlock])
ipv6Pool_poolCidrBlocks = Lens.lens (\Ipv6Pool' {poolCidrBlocks} -> poolCidrBlocks) (\s@Ipv6Pool' {} a -> s {poolCidrBlocks = a} :: Ipv6Pool) Core.. Lens.mapping Lens._Coerce

-- | Any tags for the address pool.
ipv6Pool_tags :: Lens.Lens' Ipv6Pool (Core.Maybe [Tag])
ipv6Pool_tags = Lens.lens (\Ipv6Pool' {tags} -> tags) (\s@Ipv6Pool' {} a -> s {tags = a} :: Ipv6Pool) Core.. Lens.mapping Lens._Coerce

-- | The description for the address pool.
ipv6Pool_description :: Lens.Lens' Ipv6Pool (Core.Maybe Core.Text)
ipv6Pool_description = Lens.lens (\Ipv6Pool' {description} -> description) (\s@Ipv6Pool' {} a -> s {description = a} :: Ipv6Pool)

instance Core.FromXML Ipv6Pool where
  parseXML x =
    Ipv6Pool'
      Core.<$> (x Core..@? "poolId")
      Core.<*> ( x Core..@? "poolCidrBlockSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "description")

instance Core.Hashable Ipv6Pool

instance Core.NFData Ipv6Pool
