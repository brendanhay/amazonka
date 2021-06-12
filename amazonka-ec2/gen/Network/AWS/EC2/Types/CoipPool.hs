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
-- Module      : Network.AWS.EC2.Types.CoipPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CoipPool where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a customer-owned address pool.
--
-- /See:/ 'newCoipPool' smart constructor.
data CoipPool = CoipPool'
  { -- | The ID of the address pool.
    poolId :: Core.Maybe Core.Text,
    -- | The ARN of the address pool.
    poolArn :: Core.Maybe Core.Text,
    -- | The address ranges of the address pool.
    poolCidrs :: Core.Maybe [Core.Text],
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Core.Maybe Core.Text,
    -- | The tags.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CoipPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolId', 'coipPool_poolId' - The ID of the address pool.
--
-- 'poolArn', 'coipPool_poolArn' - The ARN of the address pool.
--
-- 'poolCidrs', 'coipPool_poolCidrs' - The address ranges of the address pool.
--
-- 'localGatewayRouteTableId', 'coipPool_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'tags', 'coipPool_tags' - The tags.
newCoipPool ::
  CoipPool
newCoipPool =
  CoipPool'
    { poolId = Core.Nothing,
      poolArn = Core.Nothing,
      poolCidrs = Core.Nothing,
      localGatewayRouteTableId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ID of the address pool.
coipPool_poolId :: Lens.Lens' CoipPool (Core.Maybe Core.Text)
coipPool_poolId = Lens.lens (\CoipPool' {poolId} -> poolId) (\s@CoipPool' {} a -> s {poolId = a} :: CoipPool)

-- | The ARN of the address pool.
coipPool_poolArn :: Lens.Lens' CoipPool (Core.Maybe Core.Text)
coipPool_poolArn = Lens.lens (\CoipPool' {poolArn} -> poolArn) (\s@CoipPool' {} a -> s {poolArn = a} :: CoipPool)

-- | The address ranges of the address pool.
coipPool_poolCidrs :: Lens.Lens' CoipPool (Core.Maybe [Core.Text])
coipPool_poolCidrs = Lens.lens (\CoipPool' {poolCidrs} -> poolCidrs) (\s@CoipPool' {} a -> s {poolCidrs = a} :: CoipPool) Core.. Lens.mapping Lens._Coerce

-- | The ID of the local gateway route table.
coipPool_localGatewayRouteTableId :: Lens.Lens' CoipPool (Core.Maybe Core.Text)
coipPool_localGatewayRouteTableId = Lens.lens (\CoipPool' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@CoipPool' {} a -> s {localGatewayRouteTableId = a} :: CoipPool)

-- | The tags.
coipPool_tags :: Lens.Lens' CoipPool (Core.Maybe [Tag])
coipPool_tags = Lens.lens (\CoipPool' {tags} -> tags) (\s@CoipPool' {} a -> s {tags = a} :: CoipPool) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML CoipPool where
  parseXML x =
    CoipPool'
      Core.<$> (x Core..@? "poolId")
      Core.<*> (x Core..@? "poolArn")
      Core.<*> ( x Core..@? "poolCidrSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "localGatewayRouteTableId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable CoipPool

instance Core.NFData CoipPool
