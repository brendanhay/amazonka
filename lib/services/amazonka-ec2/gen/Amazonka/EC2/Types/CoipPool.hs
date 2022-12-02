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
-- Module      : Amazonka.EC2.Types.CoipPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CoipPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a customer-owned address pool.
--
-- /See:/ 'newCoipPool' smart constructor.
data CoipPool = CoipPool'
  { -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN of the address pool.
    poolArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The address ranges of the address pool.
    poolCidrs :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the address pool.
    poolId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoipPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'coipPool_tags' - The tags.
--
-- 'poolArn', 'coipPool_poolArn' - The ARN of the address pool.
--
-- 'localGatewayRouteTableId', 'coipPool_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'poolCidrs', 'coipPool_poolCidrs' - The address ranges of the address pool.
--
-- 'poolId', 'coipPool_poolId' - The ID of the address pool.
newCoipPool ::
  CoipPool
newCoipPool =
  CoipPool'
    { tags = Prelude.Nothing,
      poolArn = Prelude.Nothing,
      localGatewayRouteTableId = Prelude.Nothing,
      poolCidrs = Prelude.Nothing,
      poolId = Prelude.Nothing
    }

-- | The tags.
coipPool_tags :: Lens.Lens' CoipPool (Prelude.Maybe [Tag])
coipPool_tags = Lens.lens (\CoipPool' {tags} -> tags) (\s@CoipPool' {} a -> s {tags = a} :: CoipPool) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the address pool.
coipPool_poolArn :: Lens.Lens' CoipPool (Prelude.Maybe Prelude.Text)
coipPool_poolArn = Lens.lens (\CoipPool' {poolArn} -> poolArn) (\s@CoipPool' {} a -> s {poolArn = a} :: CoipPool)

-- | The ID of the local gateway route table.
coipPool_localGatewayRouteTableId :: Lens.Lens' CoipPool (Prelude.Maybe Prelude.Text)
coipPool_localGatewayRouteTableId = Lens.lens (\CoipPool' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@CoipPool' {} a -> s {localGatewayRouteTableId = a} :: CoipPool)

-- | The address ranges of the address pool.
coipPool_poolCidrs :: Lens.Lens' CoipPool (Prelude.Maybe [Prelude.Text])
coipPool_poolCidrs = Lens.lens (\CoipPool' {poolCidrs} -> poolCidrs) (\s@CoipPool' {} a -> s {poolCidrs = a} :: CoipPool) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the address pool.
coipPool_poolId :: Lens.Lens' CoipPool (Prelude.Maybe Prelude.Text)
coipPool_poolId = Lens.lens (\CoipPool' {poolId} -> poolId) (\s@CoipPool' {} a -> s {poolId = a} :: CoipPool)

instance Data.FromXML CoipPool where
  parseXML x =
    CoipPool'
      Prelude.<$> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "poolArn")
      Prelude.<*> (x Data..@? "localGatewayRouteTableId")
      Prelude.<*> ( x Data..@? "poolCidrSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "poolId")

instance Prelude.Hashable CoipPool where
  hashWithSalt _salt CoipPool' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` poolArn
      `Prelude.hashWithSalt` localGatewayRouteTableId
      `Prelude.hashWithSalt` poolCidrs
      `Prelude.hashWithSalt` poolId

instance Prelude.NFData CoipPool where
  rnf CoipPool' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf poolArn
      `Prelude.seq` Prelude.rnf localGatewayRouteTableId
      `Prelude.seq` Prelude.rnf poolCidrs
      `Prelude.seq` Prelude.rnf poolId
