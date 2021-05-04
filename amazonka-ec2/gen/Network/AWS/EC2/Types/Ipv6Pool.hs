{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PoolCidrBlock
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IPv6 address pool.
--
-- /See:/ 'newIpv6Pool' smart constructor.
data Ipv6Pool = Ipv6Pool'
  { -- | The ID of the address pool.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The CIDR blocks for the address pool.
    poolCidrBlocks :: Prelude.Maybe [PoolCidrBlock],
    -- | Any tags for the address pool.
    tags :: Prelude.Maybe [Tag],
    -- | The description for the address pool.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { poolId = Prelude.Nothing,
      poolCidrBlocks = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ID of the address pool.
ipv6Pool_poolId :: Lens.Lens' Ipv6Pool (Prelude.Maybe Prelude.Text)
ipv6Pool_poolId = Lens.lens (\Ipv6Pool' {poolId} -> poolId) (\s@Ipv6Pool' {} a -> s {poolId = a} :: Ipv6Pool)

-- | The CIDR blocks for the address pool.
ipv6Pool_poolCidrBlocks :: Lens.Lens' Ipv6Pool (Prelude.Maybe [PoolCidrBlock])
ipv6Pool_poolCidrBlocks = Lens.lens (\Ipv6Pool' {poolCidrBlocks} -> poolCidrBlocks) (\s@Ipv6Pool' {} a -> s {poolCidrBlocks = a} :: Ipv6Pool) Prelude.. Lens.mapping Prelude._Coerce

-- | Any tags for the address pool.
ipv6Pool_tags :: Lens.Lens' Ipv6Pool (Prelude.Maybe [Tag])
ipv6Pool_tags = Lens.lens (\Ipv6Pool' {tags} -> tags) (\s@Ipv6Pool' {} a -> s {tags = a} :: Ipv6Pool) Prelude.. Lens.mapping Prelude._Coerce

-- | The description for the address pool.
ipv6Pool_description :: Lens.Lens' Ipv6Pool (Prelude.Maybe Prelude.Text)
ipv6Pool_description = Lens.lens (\Ipv6Pool' {description} -> description) (\s@Ipv6Pool' {} a -> s {description = a} :: Ipv6Pool)

instance Prelude.FromXML Ipv6Pool where
  parseXML x =
    Ipv6Pool'
      Prelude.<$> (x Prelude..@? "poolId")
      Prelude.<*> ( x Prelude..@? "poolCidrBlockSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "description")

instance Prelude.Hashable Ipv6Pool

instance Prelude.NFData Ipv6Pool
