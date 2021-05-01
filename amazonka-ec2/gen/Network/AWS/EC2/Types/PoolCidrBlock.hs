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
-- Module      : Network.AWS.EC2.Types.PoolCidrBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PoolCidrBlock where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a CIDR block for an address pool.
--
-- /See:/ 'newPoolCidrBlock' smart constructor.
data PoolCidrBlock = PoolCidrBlock'
  { -- | The CIDR block.
    cidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PoolCidrBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'poolCidrBlock_cidr' - The CIDR block.
newPoolCidrBlock ::
  PoolCidrBlock
newPoolCidrBlock =
  PoolCidrBlock' {cidr = Prelude.Nothing}

-- | The CIDR block.
poolCidrBlock_cidr :: Lens.Lens' PoolCidrBlock (Prelude.Maybe Prelude.Text)
poolCidrBlock_cidr = Lens.lens (\PoolCidrBlock' {cidr} -> cidr) (\s@PoolCidrBlock' {} a -> s {cidr = a} :: PoolCidrBlock)

instance Prelude.FromXML PoolCidrBlock where
  parseXML x =
    PoolCidrBlock'
      Prelude.<$> (x Prelude..@? "poolCidrBlock")

instance Prelude.Hashable PoolCidrBlock

instance Prelude.NFData PoolCidrBlock
