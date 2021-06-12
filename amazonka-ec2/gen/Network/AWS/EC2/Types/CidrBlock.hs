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
-- Module      : Network.AWS.EC2.Types.CidrBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CidrBlock where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an IPv4 CIDR block.
--
-- /See:/ 'newCidrBlock' smart constructor.
data CidrBlock = CidrBlock'
  { -- | The IPv4 CIDR block.
    cidrBlock :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CidrBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlock', 'cidrBlock_cidrBlock' - The IPv4 CIDR block.
newCidrBlock ::
  CidrBlock
newCidrBlock = CidrBlock' {cidrBlock = Core.Nothing}

-- | The IPv4 CIDR block.
cidrBlock_cidrBlock :: Lens.Lens' CidrBlock (Core.Maybe Core.Text)
cidrBlock_cidrBlock = Lens.lens (\CidrBlock' {cidrBlock} -> cidrBlock) (\s@CidrBlock' {} a -> s {cidrBlock = a} :: CidrBlock)

instance Core.FromXML CidrBlock where
  parseXML x =
    CidrBlock' Core.<$> (x Core..@? "cidrBlock")

instance Core.Hashable CidrBlock

instance Core.NFData CidrBlock
