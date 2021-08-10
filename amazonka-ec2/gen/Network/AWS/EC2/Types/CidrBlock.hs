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
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IPv4 CIDR block.
--
-- /See:/ 'newCidrBlock' smart constructor.
data CidrBlock = CidrBlock'
  { -- | The IPv4 CIDR block.
    cidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
newCidrBlock =
  CidrBlock' {cidrBlock = Prelude.Nothing}

-- | The IPv4 CIDR block.
cidrBlock_cidrBlock :: Lens.Lens' CidrBlock (Prelude.Maybe Prelude.Text)
cidrBlock_cidrBlock = Lens.lens (\CidrBlock' {cidrBlock} -> cidrBlock) (\s@CidrBlock' {} a -> s {cidrBlock = a} :: CidrBlock)

instance Core.FromXML CidrBlock where
  parseXML x =
    CidrBlock' Prelude.<$> (x Core..@? "cidrBlock")

instance Prelude.Hashable CidrBlock

instance Prelude.NFData CidrBlock
