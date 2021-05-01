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
-- Module      : Network.AWS.EC2.Types.Ipv6CidrBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Ipv6CidrBlock where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IPv6 CIDR block.
--
-- /See:/ 'newIpv6CidrBlock' smart constructor.
data Ipv6CidrBlock = Ipv6CidrBlock'
  { -- | The IPv6 CIDR block.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Ipv6CidrBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrBlock', 'ipv6CidrBlock_ipv6CidrBlock' - The IPv6 CIDR block.
newIpv6CidrBlock ::
  Ipv6CidrBlock
newIpv6CidrBlock =
  Ipv6CidrBlock' {ipv6CidrBlock = Prelude.Nothing}

-- | The IPv6 CIDR block.
ipv6CidrBlock_ipv6CidrBlock :: Lens.Lens' Ipv6CidrBlock (Prelude.Maybe Prelude.Text)
ipv6CidrBlock_ipv6CidrBlock = Lens.lens (\Ipv6CidrBlock' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@Ipv6CidrBlock' {} a -> s {ipv6CidrBlock = a} :: Ipv6CidrBlock)

instance Prelude.FromXML Ipv6CidrBlock where
  parseXML x =
    Ipv6CidrBlock'
      Prelude.<$> (x Prelude..@? "ipv6CidrBlock")

instance Prelude.Hashable Ipv6CidrBlock

instance Prelude.NFData Ipv6CidrBlock
