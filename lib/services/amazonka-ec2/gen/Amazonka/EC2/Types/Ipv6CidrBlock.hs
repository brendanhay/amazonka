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
-- Module      : Amazonka.EC2.Types.Ipv6CidrBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ipv6CidrBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an IPv6 CIDR block.
--
-- /See:/ 'newIpv6CidrBlock' smart constructor.
data Ipv6CidrBlock = Ipv6CidrBlock'
  { -- | The IPv6 CIDR block.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML Ipv6CidrBlock where
  parseXML x =
    Ipv6CidrBlock'
      Prelude.<$> (x Data..@? "ipv6CidrBlock")

instance Prelude.Hashable Ipv6CidrBlock where
  hashWithSalt _salt Ipv6CidrBlock' {..} =
    _salt `Prelude.hashWithSalt` ipv6CidrBlock

instance Prelude.NFData Ipv6CidrBlock where
  rnf Ipv6CidrBlock' {..} = Prelude.rnf ipv6CidrBlock
