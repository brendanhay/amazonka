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
-- Module      : Amazonka.EC2.Types.CidrBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CidrBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromXML CidrBlock where
  parseXML x =
    CidrBlock' Prelude.<$> (x Data..@? "cidrBlock")

instance Prelude.Hashable CidrBlock where
  hashWithSalt _salt CidrBlock' {..} =
    _salt `Prelude.hashWithSalt` cidrBlock

instance Prelude.NFData CidrBlock where
  rnf CidrBlock' {..} = Prelude.rnf cidrBlock
