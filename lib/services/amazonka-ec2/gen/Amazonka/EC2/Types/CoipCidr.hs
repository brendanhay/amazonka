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
-- Module      : Amazonka.EC2.Types.CoipCidr
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CoipCidr where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about a customer-owned IP address range.
--
-- /See:/ 'newCoipCidr' smart constructor.
data CoipCidr = CoipCidr'
  { -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | An address range in a customer-owned IP address space.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The ID of the address pool.
    coipPoolId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayRouteTableId', 'coipCidr_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'cidr', 'coipCidr_cidr' - An address range in a customer-owned IP address space.
--
-- 'coipPoolId', 'coipCidr_coipPoolId' - The ID of the address pool.
newCoipCidr ::
  CoipCidr
newCoipCidr =
  CoipCidr'
    { localGatewayRouteTableId =
        Prelude.Nothing,
      cidr = Prelude.Nothing,
      coipPoolId = Prelude.Nothing
    }

-- | The ID of the local gateway route table.
coipCidr_localGatewayRouteTableId :: Lens.Lens' CoipCidr (Prelude.Maybe Prelude.Text)
coipCidr_localGatewayRouteTableId = Lens.lens (\CoipCidr' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@CoipCidr' {} a -> s {localGatewayRouteTableId = a} :: CoipCidr)

-- | An address range in a customer-owned IP address space.
coipCidr_cidr :: Lens.Lens' CoipCidr (Prelude.Maybe Prelude.Text)
coipCidr_cidr = Lens.lens (\CoipCidr' {cidr} -> cidr) (\s@CoipCidr' {} a -> s {cidr = a} :: CoipCidr)

-- | The ID of the address pool.
coipCidr_coipPoolId :: Lens.Lens' CoipCidr (Prelude.Maybe Prelude.Text)
coipCidr_coipPoolId = Lens.lens (\CoipCidr' {coipPoolId} -> coipPoolId) (\s@CoipCidr' {} a -> s {coipPoolId = a} :: CoipCidr)

instance Data.FromXML CoipCidr where
  parseXML x =
    CoipCidr'
      Prelude.<$> (x Data..@? "localGatewayRouteTableId")
      Prelude.<*> (x Data..@? "cidr")
      Prelude.<*> (x Data..@? "coipPoolId")

instance Prelude.Hashable CoipCidr where
  hashWithSalt _salt CoipCidr' {..} =
    _salt
      `Prelude.hashWithSalt` localGatewayRouteTableId
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` coipPoolId

instance Prelude.NFData CoipCidr where
  rnf CoipCidr' {..} =
    Prelude.rnf localGatewayRouteTableId
      `Prelude.seq` Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf coipPoolId
