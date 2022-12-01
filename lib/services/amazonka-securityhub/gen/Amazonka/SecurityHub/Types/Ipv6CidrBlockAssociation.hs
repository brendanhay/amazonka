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
-- Module      : Amazonka.SecurityHub.Types.Ipv6CidrBlockAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Ipv6CidrBlockAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An IPV6 CIDR block association.
--
-- /See:/ 'newIpv6CidrBlockAssociation' smart constructor.
data Ipv6CidrBlockAssociation = Ipv6CidrBlockAssociation'
  { -- | Information about the state of the CIDR block. Valid values are as
    -- follows:
    --
    -- -   @associating@
    --
    -- -   @associated@
    --
    -- -   @disassociating@
    --
    -- -   @disassociated@
    --
    -- -   @failed@
    --
    -- -   @failing@
    cidrBlockState :: Prelude.Maybe Prelude.Text,
    -- | The association ID for the IPv6 CIDR block.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR block.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ipv6CidrBlockAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlockState', 'ipv6CidrBlockAssociation_cidrBlockState' - Information about the state of the CIDR block. Valid values are as
-- follows:
--
-- -   @associating@
--
-- -   @associated@
--
-- -   @disassociating@
--
-- -   @disassociated@
--
-- -   @failed@
--
-- -   @failing@
--
-- 'associationId', 'ipv6CidrBlockAssociation_associationId' - The association ID for the IPv6 CIDR block.
--
-- 'ipv6CidrBlock', 'ipv6CidrBlockAssociation_ipv6CidrBlock' - The IPv6 CIDR block.
newIpv6CidrBlockAssociation ::
  Ipv6CidrBlockAssociation
newIpv6CidrBlockAssociation =
  Ipv6CidrBlockAssociation'
    { cidrBlockState =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing
    }

-- | Information about the state of the CIDR block. Valid values are as
-- follows:
--
-- -   @associating@
--
-- -   @associated@
--
-- -   @disassociating@
--
-- -   @disassociated@
--
-- -   @failed@
--
-- -   @failing@
ipv6CidrBlockAssociation_cidrBlockState :: Lens.Lens' Ipv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
ipv6CidrBlockAssociation_cidrBlockState = Lens.lens (\Ipv6CidrBlockAssociation' {cidrBlockState} -> cidrBlockState) (\s@Ipv6CidrBlockAssociation' {} a -> s {cidrBlockState = a} :: Ipv6CidrBlockAssociation)

-- | The association ID for the IPv6 CIDR block.
ipv6CidrBlockAssociation_associationId :: Lens.Lens' Ipv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
ipv6CidrBlockAssociation_associationId = Lens.lens (\Ipv6CidrBlockAssociation' {associationId} -> associationId) (\s@Ipv6CidrBlockAssociation' {} a -> s {associationId = a} :: Ipv6CidrBlockAssociation)

-- | The IPv6 CIDR block.
ipv6CidrBlockAssociation_ipv6CidrBlock :: Lens.Lens' Ipv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
ipv6CidrBlockAssociation_ipv6CidrBlock = Lens.lens (\Ipv6CidrBlockAssociation' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@Ipv6CidrBlockAssociation' {} a -> s {ipv6CidrBlock = a} :: Ipv6CidrBlockAssociation)

instance Core.FromJSON Ipv6CidrBlockAssociation where
  parseJSON =
    Core.withObject
      "Ipv6CidrBlockAssociation"
      ( \x ->
          Ipv6CidrBlockAssociation'
            Prelude.<$> (x Core..:? "CidrBlockState")
            Prelude.<*> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "Ipv6CidrBlock")
      )

instance Prelude.Hashable Ipv6CidrBlockAssociation where
  hashWithSalt _salt Ipv6CidrBlockAssociation' {..} =
    _salt `Prelude.hashWithSalt` cidrBlockState
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` ipv6CidrBlock

instance Prelude.NFData Ipv6CidrBlockAssociation where
  rnf Ipv6CidrBlockAssociation' {..} =
    Prelude.rnf cidrBlockState
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf ipv6CidrBlock

instance Core.ToJSON Ipv6CidrBlockAssociation where
  toJSON Ipv6CidrBlockAssociation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CidrBlockState" Core..=)
              Prelude.<$> cidrBlockState,
            ("AssociationId" Core..=) Prelude.<$> associationId,
            ("Ipv6CidrBlock" Core..=) Prelude.<$> ipv6CidrBlock
          ]
      )
