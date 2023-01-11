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
-- Module      : Amazonka.SecurityHub.Types.CidrBlockAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.CidrBlockAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An IPv4 CIDR block association.
--
-- /See:/ 'newCidrBlockAssociation' smart constructor.
data CidrBlockAssociation = CidrBlockAssociation'
  { -- | The association ID for the IPv4 CIDR block.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR block.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the state of the IPv4 CIDR block.
    cidrBlockState :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CidrBlockAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'cidrBlockAssociation_associationId' - The association ID for the IPv4 CIDR block.
--
-- 'cidrBlock', 'cidrBlockAssociation_cidrBlock' - The IPv4 CIDR block.
--
-- 'cidrBlockState', 'cidrBlockAssociation_cidrBlockState' - Information about the state of the IPv4 CIDR block.
newCidrBlockAssociation ::
  CidrBlockAssociation
newCidrBlockAssociation =
  CidrBlockAssociation'
    { associationId =
        Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      cidrBlockState = Prelude.Nothing
    }

-- | The association ID for the IPv4 CIDR block.
cidrBlockAssociation_associationId :: Lens.Lens' CidrBlockAssociation (Prelude.Maybe Prelude.Text)
cidrBlockAssociation_associationId = Lens.lens (\CidrBlockAssociation' {associationId} -> associationId) (\s@CidrBlockAssociation' {} a -> s {associationId = a} :: CidrBlockAssociation)

-- | The IPv4 CIDR block.
cidrBlockAssociation_cidrBlock :: Lens.Lens' CidrBlockAssociation (Prelude.Maybe Prelude.Text)
cidrBlockAssociation_cidrBlock = Lens.lens (\CidrBlockAssociation' {cidrBlock} -> cidrBlock) (\s@CidrBlockAssociation' {} a -> s {cidrBlock = a} :: CidrBlockAssociation)

-- | Information about the state of the IPv4 CIDR block.
cidrBlockAssociation_cidrBlockState :: Lens.Lens' CidrBlockAssociation (Prelude.Maybe Prelude.Text)
cidrBlockAssociation_cidrBlockState = Lens.lens (\CidrBlockAssociation' {cidrBlockState} -> cidrBlockState) (\s@CidrBlockAssociation' {} a -> s {cidrBlockState = a} :: CidrBlockAssociation)

instance Data.FromJSON CidrBlockAssociation where
  parseJSON =
    Data.withObject
      "CidrBlockAssociation"
      ( \x ->
          CidrBlockAssociation'
            Prelude.<$> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "CidrBlock")
            Prelude.<*> (x Data..:? "CidrBlockState")
      )

instance Prelude.Hashable CidrBlockAssociation where
  hashWithSalt _salt CidrBlockAssociation' {..} =
    _salt `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` cidrBlockState

instance Prelude.NFData CidrBlockAssociation where
  rnf CidrBlockAssociation' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf cidrBlockState

instance Data.ToJSON CidrBlockAssociation where
  toJSON CidrBlockAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociationId" Data..=) Prelude.<$> associationId,
            ("CidrBlock" Data..=) Prelude.<$> cidrBlock,
            ("CidrBlockState" Data..=)
              Prelude.<$> cidrBlockState
          ]
      )
