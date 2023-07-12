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
-- Module      : Amazonka.SecurityHub.Types.VpcInfoCidrBlockSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.VpcInfoCidrBlockSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the IPv4 CIDR blocks for the VPC.
--
-- /See:/ 'newVpcInfoCidrBlockSetDetails' smart constructor.
data VpcInfoCidrBlockSetDetails = VpcInfoCidrBlockSetDetails'
  { -- | The IPv4 CIDR block for the VPC.
    cidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcInfoCidrBlockSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlock', 'vpcInfoCidrBlockSetDetails_cidrBlock' - The IPv4 CIDR block for the VPC.
newVpcInfoCidrBlockSetDetails ::
  VpcInfoCidrBlockSetDetails
newVpcInfoCidrBlockSetDetails =
  VpcInfoCidrBlockSetDetails'
    { cidrBlock =
        Prelude.Nothing
    }

-- | The IPv4 CIDR block for the VPC.
vpcInfoCidrBlockSetDetails_cidrBlock :: Lens.Lens' VpcInfoCidrBlockSetDetails (Prelude.Maybe Prelude.Text)
vpcInfoCidrBlockSetDetails_cidrBlock = Lens.lens (\VpcInfoCidrBlockSetDetails' {cidrBlock} -> cidrBlock) (\s@VpcInfoCidrBlockSetDetails' {} a -> s {cidrBlock = a} :: VpcInfoCidrBlockSetDetails)

instance Data.FromJSON VpcInfoCidrBlockSetDetails where
  parseJSON =
    Data.withObject
      "VpcInfoCidrBlockSetDetails"
      ( \x ->
          VpcInfoCidrBlockSetDetails'
            Prelude.<$> (x Data..:? "CidrBlock")
      )

instance Prelude.Hashable VpcInfoCidrBlockSetDetails where
  hashWithSalt _salt VpcInfoCidrBlockSetDetails' {..} =
    _salt `Prelude.hashWithSalt` cidrBlock

instance Prelude.NFData VpcInfoCidrBlockSetDetails where
  rnf VpcInfoCidrBlockSetDetails' {..} =
    Prelude.rnf cidrBlock

instance Data.ToJSON VpcInfoCidrBlockSetDetails where
  toJSON VpcInfoCidrBlockSetDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("CidrBlock" Data..=) Prelude.<$> cidrBlock]
      )
