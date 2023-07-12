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
-- Module      : Amazonka.Route53.Types.CidrBlockSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.CidrBlockSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that lists the CIDR blocks.
--
-- /See:/ 'newCidrBlockSummary' smart constructor.
data CidrBlockSummary = CidrBlockSummary'
  { -- | Value for the CIDR block.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The location name of the CIDR block.
    locationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CidrBlockSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlock', 'cidrBlockSummary_cidrBlock' - Value for the CIDR block.
--
-- 'locationName', 'cidrBlockSummary_locationName' - The location name of the CIDR block.
newCidrBlockSummary ::
  CidrBlockSummary
newCidrBlockSummary =
  CidrBlockSummary'
    { cidrBlock = Prelude.Nothing,
      locationName = Prelude.Nothing
    }

-- | Value for the CIDR block.
cidrBlockSummary_cidrBlock :: Lens.Lens' CidrBlockSummary (Prelude.Maybe Prelude.Text)
cidrBlockSummary_cidrBlock = Lens.lens (\CidrBlockSummary' {cidrBlock} -> cidrBlock) (\s@CidrBlockSummary' {} a -> s {cidrBlock = a} :: CidrBlockSummary)

-- | The location name of the CIDR block.
cidrBlockSummary_locationName :: Lens.Lens' CidrBlockSummary (Prelude.Maybe Prelude.Text)
cidrBlockSummary_locationName = Lens.lens (\CidrBlockSummary' {locationName} -> locationName) (\s@CidrBlockSummary' {} a -> s {locationName = a} :: CidrBlockSummary)

instance Data.FromXML CidrBlockSummary where
  parseXML x =
    CidrBlockSummary'
      Prelude.<$> (x Data..@? "CidrBlock")
      Prelude.<*> (x Data..@? "LocationName")

instance Prelude.Hashable CidrBlockSummary where
  hashWithSalt _salt CidrBlockSummary' {..} =
    _salt
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` locationName

instance Prelude.NFData CidrBlockSummary where
  rnf CidrBlockSummary' {..} =
    Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf locationName
