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
-- Module      : Amazonka.NetworkFirewall.Types.CIDRSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.CIDRSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.IPSetMetadata
import qualified Amazonka.Prelude as Prelude

-- | Summarizes the CIDR blocks used by the IP set references in a firewall.
-- Network Firewall calculates the number of CIDRs by taking an aggregated
-- count of all CIDRs used by the IP sets you are referencing.
--
-- /See:/ 'newCIDRSummary' smart constructor.
data CIDRSummary = CIDRSummary'
  { -- | The number of CIDR blocks available for use by the IP set references in
    -- a firewall.
    availableCIDRCount :: Prelude.Maybe Prelude.Natural,
    -- | The list of the IP set references used by a firewall.
    iPSetReferences :: Prelude.Maybe (Prelude.HashMap Prelude.Text IPSetMetadata),
    -- | The number of CIDR blocks used by the IP set references in a firewall.
    utilizedCIDRCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CIDRSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availableCIDRCount', 'cIDRSummary_availableCIDRCount' - The number of CIDR blocks available for use by the IP set references in
-- a firewall.
--
-- 'iPSetReferences', 'cIDRSummary_iPSetReferences' - The list of the IP set references used by a firewall.
--
-- 'utilizedCIDRCount', 'cIDRSummary_utilizedCIDRCount' - The number of CIDR blocks used by the IP set references in a firewall.
newCIDRSummary ::
  CIDRSummary
newCIDRSummary =
  CIDRSummary'
    { availableCIDRCount = Prelude.Nothing,
      iPSetReferences = Prelude.Nothing,
      utilizedCIDRCount = Prelude.Nothing
    }

-- | The number of CIDR blocks available for use by the IP set references in
-- a firewall.
cIDRSummary_availableCIDRCount :: Lens.Lens' CIDRSummary (Prelude.Maybe Prelude.Natural)
cIDRSummary_availableCIDRCount = Lens.lens (\CIDRSummary' {availableCIDRCount} -> availableCIDRCount) (\s@CIDRSummary' {} a -> s {availableCIDRCount = a} :: CIDRSummary)

-- | The list of the IP set references used by a firewall.
cIDRSummary_iPSetReferences :: Lens.Lens' CIDRSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text IPSetMetadata))
cIDRSummary_iPSetReferences = Lens.lens (\CIDRSummary' {iPSetReferences} -> iPSetReferences) (\s@CIDRSummary' {} a -> s {iPSetReferences = a} :: CIDRSummary) Prelude.. Lens.mapping Lens.coerced

-- | The number of CIDR blocks used by the IP set references in a firewall.
cIDRSummary_utilizedCIDRCount :: Lens.Lens' CIDRSummary (Prelude.Maybe Prelude.Natural)
cIDRSummary_utilizedCIDRCount = Lens.lens (\CIDRSummary' {utilizedCIDRCount} -> utilizedCIDRCount) (\s@CIDRSummary' {} a -> s {utilizedCIDRCount = a} :: CIDRSummary)

instance Data.FromJSON CIDRSummary where
  parseJSON =
    Data.withObject
      "CIDRSummary"
      ( \x ->
          CIDRSummary'
            Prelude.<$> (x Data..:? "AvailableCIDRCount")
            Prelude.<*> ( x Data..:? "IPSetReferences"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UtilizedCIDRCount")
      )

instance Prelude.Hashable CIDRSummary where
  hashWithSalt _salt CIDRSummary' {..} =
    _salt `Prelude.hashWithSalt` availableCIDRCount
      `Prelude.hashWithSalt` iPSetReferences
      `Prelude.hashWithSalt` utilizedCIDRCount

instance Prelude.NFData CIDRSummary where
  rnf CIDRSummary' {..} =
    Prelude.rnf availableCIDRCount
      `Prelude.seq` Prelude.rnf iPSetReferences
      `Prelude.seq` Prelude.rnf utilizedCIDRCount
