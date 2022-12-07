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
-- Module      : Amazonka.NetworkFirewall.Types.CapacityUsageSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.CapacityUsageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.CIDRSummary
import qualified Amazonka.Prelude as Prelude

-- | The capacity usage summary of the resources used by the ReferenceSets in
-- a firewall.
--
-- /See:/ 'newCapacityUsageSummary' smart constructor.
data CapacityUsageSummary = CapacityUsageSummary'
  { -- | Describes the capacity usage of the CIDR blocks used by the IP set
    -- references in a firewall.
    cIDRs :: Prelude.Maybe CIDRSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityUsageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cIDRs', 'capacityUsageSummary_cIDRs' - Describes the capacity usage of the CIDR blocks used by the IP set
-- references in a firewall.
newCapacityUsageSummary ::
  CapacityUsageSummary
newCapacityUsageSummary =
  CapacityUsageSummary' {cIDRs = Prelude.Nothing}

-- | Describes the capacity usage of the CIDR blocks used by the IP set
-- references in a firewall.
capacityUsageSummary_cIDRs :: Lens.Lens' CapacityUsageSummary (Prelude.Maybe CIDRSummary)
capacityUsageSummary_cIDRs = Lens.lens (\CapacityUsageSummary' {cIDRs} -> cIDRs) (\s@CapacityUsageSummary' {} a -> s {cIDRs = a} :: CapacityUsageSummary)

instance Data.FromJSON CapacityUsageSummary where
  parseJSON =
    Data.withObject
      "CapacityUsageSummary"
      ( \x ->
          CapacityUsageSummary'
            Prelude.<$> (x Data..:? "CIDRs")
      )

instance Prelude.Hashable CapacityUsageSummary where
  hashWithSalt _salt CapacityUsageSummary' {..} =
    _salt `Prelude.hashWithSalt` cIDRs

instance Prelude.NFData CapacityUsageSummary where
  rnf CapacityUsageSummary' {..} = Prelude.rnf cIDRs
