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
-- Module      : Amazonka.Config.Types.AggregateComplianceCount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateComplianceCount where

import Amazonka.Config.Types.ComplianceSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns the number of compliant and noncompliant rules for one or more
-- accounts and regions in an aggregator.
--
-- /See:/ 'newAggregateComplianceCount' smart constructor.
data AggregateComplianceCount = AggregateComplianceCount'
  { -- | The number of compliant and noncompliant Config rules.
    complianceSummary :: Prelude.Maybe ComplianceSummary,
    -- | The 12-digit account ID or region based on the GroupByKey value.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateComplianceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummary', 'aggregateComplianceCount_complianceSummary' - The number of compliant and noncompliant Config rules.
--
-- 'groupName', 'aggregateComplianceCount_groupName' - The 12-digit account ID or region based on the GroupByKey value.
newAggregateComplianceCount ::
  AggregateComplianceCount
newAggregateComplianceCount =
  AggregateComplianceCount'
    { complianceSummary =
        Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The number of compliant and noncompliant Config rules.
aggregateComplianceCount_complianceSummary :: Lens.Lens' AggregateComplianceCount (Prelude.Maybe ComplianceSummary)
aggregateComplianceCount_complianceSummary = Lens.lens (\AggregateComplianceCount' {complianceSummary} -> complianceSummary) (\s@AggregateComplianceCount' {} a -> s {complianceSummary = a} :: AggregateComplianceCount)

-- | The 12-digit account ID or region based on the GroupByKey value.
aggregateComplianceCount_groupName :: Lens.Lens' AggregateComplianceCount (Prelude.Maybe Prelude.Text)
aggregateComplianceCount_groupName = Lens.lens (\AggregateComplianceCount' {groupName} -> groupName) (\s@AggregateComplianceCount' {} a -> s {groupName = a} :: AggregateComplianceCount)

instance Data.FromJSON AggregateComplianceCount where
  parseJSON =
    Data.withObject
      "AggregateComplianceCount"
      ( \x ->
          AggregateComplianceCount'
            Prelude.<$> (x Data..:? "ComplianceSummary")
            Prelude.<*> (x Data..:? "GroupName")
      )

instance Prelude.Hashable AggregateComplianceCount where
  hashWithSalt _salt AggregateComplianceCount' {..} =
    _salt `Prelude.hashWithSalt` complianceSummary
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData AggregateComplianceCount where
  rnf AggregateComplianceCount' {..} =
    Prelude.rnf complianceSummary
      `Prelude.seq` Prelude.rnf groupName
