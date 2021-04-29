{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.Types.AggregateComplianceCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateComplianceCount where

import Network.AWS.Config.Types.ComplianceSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns the number of compliant and noncompliant rules for one or more
-- accounts and regions in an aggregator.
--
-- /See:/ 'newAggregateComplianceCount' smart constructor.
data AggregateComplianceCount = AggregateComplianceCount'
  { -- | The number of compliant and noncompliant AWS Config rules.
    complianceSummary :: Prelude.Maybe ComplianceSummary,
    -- | The 12-digit account ID or region based on the GroupByKey value.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AggregateComplianceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummary', 'aggregateComplianceCount_complianceSummary' - The number of compliant and noncompliant AWS Config rules.
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

-- | The number of compliant and noncompliant AWS Config rules.
aggregateComplianceCount_complianceSummary :: Lens.Lens' AggregateComplianceCount (Prelude.Maybe ComplianceSummary)
aggregateComplianceCount_complianceSummary = Lens.lens (\AggregateComplianceCount' {complianceSummary} -> complianceSummary) (\s@AggregateComplianceCount' {} a -> s {complianceSummary = a} :: AggregateComplianceCount)

-- | The 12-digit account ID or region based on the GroupByKey value.
aggregateComplianceCount_groupName :: Lens.Lens' AggregateComplianceCount (Prelude.Maybe Prelude.Text)
aggregateComplianceCount_groupName = Lens.lens (\AggregateComplianceCount' {groupName} -> groupName) (\s@AggregateComplianceCount' {} a -> s {groupName = a} :: AggregateComplianceCount)

instance Prelude.FromJSON AggregateComplianceCount where
  parseJSON =
    Prelude.withObject
      "AggregateComplianceCount"
      ( \x ->
          AggregateComplianceCount'
            Prelude.<$> (x Prelude..:? "ComplianceSummary")
            Prelude.<*> (x Prelude..:? "GroupName")
      )

instance Prelude.Hashable AggregateComplianceCount

instance Prelude.NFData AggregateComplianceCount
