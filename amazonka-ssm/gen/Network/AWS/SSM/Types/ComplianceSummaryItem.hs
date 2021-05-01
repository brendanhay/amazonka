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
-- Module      : Network.AWS.SSM.Types.ComplianceSummaryItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceSummaryItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.NonCompliantSummary

-- | A summary of compliance information by compliance type.
--
-- /See:/ 'newComplianceSummaryItem' smart constructor.
data ComplianceSummaryItem = ComplianceSummaryItem'
  { -- | A list of COMPLIANT items for the specified compliance type.
    compliantSummary :: Prelude.Maybe CompliantSummary,
    -- | The type of compliance item. For example, the compliance type can be
    -- Association, Patch, or Custom:string.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | A list of NON_COMPLIANT items for the specified compliance type.
    nonCompliantSummary :: Prelude.Maybe NonCompliantSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ComplianceSummaryItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compliantSummary', 'complianceSummaryItem_compliantSummary' - A list of COMPLIANT items for the specified compliance type.
--
-- 'complianceType', 'complianceSummaryItem_complianceType' - The type of compliance item. For example, the compliance type can be
-- Association, Patch, or Custom:string.
--
-- 'nonCompliantSummary', 'complianceSummaryItem_nonCompliantSummary' - A list of NON_COMPLIANT items for the specified compliance type.
newComplianceSummaryItem ::
  ComplianceSummaryItem
newComplianceSummaryItem =
  ComplianceSummaryItem'
    { compliantSummary =
        Prelude.Nothing,
      complianceType = Prelude.Nothing,
      nonCompliantSummary = Prelude.Nothing
    }

-- | A list of COMPLIANT items for the specified compliance type.
complianceSummaryItem_compliantSummary :: Lens.Lens' ComplianceSummaryItem (Prelude.Maybe CompliantSummary)
complianceSummaryItem_compliantSummary = Lens.lens (\ComplianceSummaryItem' {compliantSummary} -> compliantSummary) (\s@ComplianceSummaryItem' {} a -> s {compliantSummary = a} :: ComplianceSummaryItem)

-- | The type of compliance item. For example, the compliance type can be
-- Association, Patch, or Custom:string.
complianceSummaryItem_complianceType :: Lens.Lens' ComplianceSummaryItem (Prelude.Maybe Prelude.Text)
complianceSummaryItem_complianceType = Lens.lens (\ComplianceSummaryItem' {complianceType} -> complianceType) (\s@ComplianceSummaryItem' {} a -> s {complianceType = a} :: ComplianceSummaryItem)

-- | A list of NON_COMPLIANT items for the specified compliance type.
complianceSummaryItem_nonCompliantSummary :: Lens.Lens' ComplianceSummaryItem (Prelude.Maybe NonCompliantSummary)
complianceSummaryItem_nonCompliantSummary = Lens.lens (\ComplianceSummaryItem' {nonCompliantSummary} -> nonCompliantSummary) (\s@ComplianceSummaryItem' {} a -> s {nonCompliantSummary = a} :: ComplianceSummaryItem)

instance Prelude.FromJSON ComplianceSummaryItem where
  parseJSON =
    Prelude.withObject
      "ComplianceSummaryItem"
      ( \x ->
          ComplianceSummaryItem'
            Prelude.<$> (x Prelude..:? "CompliantSummary")
            Prelude.<*> (x Prelude..:? "ComplianceType")
            Prelude.<*> (x Prelude..:? "NonCompliantSummary")
      )

instance Prelude.Hashable ComplianceSummaryItem

instance Prelude.NFData ComplianceSummaryItem
