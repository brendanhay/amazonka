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
-- Module      : Network.AWS.Config.Types.ComplianceSummaryByResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceSummaryByResourceType where

import Network.AWS.Config.Types.ComplianceSummary
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The number of AWS resources of a specific type that are compliant or
-- noncompliant, up to a maximum of 100 for each.
--
-- /See:/ 'newComplianceSummaryByResourceType' smart constructor.
data ComplianceSummaryByResourceType = ComplianceSummaryByResourceType'
  { -- | The number of AWS resources that are compliant or noncompliant, up to a
    -- maximum of 100 for each.
    complianceSummary :: Core.Maybe ComplianceSummary,
    -- | The type of AWS resource.
    resourceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ComplianceSummaryByResourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummary', 'complianceSummaryByResourceType_complianceSummary' - The number of AWS resources that are compliant or noncompliant, up to a
-- maximum of 100 for each.
--
-- 'resourceType', 'complianceSummaryByResourceType_resourceType' - The type of AWS resource.
newComplianceSummaryByResourceType ::
  ComplianceSummaryByResourceType
newComplianceSummaryByResourceType =
  ComplianceSummaryByResourceType'
    { complianceSummary =
        Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The number of AWS resources that are compliant or noncompliant, up to a
-- maximum of 100 for each.
complianceSummaryByResourceType_complianceSummary :: Lens.Lens' ComplianceSummaryByResourceType (Core.Maybe ComplianceSummary)
complianceSummaryByResourceType_complianceSummary = Lens.lens (\ComplianceSummaryByResourceType' {complianceSummary} -> complianceSummary) (\s@ComplianceSummaryByResourceType' {} a -> s {complianceSummary = a} :: ComplianceSummaryByResourceType)

-- | The type of AWS resource.
complianceSummaryByResourceType_resourceType :: Lens.Lens' ComplianceSummaryByResourceType (Core.Maybe Core.Text)
complianceSummaryByResourceType_resourceType = Lens.lens (\ComplianceSummaryByResourceType' {resourceType} -> resourceType) (\s@ComplianceSummaryByResourceType' {} a -> s {resourceType = a} :: ComplianceSummaryByResourceType)

instance
  Core.FromJSON
    ComplianceSummaryByResourceType
  where
  parseJSON =
    Core.withObject
      "ComplianceSummaryByResourceType"
      ( \x ->
          ComplianceSummaryByResourceType'
            Core.<$> (x Core..:? "ComplianceSummary")
            Core.<*> (x Core..:? "ResourceType")
      )

instance
  Core.Hashable
    ComplianceSummaryByResourceType

instance Core.NFData ComplianceSummaryByResourceType
