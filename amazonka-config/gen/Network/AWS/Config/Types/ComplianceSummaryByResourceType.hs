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
-- Module      : Network.AWS.Config.Types.ComplianceSummaryByResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceSummaryByResourceType where

import Network.AWS.Config.Types.ComplianceSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The number of AWS resources of a specific type that are compliant or
-- noncompliant, up to a maximum of 100 for each.
--
-- /See:/ 'newComplianceSummaryByResourceType' smart constructor.
data ComplianceSummaryByResourceType = ComplianceSummaryByResourceType'
  { -- | The number of AWS resources that are compliant or noncompliant, up to a
    -- maximum of 100 for each.
    complianceSummary :: Prelude.Maybe ComplianceSummary,
    -- | The type of AWS resource.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The number of AWS resources that are compliant or noncompliant, up to a
-- maximum of 100 for each.
complianceSummaryByResourceType_complianceSummary :: Lens.Lens' ComplianceSummaryByResourceType (Prelude.Maybe ComplianceSummary)
complianceSummaryByResourceType_complianceSummary = Lens.lens (\ComplianceSummaryByResourceType' {complianceSummary} -> complianceSummary) (\s@ComplianceSummaryByResourceType' {} a -> s {complianceSummary = a} :: ComplianceSummaryByResourceType)

-- | The type of AWS resource.
complianceSummaryByResourceType_resourceType :: Lens.Lens' ComplianceSummaryByResourceType (Prelude.Maybe Prelude.Text)
complianceSummaryByResourceType_resourceType = Lens.lens (\ComplianceSummaryByResourceType' {resourceType} -> resourceType) (\s@ComplianceSummaryByResourceType' {} a -> s {resourceType = a} :: ComplianceSummaryByResourceType)

instance
  Prelude.FromJSON
    ComplianceSummaryByResourceType
  where
  parseJSON =
    Prelude.withObject
      "ComplianceSummaryByResourceType"
      ( \x ->
          ComplianceSummaryByResourceType'
            Prelude.<$> (x Prelude..:? "ComplianceSummary")
            Prelude.<*> (x Prelude..:? "ResourceType")
      )

instance
  Prelude.Hashable
    ComplianceSummaryByResourceType

instance
  Prelude.NFData
    ComplianceSummaryByResourceType
