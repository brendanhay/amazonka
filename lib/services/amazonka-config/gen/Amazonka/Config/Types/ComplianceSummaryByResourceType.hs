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
-- Module      : Amazonka.Config.Types.ComplianceSummaryByResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ComplianceSummaryByResourceType where

import Amazonka.Config.Types.ComplianceSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The number of Amazon Web Services resources of a specific type that are
-- compliant or noncompliant, up to a maximum of 100 for each.
--
-- /See:/ 'newComplianceSummaryByResourceType' smart constructor.
data ComplianceSummaryByResourceType = ComplianceSummaryByResourceType'
  { -- | The number of Amazon Web Services resources that are compliant or
    -- noncompliant, up to a maximum of 100 for each.
    complianceSummary :: Prelude.Maybe ComplianceSummary,
    -- | The type of Amazon Web Services resource.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComplianceSummaryByResourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummary', 'complianceSummaryByResourceType_complianceSummary' - The number of Amazon Web Services resources that are compliant or
-- noncompliant, up to a maximum of 100 for each.
--
-- 'resourceType', 'complianceSummaryByResourceType_resourceType' - The type of Amazon Web Services resource.
newComplianceSummaryByResourceType ::
  ComplianceSummaryByResourceType
newComplianceSummaryByResourceType =
  ComplianceSummaryByResourceType'
    { complianceSummary =
        Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The number of Amazon Web Services resources that are compliant or
-- noncompliant, up to a maximum of 100 for each.
complianceSummaryByResourceType_complianceSummary :: Lens.Lens' ComplianceSummaryByResourceType (Prelude.Maybe ComplianceSummary)
complianceSummaryByResourceType_complianceSummary = Lens.lens (\ComplianceSummaryByResourceType' {complianceSummary} -> complianceSummary) (\s@ComplianceSummaryByResourceType' {} a -> s {complianceSummary = a} :: ComplianceSummaryByResourceType)

-- | The type of Amazon Web Services resource.
complianceSummaryByResourceType_resourceType :: Lens.Lens' ComplianceSummaryByResourceType (Prelude.Maybe Prelude.Text)
complianceSummaryByResourceType_resourceType = Lens.lens (\ComplianceSummaryByResourceType' {resourceType} -> resourceType) (\s@ComplianceSummaryByResourceType' {} a -> s {resourceType = a} :: ComplianceSummaryByResourceType)

instance
  Data.FromJSON
    ComplianceSummaryByResourceType
  where
  parseJSON =
    Data.withObject
      "ComplianceSummaryByResourceType"
      ( \x ->
          ComplianceSummaryByResourceType'
            Prelude.<$> (x Data..:? "ComplianceSummary")
            Prelude.<*> (x Data..:? "ResourceType")
      )

instance
  Prelude.Hashable
    ComplianceSummaryByResourceType
  where
  hashWithSalt
    _salt
    ComplianceSummaryByResourceType' {..} =
      _salt
        `Prelude.hashWithSalt` complianceSummary
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    ComplianceSummaryByResourceType
  where
  rnf ComplianceSummaryByResourceType' {..} =
    Prelude.rnf complianceSummary
      `Prelude.seq` Prelude.rnf resourceType
