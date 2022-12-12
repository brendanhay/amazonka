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
-- Module      : Amazonka.EC2.Types.AccessScopeAnalysisFinding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AccessScopeAnalysisFinding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PathComponent
import qualified Amazonka.Prelude as Prelude

-- | Describes a finding for a Network Access Scope.
--
-- /See:/ 'newAccessScopeAnalysisFinding' smart constructor.
data AccessScopeAnalysisFinding = AccessScopeAnalysisFinding'
  { -- | The finding components.
    findingComponents :: Prelude.Maybe [PathComponent],
    -- | The ID of the finding.
    findingId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Network Access Scope analysis.
    networkInsightsAccessScopeAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Network Access Scope.
    networkInsightsAccessScopeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessScopeAnalysisFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingComponents', 'accessScopeAnalysisFinding_findingComponents' - The finding components.
--
-- 'findingId', 'accessScopeAnalysisFinding_findingId' - The ID of the finding.
--
-- 'networkInsightsAccessScopeAnalysisId', 'accessScopeAnalysisFinding_networkInsightsAccessScopeAnalysisId' - The ID of the Network Access Scope analysis.
--
-- 'networkInsightsAccessScopeId', 'accessScopeAnalysisFinding_networkInsightsAccessScopeId' - The ID of the Network Access Scope.
newAccessScopeAnalysisFinding ::
  AccessScopeAnalysisFinding
newAccessScopeAnalysisFinding =
  AccessScopeAnalysisFinding'
    { findingComponents =
        Prelude.Nothing,
      findingId = Prelude.Nothing,
      networkInsightsAccessScopeAnalysisId =
        Prelude.Nothing,
      networkInsightsAccessScopeId = Prelude.Nothing
    }

-- | The finding components.
accessScopeAnalysisFinding_findingComponents :: Lens.Lens' AccessScopeAnalysisFinding (Prelude.Maybe [PathComponent])
accessScopeAnalysisFinding_findingComponents = Lens.lens (\AccessScopeAnalysisFinding' {findingComponents} -> findingComponents) (\s@AccessScopeAnalysisFinding' {} a -> s {findingComponents = a} :: AccessScopeAnalysisFinding) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the finding.
accessScopeAnalysisFinding_findingId :: Lens.Lens' AccessScopeAnalysisFinding (Prelude.Maybe Prelude.Text)
accessScopeAnalysisFinding_findingId = Lens.lens (\AccessScopeAnalysisFinding' {findingId} -> findingId) (\s@AccessScopeAnalysisFinding' {} a -> s {findingId = a} :: AccessScopeAnalysisFinding)

-- | The ID of the Network Access Scope analysis.
accessScopeAnalysisFinding_networkInsightsAccessScopeAnalysisId :: Lens.Lens' AccessScopeAnalysisFinding (Prelude.Maybe Prelude.Text)
accessScopeAnalysisFinding_networkInsightsAccessScopeAnalysisId = Lens.lens (\AccessScopeAnalysisFinding' {networkInsightsAccessScopeAnalysisId} -> networkInsightsAccessScopeAnalysisId) (\s@AccessScopeAnalysisFinding' {} a -> s {networkInsightsAccessScopeAnalysisId = a} :: AccessScopeAnalysisFinding)

-- | The ID of the Network Access Scope.
accessScopeAnalysisFinding_networkInsightsAccessScopeId :: Lens.Lens' AccessScopeAnalysisFinding (Prelude.Maybe Prelude.Text)
accessScopeAnalysisFinding_networkInsightsAccessScopeId = Lens.lens (\AccessScopeAnalysisFinding' {networkInsightsAccessScopeId} -> networkInsightsAccessScopeId) (\s@AccessScopeAnalysisFinding' {} a -> s {networkInsightsAccessScopeId = a} :: AccessScopeAnalysisFinding)

instance Data.FromXML AccessScopeAnalysisFinding where
  parseXML x =
    AccessScopeAnalysisFinding'
      Prelude.<$> ( x Data..@? "findingComponentSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "findingId")
      Prelude.<*> (x Data..@? "networkInsightsAccessScopeAnalysisId")
      Prelude.<*> (x Data..@? "networkInsightsAccessScopeId")

instance Prelude.Hashable AccessScopeAnalysisFinding where
  hashWithSalt _salt AccessScopeAnalysisFinding' {..} =
    _salt `Prelude.hashWithSalt` findingComponents
      `Prelude.hashWithSalt` findingId
      `Prelude.hashWithSalt` networkInsightsAccessScopeAnalysisId
      `Prelude.hashWithSalt` networkInsightsAccessScopeId

instance Prelude.NFData AccessScopeAnalysisFinding where
  rnf AccessScopeAnalysisFinding' {..} =
    Prelude.rnf findingComponents
      `Prelude.seq` Prelude.rnf findingId
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeAnalysisId
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeId
