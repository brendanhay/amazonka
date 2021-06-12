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
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackComplianceSummary where

import Network.AWS.Config.Types.ConformancePackComplianceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary includes the name and status of the conformance pack.
--
-- /See:/ 'newConformancePackComplianceSummary' smart constructor.
data ConformancePackComplianceSummary = ConformancePackComplianceSummary'
  { -- | The name of the conformance pack name.
    conformancePackName :: Core.Text,
    -- | The status of the conformance pack. The allowed values are COMPLIANT and
    -- NON_COMPLIANT.
    conformancePackComplianceStatus :: ConformancePackComplianceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConformancePackComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackName', 'conformancePackComplianceSummary_conformancePackName' - The name of the conformance pack name.
--
-- 'conformancePackComplianceStatus', 'conformancePackComplianceSummary_conformancePackComplianceStatus' - The status of the conformance pack. The allowed values are COMPLIANT and
-- NON_COMPLIANT.
newConformancePackComplianceSummary ::
  -- | 'conformancePackName'
  Core.Text ->
  -- | 'conformancePackComplianceStatus'
  ConformancePackComplianceType ->
  ConformancePackComplianceSummary
newConformancePackComplianceSummary
  pConformancePackName_
  pConformancePackComplianceStatus_ =
    ConformancePackComplianceSummary'
      { conformancePackName =
          pConformancePackName_,
        conformancePackComplianceStatus =
          pConformancePackComplianceStatus_
      }

-- | The name of the conformance pack name.
conformancePackComplianceSummary_conformancePackName :: Lens.Lens' ConformancePackComplianceSummary Core.Text
conformancePackComplianceSummary_conformancePackName = Lens.lens (\ConformancePackComplianceSummary' {conformancePackName} -> conformancePackName) (\s@ConformancePackComplianceSummary' {} a -> s {conformancePackName = a} :: ConformancePackComplianceSummary)

-- | The status of the conformance pack. The allowed values are COMPLIANT and
-- NON_COMPLIANT.
conformancePackComplianceSummary_conformancePackComplianceStatus :: Lens.Lens' ConformancePackComplianceSummary ConformancePackComplianceType
conformancePackComplianceSummary_conformancePackComplianceStatus = Lens.lens (\ConformancePackComplianceSummary' {conformancePackComplianceStatus} -> conformancePackComplianceStatus) (\s@ConformancePackComplianceSummary' {} a -> s {conformancePackComplianceStatus = a} :: ConformancePackComplianceSummary)

instance
  Core.FromJSON
    ConformancePackComplianceSummary
  where
  parseJSON =
    Core.withObject
      "ConformancePackComplianceSummary"
      ( \x ->
          ConformancePackComplianceSummary'
            Core.<$> (x Core..: "ConformancePackName")
            Core.<*> (x Core..: "ConformancePackComplianceStatus")
      )

instance
  Core.Hashable
    ConformancePackComplianceSummary

instance Core.NFData ConformancePackComplianceSummary
