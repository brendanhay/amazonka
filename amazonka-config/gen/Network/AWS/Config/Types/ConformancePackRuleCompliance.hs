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
-- Module      : Network.AWS.Config.Types.ConformancePackRuleCompliance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackRuleCompliance where

import Network.AWS.Config.Types.ConformancePackComplianceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Compliance information of one or more AWS Config rules within a
-- conformance pack. You can filter using AWS Config rule names and
-- compliance types.
--
-- /See:/ 'newConformancePackRuleCompliance' smart constructor.
data ConformancePackRuleCompliance = ConformancePackRuleCompliance'
  { -- | Name of the config rule.
    configRuleName :: Core.Maybe Core.Text,
    -- | Compliance of the AWS Config rule
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
    complianceType :: Core.Maybe ConformancePackComplianceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConformancePackRuleCompliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'conformancePackRuleCompliance_configRuleName' - Name of the config rule.
--
-- 'complianceType', 'conformancePackRuleCompliance_complianceType' - Compliance of the AWS Config rule
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
newConformancePackRuleCompliance ::
  ConformancePackRuleCompliance
newConformancePackRuleCompliance =
  ConformancePackRuleCompliance'
    { configRuleName =
        Core.Nothing,
      complianceType = Core.Nothing
    }

-- | Name of the config rule.
conformancePackRuleCompliance_configRuleName :: Lens.Lens' ConformancePackRuleCompliance (Core.Maybe Core.Text)
conformancePackRuleCompliance_configRuleName = Lens.lens (\ConformancePackRuleCompliance' {configRuleName} -> configRuleName) (\s@ConformancePackRuleCompliance' {} a -> s {configRuleName = a} :: ConformancePackRuleCompliance)

-- | Compliance of the AWS Config rule
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
conformancePackRuleCompliance_complianceType :: Lens.Lens' ConformancePackRuleCompliance (Core.Maybe ConformancePackComplianceType)
conformancePackRuleCompliance_complianceType = Lens.lens (\ConformancePackRuleCompliance' {complianceType} -> complianceType) (\s@ConformancePackRuleCompliance' {} a -> s {complianceType = a} :: ConformancePackRuleCompliance)

instance Core.FromJSON ConformancePackRuleCompliance where
  parseJSON =
    Core.withObject
      "ConformancePackRuleCompliance"
      ( \x ->
          ConformancePackRuleCompliance'
            Core.<$> (x Core..:? "ConfigRuleName")
            Core.<*> (x Core..:? "ComplianceType")
      )

instance Core.Hashable ConformancePackRuleCompliance

instance Core.NFData ConformancePackRuleCompliance
