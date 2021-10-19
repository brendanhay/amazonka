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
import qualified Network.AWS.Prelude as Prelude

-- | Compliance information of one or more Config rules within a conformance
-- pack. You can filter using Config rule names and compliance types.
--
-- /See:/ 'newConformancePackRuleCompliance' smart constructor.
data ConformancePackRuleCompliance = ConformancePackRuleCompliance'
  { -- | Controls for the conformance pack. A control is a process to prevent or
    -- detect problems while meeting objectives. A control can align with a
    -- specific compliance regime or map to internal controls defined by an
    -- organization.
    controls :: Prelude.Maybe [Prelude.Text],
    -- | Name of the config rule.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | Compliance of the Config rule.
    --
    -- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
    -- @INSUFFICIENT_DATA@.
    complianceType :: Prelude.Maybe ConformancePackComplianceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConformancePackRuleCompliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controls', 'conformancePackRuleCompliance_controls' - Controls for the conformance pack. A control is a process to prevent or
-- detect problems while meeting objectives. A control can align with a
-- specific compliance regime or map to internal controls defined by an
-- organization.
--
-- 'configRuleName', 'conformancePackRuleCompliance_configRuleName' - Name of the config rule.
--
-- 'complianceType', 'conformancePackRuleCompliance_complianceType' - Compliance of the Config rule.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @INSUFFICIENT_DATA@.
newConformancePackRuleCompliance ::
  ConformancePackRuleCompliance
newConformancePackRuleCompliance =
  ConformancePackRuleCompliance'
    { controls =
        Prelude.Nothing,
      configRuleName = Prelude.Nothing,
      complianceType = Prelude.Nothing
    }

-- | Controls for the conformance pack. A control is a process to prevent or
-- detect problems while meeting objectives. A control can align with a
-- specific compliance regime or map to internal controls defined by an
-- organization.
conformancePackRuleCompliance_controls :: Lens.Lens' ConformancePackRuleCompliance (Prelude.Maybe [Prelude.Text])
conformancePackRuleCompliance_controls = Lens.lens (\ConformancePackRuleCompliance' {controls} -> controls) (\s@ConformancePackRuleCompliance' {} a -> s {controls = a} :: ConformancePackRuleCompliance) Prelude.. Lens.mapping Lens.coerced

-- | Name of the config rule.
conformancePackRuleCompliance_configRuleName :: Lens.Lens' ConformancePackRuleCompliance (Prelude.Maybe Prelude.Text)
conformancePackRuleCompliance_configRuleName = Lens.lens (\ConformancePackRuleCompliance' {configRuleName} -> configRuleName) (\s@ConformancePackRuleCompliance' {} a -> s {configRuleName = a} :: ConformancePackRuleCompliance)

-- | Compliance of the Config rule.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @INSUFFICIENT_DATA@.
conformancePackRuleCompliance_complianceType :: Lens.Lens' ConformancePackRuleCompliance (Prelude.Maybe ConformancePackComplianceType)
conformancePackRuleCompliance_complianceType = Lens.lens (\ConformancePackRuleCompliance' {complianceType} -> complianceType) (\s@ConformancePackRuleCompliance' {} a -> s {complianceType = a} :: ConformancePackRuleCompliance)

instance Core.FromJSON ConformancePackRuleCompliance where
  parseJSON =
    Core.withObject
      "ConformancePackRuleCompliance"
      ( \x ->
          ConformancePackRuleCompliance'
            Prelude.<$> (x Core..:? "Controls" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ConfigRuleName")
            Prelude.<*> (x Core..:? "ComplianceType")
      )

instance
  Prelude.Hashable
    ConformancePackRuleCompliance

instance Prelude.NFData ConformancePackRuleCompliance
