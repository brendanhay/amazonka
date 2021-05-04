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
-- Module      : Network.AWS.Config.Types.ConformancePackRuleCompliance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackRuleCompliance where

import Network.AWS.Config.Types.ConformancePackComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Compliance information of one or more AWS Config rules within a
-- conformance pack. You can filter using AWS Config rule names and
-- compliance types.
--
-- /See:/ 'newConformancePackRuleCompliance' smart constructor.
data ConformancePackRuleCompliance = ConformancePackRuleCompliance'
  { -- | Name of the config rule.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | Compliance of the AWS Config rule
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
    complianceType :: Prelude.Maybe ConformancePackComplianceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      complianceType = Prelude.Nothing
    }

-- | Name of the config rule.
conformancePackRuleCompliance_configRuleName :: Lens.Lens' ConformancePackRuleCompliance (Prelude.Maybe Prelude.Text)
conformancePackRuleCompliance_configRuleName = Lens.lens (\ConformancePackRuleCompliance' {configRuleName} -> configRuleName) (\s@ConformancePackRuleCompliance' {} a -> s {configRuleName = a} :: ConformancePackRuleCompliance)

-- | Compliance of the AWS Config rule
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
conformancePackRuleCompliance_complianceType :: Lens.Lens' ConformancePackRuleCompliance (Prelude.Maybe ConformancePackComplianceType)
conformancePackRuleCompliance_complianceType = Lens.lens (\ConformancePackRuleCompliance' {complianceType} -> complianceType) (\s@ConformancePackRuleCompliance' {} a -> s {complianceType = a} :: ConformancePackRuleCompliance)

instance
  Prelude.FromJSON
    ConformancePackRuleCompliance
  where
  parseJSON =
    Prelude.withObject
      "ConformancePackRuleCompliance"
      ( \x ->
          ConformancePackRuleCompliance'
            Prelude.<$> (x Prelude..:? "ConfigRuleName")
            Prelude.<*> (x Prelude..:? "ComplianceType")
      )

instance
  Prelude.Hashable
    ConformancePackRuleCompliance

instance Prelude.NFData ConformancePackRuleCompliance
