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
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackComplianceFilters where

import Network.AWS.Config.Types.ConformancePackComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filters the conformance pack by compliance types and AWS Config rule
-- names.
--
-- /See:/ 'newConformancePackComplianceFilters' smart constructor.
data ConformancePackComplianceFilters = ConformancePackComplianceFilters'
  { -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
    complianceType :: Prelude.Maybe ConformancePackComplianceType,
    -- | Filters the results by AWS Config rule names.
    configRuleNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConformancePackComplianceFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceType', 'conformancePackComplianceFilters_complianceType' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
--
-- 'configRuleNames', 'conformancePackComplianceFilters_configRuleNames' - Filters the results by AWS Config rule names.
newConformancePackComplianceFilters ::
  ConformancePackComplianceFilters
newConformancePackComplianceFilters =
  ConformancePackComplianceFilters'
    { complianceType =
        Prelude.Nothing,
      configRuleNames = Prelude.Nothing
    }

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
conformancePackComplianceFilters_complianceType :: Lens.Lens' ConformancePackComplianceFilters (Prelude.Maybe ConformancePackComplianceType)
conformancePackComplianceFilters_complianceType = Lens.lens (\ConformancePackComplianceFilters' {complianceType} -> complianceType) (\s@ConformancePackComplianceFilters' {} a -> s {complianceType = a} :: ConformancePackComplianceFilters)

-- | Filters the results by AWS Config rule names.
conformancePackComplianceFilters_configRuleNames :: Lens.Lens' ConformancePackComplianceFilters (Prelude.Maybe [Prelude.Text])
conformancePackComplianceFilters_configRuleNames = Lens.lens (\ConformancePackComplianceFilters' {configRuleNames} -> configRuleNames) (\s@ConformancePackComplianceFilters' {} a -> s {configRuleNames = a} :: ConformancePackComplianceFilters) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.Hashable
    ConformancePackComplianceFilters

instance
  Prelude.NFData
    ConformancePackComplianceFilters

instance
  Prelude.ToJSON
    ConformancePackComplianceFilters
  where
  toJSON ConformancePackComplianceFilters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ComplianceType" Prelude..=)
              Prelude.<$> complianceType,
            ("ConfigRuleNames" Prelude..=)
              Prelude.<$> configRuleNames
          ]
      )
