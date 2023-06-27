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
-- Module      : Amazonka.Config.Types.ConformancePackEvaluationFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackEvaluationFilters where

import Amazonka.Config.Types.ConformancePackComplianceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters a conformance pack by Config rule names, compliance types,
-- Amazon Web Services resource types, and resource IDs.
--
-- /See:/ 'newConformancePackEvaluationFilters' smart constructor.
data ConformancePackEvaluationFilters = ConformancePackEvaluationFilters'
  { -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
    -- @INSUFFICIENT_DATA@ is not supported.
    complianceType :: Prelude.Maybe ConformancePackComplianceType,
    -- | Filters the results by Config rule names.
    configRuleNames :: Prelude.Maybe [Prelude.Text],
    -- | Filters the results by resource IDs.
    --
    -- This is valid only when you provide resource type. If there is no
    -- resource type, you will see an error.
    resourceIds :: Prelude.Maybe [Prelude.Text],
    -- | Filters the results by the resource type (for example,
    -- @\"AWS::EC2::Instance\"@).
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConformancePackEvaluationFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceType', 'conformancePackEvaluationFilters_complianceType' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
-- @INSUFFICIENT_DATA@ is not supported.
--
-- 'configRuleNames', 'conformancePackEvaluationFilters_configRuleNames' - Filters the results by Config rule names.
--
-- 'resourceIds', 'conformancePackEvaluationFilters_resourceIds' - Filters the results by resource IDs.
--
-- This is valid only when you provide resource type. If there is no
-- resource type, you will see an error.
--
-- 'resourceType', 'conformancePackEvaluationFilters_resourceType' - Filters the results by the resource type (for example,
-- @\"AWS::EC2::Instance\"@).
newConformancePackEvaluationFilters ::
  ConformancePackEvaluationFilters
newConformancePackEvaluationFilters =
  ConformancePackEvaluationFilters'
    { complianceType =
        Prelude.Nothing,
      configRuleNames = Prelude.Nothing,
      resourceIds = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
-- @INSUFFICIENT_DATA@ is not supported.
conformancePackEvaluationFilters_complianceType :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe ConformancePackComplianceType)
conformancePackEvaluationFilters_complianceType = Lens.lens (\ConformancePackEvaluationFilters' {complianceType} -> complianceType) (\s@ConformancePackEvaluationFilters' {} a -> s {complianceType = a} :: ConformancePackEvaluationFilters)

-- | Filters the results by Config rule names.
conformancePackEvaluationFilters_configRuleNames :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe [Prelude.Text])
conformancePackEvaluationFilters_configRuleNames = Lens.lens (\ConformancePackEvaluationFilters' {configRuleNames} -> configRuleNames) (\s@ConformancePackEvaluationFilters' {} a -> s {configRuleNames = a} :: ConformancePackEvaluationFilters) Prelude.. Lens.mapping Lens.coerced

-- | Filters the results by resource IDs.
--
-- This is valid only when you provide resource type. If there is no
-- resource type, you will see an error.
conformancePackEvaluationFilters_resourceIds :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe [Prelude.Text])
conformancePackEvaluationFilters_resourceIds = Lens.lens (\ConformancePackEvaluationFilters' {resourceIds} -> resourceIds) (\s@ConformancePackEvaluationFilters' {} a -> s {resourceIds = a} :: ConformancePackEvaluationFilters) Prelude.. Lens.mapping Lens.coerced

-- | Filters the results by the resource type (for example,
-- @\"AWS::EC2::Instance\"@).
conformancePackEvaluationFilters_resourceType :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe Prelude.Text)
conformancePackEvaluationFilters_resourceType = Lens.lens (\ConformancePackEvaluationFilters' {resourceType} -> resourceType) (\s@ConformancePackEvaluationFilters' {} a -> s {resourceType = a} :: ConformancePackEvaluationFilters)

instance
  Prelude.Hashable
    ConformancePackEvaluationFilters
  where
  hashWithSalt
    _salt
    ConformancePackEvaluationFilters' {..} =
      _salt
        `Prelude.hashWithSalt` complianceType
        `Prelude.hashWithSalt` configRuleNames
        `Prelude.hashWithSalt` resourceIds
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    ConformancePackEvaluationFilters
  where
  rnf ConformancePackEvaluationFilters' {..} =
    Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf configRuleNames
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToJSON ConformancePackEvaluationFilters where
  toJSON ConformancePackEvaluationFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComplianceType" Data..=)
              Prelude.<$> complianceType,
            ("ConfigRuleNames" Data..=)
              Prelude.<$> configRuleNames,
            ("ResourceIds" Data..=) Prelude.<$> resourceIds,
            ("ResourceType" Data..=) Prelude.<$> resourceType
          ]
      )
