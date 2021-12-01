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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackEvaluationFilters where

import Amazonka.Config.Types.ConformancePackComplianceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Filters a conformance pack by Config rule names, compliance types,
-- Amazon Web Services resource types, and resource IDs.
--
-- /See:/ 'newConformancePackEvaluationFilters' smart constructor.
data ConformancePackEvaluationFilters = ConformancePackEvaluationFilters'
  { -- | Filters the results by resource IDs.
    --
    -- This is valid only when you provide resource type. If there is no
    -- resource type, you will see an error.
    resourceIds :: Prelude.Maybe [Prelude.Text],
    -- | Filters the results by the resource type (for example,
    -- @\"AWS::EC2::Instance\"@).
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Filters the results by Config rule names.
    configRuleNames :: Prelude.Maybe [Prelude.Text],
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
    -- @INSUFFICIENT_DATA@ is not supported.
    complianceType :: Prelude.Maybe ConformancePackComplianceType
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
-- 'resourceIds', 'conformancePackEvaluationFilters_resourceIds' - Filters the results by resource IDs.
--
-- This is valid only when you provide resource type. If there is no
-- resource type, you will see an error.
--
-- 'resourceType', 'conformancePackEvaluationFilters_resourceType' - Filters the results by the resource type (for example,
-- @\"AWS::EC2::Instance\"@).
--
-- 'configRuleNames', 'conformancePackEvaluationFilters_configRuleNames' - Filters the results by Config rule names.
--
-- 'complianceType', 'conformancePackEvaluationFilters_complianceType' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
-- @INSUFFICIENT_DATA@ is not supported.
newConformancePackEvaluationFilters ::
  ConformancePackEvaluationFilters
newConformancePackEvaluationFilters =
  ConformancePackEvaluationFilters'
    { resourceIds =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      configRuleNames = Prelude.Nothing,
      complianceType = Prelude.Nothing
    }

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

-- | Filters the results by Config rule names.
conformancePackEvaluationFilters_configRuleNames :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe [Prelude.Text])
conformancePackEvaluationFilters_configRuleNames = Lens.lens (\ConformancePackEvaluationFilters' {configRuleNames} -> configRuleNames) (\s@ConformancePackEvaluationFilters' {} a -> s {configRuleNames = a} :: ConformancePackEvaluationFilters) Prelude.. Lens.mapping Lens.coerced

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
-- @INSUFFICIENT_DATA@ is not supported.
conformancePackEvaluationFilters_complianceType :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe ConformancePackComplianceType)
conformancePackEvaluationFilters_complianceType = Lens.lens (\ConformancePackEvaluationFilters' {complianceType} -> complianceType) (\s@ConformancePackEvaluationFilters' {} a -> s {complianceType = a} :: ConformancePackEvaluationFilters)

instance
  Prelude.Hashable
    ConformancePackEvaluationFilters
  where
  hashWithSalt
    salt'
    ConformancePackEvaluationFilters' {..} =
      salt' `Prelude.hashWithSalt` complianceType
        `Prelude.hashWithSalt` configRuleNames
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` resourceIds

instance
  Prelude.NFData
    ConformancePackEvaluationFilters
  where
  rnf ConformancePackEvaluationFilters' {..} =
    Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf configRuleNames
      `Prelude.seq` Prelude.rnf resourceType

instance Core.ToJSON ConformancePackEvaluationFilters where
  toJSON ConformancePackEvaluationFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceIds" Core..=) Prelude.<$> resourceIds,
            ("ResourceType" Core..=) Prelude.<$> resourceType,
            ("ConfigRuleNames" Core..=)
              Prelude.<$> configRuleNames,
            ("ComplianceType" Core..=)
              Prelude.<$> complianceType
          ]
      )
