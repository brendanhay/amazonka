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
-- Module      : Network.AWS.Config.Types.ConformancePackEvaluationFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackEvaluationFilters where

import Network.AWS.Config.Types.ConformancePackComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filters a conformance pack by AWS Config rule names, compliance types,
-- AWS resource types, and resource IDs.
--
-- /See:/ 'newConformancePackEvaluationFilters' smart constructor.
data ConformancePackEvaluationFilters = ConformancePackEvaluationFilters'
  { -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
    complianceType :: Prelude.Maybe ConformancePackComplianceType,
    -- | Filters the results by the resource type (for example,
    -- @\"AWS::EC2::Instance\"@).
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Filters the results by resource IDs.
    --
    -- This is valid only when you provide resource type. If there is no
    -- resource type, you will see an error.
    resourceIds :: Prelude.Maybe [Prelude.Text],
    -- | Filters the results by AWS Config rule names.
    configRuleNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
--
-- 'resourceType', 'conformancePackEvaluationFilters_resourceType' - Filters the results by the resource type (for example,
-- @\"AWS::EC2::Instance\"@).
--
-- 'resourceIds', 'conformancePackEvaluationFilters_resourceIds' - Filters the results by resource IDs.
--
-- This is valid only when you provide resource type. If there is no
-- resource type, you will see an error.
--
-- 'configRuleNames', 'conformancePackEvaluationFilters_configRuleNames' - Filters the results by AWS Config rule names.
newConformancePackEvaluationFilters ::
  ConformancePackEvaluationFilters
newConformancePackEvaluationFilters =
  ConformancePackEvaluationFilters'
    { complianceType =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceIds = Prelude.Nothing,
      configRuleNames = Prelude.Nothing
    }

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
conformancePackEvaluationFilters_complianceType :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe ConformancePackComplianceType)
conformancePackEvaluationFilters_complianceType = Lens.lens (\ConformancePackEvaluationFilters' {complianceType} -> complianceType) (\s@ConformancePackEvaluationFilters' {} a -> s {complianceType = a} :: ConformancePackEvaluationFilters)

-- | Filters the results by the resource type (for example,
-- @\"AWS::EC2::Instance\"@).
conformancePackEvaluationFilters_resourceType :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe Prelude.Text)
conformancePackEvaluationFilters_resourceType = Lens.lens (\ConformancePackEvaluationFilters' {resourceType} -> resourceType) (\s@ConformancePackEvaluationFilters' {} a -> s {resourceType = a} :: ConformancePackEvaluationFilters)

-- | Filters the results by resource IDs.
--
-- This is valid only when you provide resource type. If there is no
-- resource type, you will see an error.
conformancePackEvaluationFilters_resourceIds :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe [Prelude.Text])
conformancePackEvaluationFilters_resourceIds = Lens.lens (\ConformancePackEvaluationFilters' {resourceIds} -> resourceIds) (\s@ConformancePackEvaluationFilters' {} a -> s {resourceIds = a} :: ConformancePackEvaluationFilters) Prelude.. Lens.mapping Prelude._Coerce

-- | Filters the results by AWS Config rule names.
conformancePackEvaluationFilters_configRuleNames :: Lens.Lens' ConformancePackEvaluationFilters (Prelude.Maybe [Prelude.Text])
conformancePackEvaluationFilters_configRuleNames = Lens.lens (\ConformancePackEvaluationFilters' {configRuleNames} -> configRuleNames) (\s@ConformancePackEvaluationFilters' {} a -> s {configRuleNames = a} :: ConformancePackEvaluationFilters) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.Hashable
    ConformancePackEvaluationFilters

instance
  Prelude.NFData
    ConformancePackEvaluationFilters

instance
  Prelude.ToJSON
    ConformancePackEvaluationFilters
  where
  toJSON ConformancePackEvaluationFilters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ComplianceType" Prelude..=)
              Prelude.<$> complianceType,
            ("ResourceType" Prelude..=) Prelude.<$> resourceType,
            ("ResourceIds" Prelude..=) Prelude.<$> resourceIds,
            ("ConfigRuleNames" Prelude..=)
              Prelude.<$> configRuleNames
          ]
      )
