{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackEvaluationFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackEvaluationFilters
  ( ConformancePackEvaluationFilters (..),

    -- * Smart constructor
    mkConformancePackEvaluationFilters,

    -- * Lenses
    cpefResourceIds,
    cpefResourceType,
    cpefConfigRuleNames,
    cpefComplianceType,
  )
where

import Network.AWS.Config.Types.ConformancePackComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters a conformance pack by AWS Config rule names, compliance types, AWS resource types, and resource IDs.
--
-- /See:/ 'mkConformancePackEvaluationFilters' smart constructor.
data ConformancePackEvaluationFilters = ConformancePackEvaluationFilters'
  { -- | Filters the results by resource IDs.
    resourceIds :: Lude.Maybe [Lude.Text],
    -- | Filters the results by the resource type (for example, @"AWS::EC2::Instance"@ ).
    resourceType :: Lude.Maybe Lude.Text,
    -- | Filters the results by AWS Config rule names.
    configRuleNames :: Lude.Maybe [Lude.Text],
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
    complianceType :: Lude.Maybe ConformancePackComplianceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConformancePackEvaluationFilters' with the minimum fields required to make a request.
--
-- * 'resourceIds' - Filters the results by resource IDs.
-- * 'resourceType' - Filters the results by the resource type (for example, @"AWS::EC2::Instance"@ ).
-- * 'configRuleNames' - Filters the results by AWS Config rule names.
-- * 'complianceType' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
mkConformancePackEvaluationFilters ::
  ConformancePackEvaluationFilters
mkConformancePackEvaluationFilters =
  ConformancePackEvaluationFilters'
    { resourceIds = Lude.Nothing,
      resourceType = Lude.Nothing,
      configRuleNames = Lude.Nothing,
      complianceType = Lude.Nothing
    }

-- | Filters the results by resource IDs.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpefResourceIds :: Lens.Lens' ConformancePackEvaluationFilters (Lude.Maybe [Lude.Text])
cpefResourceIds = Lens.lens (resourceIds :: ConformancePackEvaluationFilters -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceIds = a} :: ConformancePackEvaluationFilters)
{-# DEPRECATED cpefResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

-- | Filters the results by the resource type (for example, @"AWS::EC2::Instance"@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpefResourceType :: Lens.Lens' ConformancePackEvaluationFilters (Lude.Maybe Lude.Text)
cpefResourceType = Lens.lens (resourceType :: ConformancePackEvaluationFilters -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ConformancePackEvaluationFilters)
{-# DEPRECATED cpefResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Filters the results by AWS Config rule names.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpefConfigRuleNames :: Lens.Lens' ConformancePackEvaluationFilters (Lude.Maybe [Lude.Text])
cpefConfigRuleNames = Lens.lens (configRuleNames :: ConformancePackEvaluationFilters -> Lude.Maybe [Lude.Text]) (\s a -> s {configRuleNames = a} :: ConformancePackEvaluationFilters)
{-# DEPRECATED cpefConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpefComplianceType :: Lens.Lens' ConformancePackEvaluationFilters (Lude.Maybe ConformancePackComplianceType)
cpefComplianceType = Lens.lens (complianceType :: ConformancePackEvaluationFilters -> Lude.Maybe ConformancePackComplianceType) (\s a -> s {complianceType = a} :: ConformancePackEvaluationFilters)
{-# DEPRECATED cpefComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

instance Lude.ToJSON ConformancePackEvaluationFilters where
  toJSON ConformancePackEvaluationFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceIds" Lude..=) Lude.<$> resourceIds,
            ("ResourceType" Lude..=) Lude.<$> resourceType,
            ("ConfigRuleNames" Lude..=) Lude.<$> configRuleNames,
            ("ComplianceType" Lude..=) Lude.<$> complianceType
          ]
      )
