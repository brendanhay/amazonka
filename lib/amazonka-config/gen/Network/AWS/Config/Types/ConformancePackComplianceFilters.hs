-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackComplianceFilters
  ( ConformancePackComplianceFilters (..),

    -- * Smart constructor
    mkConformancePackComplianceFilters,

    -- * Lenses
    cpcfConfigRuleNames,
    cpcfComplianceType,
  )
where

import Network.AWS.Config.Types.ConformancePackComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters the conformance pack by compliance types and AWS Config rule names.
--
-- /See:/ 'mkConformancePackComplianceFilters' smart constructor.
data ConformancePackComplianceFilters = ConformancePackComplianceFilters'
  { configRuleNames ::
      Lude.Maybe [Lude.Text],
    complianceType ::
      Lude.Maybe
        ConformancePackComplianceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConformancePackComplianceFilters' with the minimum fields required to make a request.
--
-- * 'complianceType' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
-- * 'configRuleNames' - Filters the results by AWS Config rule names.
mkConformancePackComplianceFilters ::
  ConformancePackComplianceFilters
mkConformancePackComplianceFilters =
  ConformancePackComplianceFilters'
    { configRuleNames = Lude.Nothing,
      complianceType = Lude.Nothing
    }

-- | Filters the results by AWS Config rule names.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcfConfigRuleNames :: Lens.Lens' ConformancePackComplianceFilters (Lude.Maybe [Lude.Text])
cpcfConfigRuleNames = Lens.lens (configRuleNames :: ConformancePackComplianceFilters -> Lude.Maybe [Lude.Text]) (\s a -> s {configRuleNames = a} :: ConformancePackComplianceFilters)
{-# DEPRECATED cpcfConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcfComplianceType :: Lens.Lens' ConformancePackComplianceFilters (Lude.Maybe ConformancePackComplianceType)
cpcfComplianceType = Lens.lens (complianceType :: ConformancePackComplianceFilters -> Lude.Maybe ConformancePackComplianceType) (\s a -> s {complianceType = a} :: ConformancePackComplianceFilters)
{-# DEPRECATED cpcfComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

instance Lude.ToJSON ConformancePackComplianceFilters where
  toJSON ConformancePackComplianceFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConfigRuleNames" Lude..=) Lude.<$> configRuleNames,
            ("ComplianceType" Lude..=) Lude.<$> complianceType
          ]
      )
