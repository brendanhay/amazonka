{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackRuleCompliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackRuleCompliance
  ( ConformancePackRuleCompliance (..),

    -- * Smart constructor
    mkConformancePackRuleCompliance,

    -- * Lenses
    cprcConfigRuleName,
    cprcComplianceType,
  )
where

import Network.AWS.Config.Types.ConformancePackComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Compliance information of one or more AWS Config rules within a conformance pack. You can filter using AWS Config rule names and compliance types.
--
-- /See:/ 'mkConformancePackRuleCompliance' smart constructor.
data ConformancePackRuleCompliance = ConformancePackRuleCompliance'
  { -- | Name of the config rule.
    configRuleName :: Lude.Maybe Lude.Text,
    -- | Compliance of the AWS Config rule
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
    complianceType :: Lude.Maybe ConformancePackComplianceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConformancePackRuleCompliance' with the minimum fields required to make a request.
--
-- * 'configRuleName' - Name of the config rule.
-- * 'complianceType' - Compliance of the AWS Config rule
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
mkConformancePackRuleCompliance ::
  ConformancePackRuleCompliance
mkConformancePackRuleCompliance =
  ConformancePackRuleCompliance'
    { configRuleName = Lude.Nothing,
      complianceType = Lude.Nothing
    }

-- | Name of the config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprcConfigRuleName :: Lens.Lens' ConformancePackRuleCompliance (Lude.Maybe Lude.Text)
cprcConfigRuleName = Lens.lens (configRuleName :: ConformancePackRuleCompliance -> Lude.Maybe Lude.Text) (\s a -> s {configRuleName = a} :: ConformancePackRuleCompliance)
{-# DEPRECATED cprcConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | Compliance of the AWS Config rule
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprcComplianceType :: Lens.Lens' ConformancePackRuleCompliance (Lude.Maybe ConformancePackComplianceType)
cprcComplianceType = Lens.lens (complianceType :: ConformancePackRuleCompliance -> Lude.Maybe ConformancePackComplianceType) (\s a -> s {complianceType = a} :: ConformancePackRuleCompliance)
{-# DEPRECATED cprcComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

instance Lude.FromJSON ConformancePackRuleCompliance where
  parseJSON =
    Lude.withObject
      "ConformancePackRuleCompliance"
      ( \x ->
          ConformancePackRuleCompliance'
            Lude.<$> (x Lude..:? "ConfigRuleName")
            Lude.<*> (x Lude..:? "ComplianceType")
      )
