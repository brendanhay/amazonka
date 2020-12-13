{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateComplianceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateComplianceCount
  ( AggregateComplianceCount (..),

    -- * Smart constructor
    mkAggregateComplianceCount,

    -- * Lenses
    accGroupName,
    accComplianceSummary,
  )
where

import Network.AWS.Config.Types.ComplianceSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns the number of compliant and noncompliant rules for one or more accounts and regions in an aggregator.
--
-- /See:/ 'mkAggregateComplianceCount' smart constructor.
data AggregateComplianceCount = AggregateComplianceCount'
  { -- | The 12-digit account ID or region based on the GroupByKey value.
    groupName :: Lude.Maybe Lude.Text,
    -- | The number of compliant and noncompliant AWS Config rules.
    complianceSummary :: Lude.Maybe ComplianceSummary
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AggregateComplianceCount' with the minimum fields required to make a request.
--
-- * 'groupName' - The 12-digit account ID or region based on the GroupByKey value.
-- * 'complianceSummary' - The number of compliant and noncompliant AWS Config rules.
mkAggregateComplianceCount ::
  AggregateComplianceCount
mkAggregateComplianceCount =
  AggregateComplianceCount'
    { groupName = Lude.Nothing,
      complianceSummary = Lude.Nothing
    }

-- | The 12-digit account ID or region based on the GroupByKey value.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
accGroupName :: Lens.Lens' AggregateComplianceCount (Lude.Maybe Lude.Text)
accGroupName = Lens.lens (groupName :: AggregateComplianceCount -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: AggregateComplianceCount)
{-# DEPRECATED accGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The number of compliant and noncompliant AWS Config rules.
--
-- /Note:/ Consider using 'complianceSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
accComplianceSummary :: Lens.Lens' AggregateComplianceCount (Lude.Maybe ComplianceSummary)
accComplianceSummary = Lens.lens (complianceSummary :: AggregateComplianceCount -> Lude.Maybe ComplianceSummary) (\s a -> s {complianceSummary = a} :: AggregateComplianceCount)
{-# DEPRECATED accComplianceSummary "Use generic-lens or generic-optics with 'complianceSummary' instead." #-}

instance Lude.FromJSON AggregateComplianceCount where
  parseJSON =
    Lude.withObject
      "AggregateComplianceCount"
      ( \x ->
          AggregateComplianceCount'
            Lude.<$> (x Lude..:? "GroupName") Lude.<*> (x Lude..:? "ComplianceSummary")
      )
