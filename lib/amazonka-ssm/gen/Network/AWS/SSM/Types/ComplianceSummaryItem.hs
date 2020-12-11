-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceSummaryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceSummaryItem
  ( ComplianceSummaryItem (..),

    -- * Smart constructor
    mkComplianceSummaryItem,

    -- * Lenses
    csiNonCompliantSummary,
    csiCompliantSummary,
    csiComplianceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.NonCompliantSummary

-- | A summary of compliance information by compliance type.
--
-- /See:/ 'mkComplianceSummaryItem' smart constructor.
data ComplianceSummaryItem = ComplianceSummaryItem'
  { nonCompliantSummary ::
      Lude.Maybe NonCompliantSummary,
    compliantSummary :: Lude.Maybe CompliantSummary,
    complianceType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceSummaryItem' with the minimum fields required to make a request.
--
-- * 'complianceType' - The type of compliance item. For example, the compliance type can be Association, Patch, or Custom:string.
-- * 'compliantSummary' - A list of COMPLIANT items for the specified compliance type.
-- * 'nonCompliantSummary' - A list of NON_COMPLIANT items for the specified compliance type.
mkComplianceSummaryItem ::
  ComplianceSummaryItem
mkComplianceSummaryItem =
  ComplianceSummaryItem'
    { nonCompliantSummary = Lude.Nothing,
      compliantSummary = Lude.Nothing,
      complianceType = Lude.Nothing
    }

-- | A list of NON_COMPLIANT items for the specified compliance type.
--
-- /Note:/ Consider using 'nonCompliantSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiNonCompliantSummary :: Lens.Lens' ComplianceSummaryItem (Lude.Maybe NonCompliantSummary)
csiNonCompliantSummary = Lens.lens (nonCompliantSummary :: ComplianceSummaryItem -> Lude.Maybe NonCompliantSummary) (\s a -> s {nonCompliantSummary = a} :: ComplianceSummaryItem)
{-# DEPRECATED csiNonCompliantSummary "Use generic-lens or generic-optics with 'nonCompliantSummary' instead." #-}

-- | A list of COMPLIANT items for the specified compliance type.
--
-- /Note:/ Consider using 'compliantSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiCompliantSummary :: Lens.Lens' ComplianceSummaryItem (Lude.Maybe CompliantSummary)
csiCompliantSummary = Lens.lens (compliantSummary :: ComplianceSummaryItem -> Lude.Maybe CompliantSummary) (\s a -> s {compliantSummary = a} :: ComplianceSummaryItem)
{-# DEPRECATED csiCompliantSummary "Use generic-lens or generic-optics with 'compliantSummary' instead." #-}

-- | The type of compliance item. For example, the compliance type can be Association, Patch, or Custom:string.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiComplianceType :: Lens.Lens' ComplianceSummaryItem (Lude.Maybe Lude.Text)
csiComplianceType = Lens.lens (complianceType :: ComplianceSummaryItem -> Lude.Maybe Lude.Text) (\s a -> s {complianceType = a} :: ComplianceSummaryItem)
{-# DEPRECATED csiComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

instance Lude.FromJSON ComplianceSummaryItem where
  parseJSON =
    Lude.withObject
      "ComplianceSummaryItem"
      ( \x ->
          ComplianceSummaryItem'
            Lude.<$> (x Lude..:? "NonCompliantSummary")
            Lude.<*> (x Lude..:? "CompliantSummary")
            Lude.<*> (x Lude..:? "ComplianceType")
      )
