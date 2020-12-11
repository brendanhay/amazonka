-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceSummaryByResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceSummaryByResourceType
  ( ComplianceSummaryByResourceType (..),

    -- * Smart constructor
    mkComplianceSummaryByResourceType,

    -- * Lenses
    csbrtResourceType,
    csbrtComplianceSummary,
  )
where

import Network.AWS.Config.Types.ComplianceSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of AWS resources of a specific type that are compliant or noncompliant, up to a maximum of 100 for each.
--
-- /See:/ 'mkComplianceSummaryByResourceType' smart constructor.
data ComplianceSummaryByResourceType = ComplianceSummaryByResourceType'
  { resourceType ::
      Lude.Maybe Lude.Text,
    complianceSummary ::
      Lude.Maybe
        ComplianceSummary
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceSummaryByResourceType' with the minimum fields required to make a request.
--
-- * 'complianceSummary' - The number of AWS resources that are compliant or noncompliant, up to a maximum of 100 for each.
-- * 'resourceType' - The type of AWS resource.
mkComplianceSummaryByResourceType ::
  ComplianceSummaryByResourceType
mkComplianceSummaryByResourceType =
  ComplianceSummaryByResourceType'
    { resourceType = Lude.Nothing,
      complianceSummary = Lude.Nothing
    }

-- | The type of AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csbrtResourceType :: Lens.Lens' ComplianceSummaryByResourceType (Lude.Maybe Lude.Text)
csbrtResourceType = Lens.lens (resourceType :: ComplianceSummaryByResourceType -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ComplianceSummaryByResourceType)
{-# DEPRECATED csbrtResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The number of AWS resources that are compliant or noncompliant, up to a maximum of 100 for each.
--
-- /Note:/ Consider using 'complianceSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csbrtComplianceSummary :: Lens.Lens' ComplianceSummaryByResourceType (Lude.Maybe ComplianceSummary)
csbrtComplianceSummary = Lens.lens (complianceSummary :: ComplianceSummaryByResourceType -> Lude.Maybe ComplianceSummary) (\s a -> s {complianceSummary = a} :: ComplianceSummaryByResourceType)
{-# DEPRECATED csbrtComplianceSummary "Use generic-lens or generic-optics with 'complianceSummary' instead." #-}

instance Lude.FromJSON ComplianceSummaryByResourceType where
  parseJSON =
    Lude.withObject
      "ComplianceSummaryByResourceType"
      ( \x ->
          ComplianceSummaryByResourceType'
            Lude.<$> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "ComplianceSummary")
      )
