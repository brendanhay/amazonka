{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
  ( ComplianceDetails (..),

    -- * Smart constructor
    mkComplianceDetails,

    -- * Lenses
    cdComplianceStatus,
    cdKeysWithNoncompliantValues,
    cdNoncompliantKeys,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroupsTagging.Types.TagKey as Types

-- | Information that shows whether a resource is compliant with the effective tag policy, including details on any noncompliant tag keys.
--
-- /See:/ 'mkComplianceDetails' smart constructor.
data ComplianceDetails = ComplianceDetails'
  { -- | Whether a resource is compliant with the effective tag policy.
    complianceStatus :: Core.Maybe Core.Bool,
    -- | These are keys defined in the effective policy that are on the resource with either incorrect case treatment or noncompliant values.
    keysWithNoncompliantValues :: Core.Maybe [Types.TagKey],
    -- | These tag keys on the resource are noncompliant with the effective tag policy.
    noncompliantKeys :: Core.Maybe [Types.TagKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComplianceDetails' value with any optional fields omitted.
mkComplianceDetails ::
  ComplianceDetails
mkComplianceDetails =
  ComplianceDetails'
    { complianceStatus = Core.Nothing,
      keysWithNoncompliantValues = Core.Nothing,
      noncompliantKeys = Core.Nothing
    }

-- | Whether a resource is compliant with the effective tag policy.
--
-- /Note:/ Consider using 'complianceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdComplianceStatus :: Lens.Lens' ComplianceDetails (Core.Maybe Core.Bool)
cdComplianceStatus = Lens.field @"complianceStatus"
{-# DEPRECATED cdComplianceStatus "Use generic-lens or generic-optics with 'complianceStatus' instead." #-}

-- | These are keys defined in the effective policy that are on the resource with either incorrect case treatment or noncompliant values.
--
-- /Note:/ Consider using 'keysWithNoncompliantValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdKeysWithNoncompliantValues :: Lens.Lens' ComplianceDetails (Core.Maybe [Types.TagKey])
cdKeysWithNoncompliantValues = Lens.field @"keysWithNoncompliantValues"
{-# DEPRECATED cdKeysWithNoncompliantValues "Use generic-lens or generic-optics with 'keysWithNoncompliantValues' instead." #-}

-- | These tag keys on the resource are noncompliant with the effective tag policy.
--
-- /Note:/ Consider using 'noncompliantKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdNoncompliantKeys :: Lens.Lens' ComplianceDetails (Core.Maybe [Types.TagKey])
cdNoncompliantKeys = Lens.field @"noncompliantKeys"
{-# DEPRECATED cdNoncompliantKeys "Use generic-lens or generic-optics with 'noncompliantKeys' instead." #-}

instance Core.FromJSON ComplianceDetails where
  parseJSON =
    Core.withObject "ComplianceDetails" Core.$
      \x ->
        ComplianceDetails'
          Core.<$> (x Core..:? "ComplianceStatus")
          Core.<*> (x Core..:? "KeysWithNoncompliantValues")
          Core.<*> (x Core..:? "NoncompliantKeys")
