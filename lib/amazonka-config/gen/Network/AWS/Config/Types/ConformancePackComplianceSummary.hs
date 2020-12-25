{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackComplianceSummary
  ( ConformancePackComplianceSummary (..),

    -- * Smart constructor
    mkConformancePackComplianceSummary,

    -- * Lenses
    cpcsConformancePackName,
    cpcsConformancePackComplianceStatus,
  )
where

import qualified Network.AWS.Config.Types.ConformancePackComplianceType as Types
import qualified Network.AWS.Config.Types.ConformancePackName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary includes the name and status of the conformance pack.
--
-- /See:/ 'mkConformancePackComplianceSummary' smart constructor.
data ConformancePackComplianceSummary = ConformancePackComplianceSummary'
  { -- | The name of the conformance pack name.
    conformancePackName :: Types.ConformancePackName,
    -- | The status of the conformance pack. The allowed values are COMPLIANT and NON_COMPLIANT.
    conformancePackComplianceStatus :: Types.ConformancePackComplianceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConformancePackComplianceSummary' value with any optional fields omitted.
mkConformancePackComplianceSummary ::
  -- | 'conformancePackName'
  Types.ConformancePackName ->
  -- | 'conformancePackComplianceStatus'
  Types.ConformancePackComplianceType ->
  ConformancePackComplianceSummary
mkConformancePackComplianceSummary
  conformancePackName
  conformancePackComplianceStatus =
    ConformancePackComplianceSummary'
      { conformancePackName,
        conformancePackComplianceStatus
      }

-- | The name of the conformance pack name.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcsConformancePackName :: Lens.Lens' ConformancePackComplianceSummary Types.ConformancePackName
cpcsConformancePackName = Lens.field @"conformancePackName"
{-# DEPRECATED cpcsConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | The status of the conformance pack. The allowed values are COMPLIANT and NON_COMPLIANT.
--
-- /Note:/ Consider using 'conformancePackComplianceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcsConformancePackComplianceStatus :: Lens.Lens' ConformancePackComplianceSummary Types.ConformancePackComplianceType
cpcsConformancePackComplianceStatus = Lens.field @"conformancePackComplianceStatus"
{-# DEPRECATED cpcsConformancePackComplianceStatus "Use generic-lens or generic-optics with 'conformancePackComplianceStatus' instead." #-}

instance Core.FromJSON ConformancePackComplianceSummary where
  parseJSON =
    Core.withObject "ConformancePackComplianceSummary" Core.$
      \x ->
        ConformancePackComplianceSummary'
          Core.<$> (x Core..: "ConformancePackName")
          Core.<*> (x Core..: "ConformancePackComplianceStatus")
