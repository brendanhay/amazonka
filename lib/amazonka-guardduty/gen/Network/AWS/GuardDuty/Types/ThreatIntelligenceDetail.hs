{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
  ( ThreatIntelligenceDetail (..),

    -- * Smart constructor
    mkThreatIntelligenceDetail,

    -- * Lenses
    tidThreatListName,
    tidThreatNames,
  )
where

import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.GuardDuty.Types.ThreatListName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An instance of a threat intelligence detail that constitutes evidence for the finding.
--
-- /See:/ 'mkThreatIntelligenceDetail' smart constructor.
data ThreatIntelligenceDetail = ThreatIntelligenceDetail'
  { -- | The name of the threat intelligence list that triggered the finding.
    threatListName :: Core.Maybe Types.ThreatListName,
    -- | A list of names of the threats in the threat intelligence list that triggered the finding.
    threatNames :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThreatIntelligenceDetail' value with any optional fields omitted.
mkThreatIntelligenceDetail ::
  ThreatIntelligenceDetail
mkThreatIntelligenceDetail =
  ThreatIntelligenceDetail'
    { threatListName = Core.Nothing,
      threatNames = Core.Nothing
    }

-- | The name of the threat intelligence list that triggered the finding.
--
-- /Note:/ Consider using 'threatListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidThreatListName :: Lens.Lens' ThreatIntelligenceDetail (Core.Maybe Types.ThreatListName)
tidThreatListName = Lens.field @"threatListName"
{-# DEPRECATED tidThreatListName "Use generic-lens or generic-optics with 'threatListName' instead." #-}

-- | A list of names of the threats in the threat intelligence list that triggered the finding.
--
-- /Note:/ Consider using 'threatNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidThreatNames :: Lens.Lens' ThreatIntelligenceDetail (Core.Maybe [Types.String])
tidThreatNames = Lens.field @"threatNames"
{-# DEPRECATED tidThreatNames "Use generic-lens or generic-optics with 'threatNames' instead." #-}

instance Core.FromJSON ThreatIntelligenceDetail where
  parseJSON =
    Core.withObject "ThreatIntelligenceDetail" Core.$
      \x ->
        ThreatIntelligenceDetail'
          Core.<$> (x Core..:? "threatListName") Core.<*> (x Core..:? "threatNames")
