{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Evidence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Evidence
  ( Evidence (..),

    -- * Smart constructor
    mkEvidence,

    -- * Lenses
    eThreatIntelligenceDetails,
  )
where

import qualified Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the reason that the finding was generated.
--
-- /See:/ 'mkEvidence' smart constructor.
newtype Evidence = Evidence'
  { -- | A list of threat intelligence details related to the evidence.
    threatIntelligenceDetails :: Core.Maybe [Types.ThreatIntelligenceDetail]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Evidence' value with any optional fields omitted.
mkEvidence ::
  Evidence
mkEvidence = Evidence' {threatIntelligenceDetails = Core.Nothing}

-- | A list of threat intelligence details related to the evidence.
--
-- /Note:/ Consider using 'threatIntelligenceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eThreatIntelligenceDetails :: Lens.Lens' Evidence (Core.Maybe [Types.ThreatIntelligenceDetail])
eThreatIntelligenceDetails = Lens.field @"threatIntelligenceDetails"
{-# DEPRECATED eThreatIntelligenceDetails "Use generic-lens or generic-optics with 'threatIntelligenceDetails' instead." #-}

instance Core.FromJSON Evidence where
  parseJSON =
    Core.withObject "Evidence" Core.$
      \x -> Evidence' Core.<$> (x Core..:? "threatIntelligenceDetails")
