{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
  ( ThreatIntelligenceDetail (..)
  -- * Smart constructor
  , mkThreatIntelligenceDetail
  -- * Lenses
  , tidThreatListName
  , tidThreatNames
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An instance of a threat intelligence detail that constitutes evidence for the finding.
--
-- /See:/ 'mkThreatIntelligenceDetail' smart constructor.
data ThreatIntelligenceDetail = ThreatIntelligenceDetail'
  { threatListName :: Core.Maybe Core.Text
    -- ^ The name of the threat intelligence list that triggered the finding.
  , threatNames :: Core.Maybe [Core.Text]
    -- ^ A list of names of the threats in the threat intelligence list that triggered the finding.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThreatIntelligenceDetail' value with any optional fields omitted.
mkThreatIntelligenceDetail
    :: ThreatIntelligenceDetail
mkThreatIntelligenceDetail
  = ThreatIntelligenceDetail'{threatListName = Core.Nothing,
                              threatNames = Core.Nothing}

-- | The name of the threat intelligence list that triggered the finding.
--
-- /Note:/ Consider using 'threatListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidThreatListName :: Lens.Lens' ThreatIntelligenceDetail (Core.Maybe Core.Text)
tidThreatListName = Lens.field @"threatListName"
{-# INLINEABLE tidThreatListName #-}
{-# DEPRECATED threatListName "Use generic-lens or generic-optics with 'threatListName' instead"  #-}

-- | A list of names of the threats in the threat intelligence list that triggered the finding.
--
-- /Note:/ Consider using 'threatNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidThreatNames :: Lens.Lens' ThreatIntelligenceDetail (Core.Maybe [Core.Text])
tidThreatNames = Lens.field @"threatNames"
{-# INLINEABLE tidThreatNames #-}
{-# DEPRECATED threatNames "Use generic-lens or generic-optics with 'threatNames' instead"  #-}

instance Core.FromJSON ThreatIntelligenceDetail where
        parseJSON
          = Core.withObject "ThreatIntelligenceDetail" Core.$
              \ x ->
                ThreatIntelligenceDetail' Core.<$>
                  (x Core..:? "threatListName") Core.<*> x Core..:? "threatNames"
