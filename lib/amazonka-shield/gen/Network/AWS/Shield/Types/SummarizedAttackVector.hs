{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SummarizedAttackVector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.SummarizedAttackVector
  ( SummarizedAttackVector (..)
  -- * Smart constructor
  , mkSummarizedAttackVector
  -- * Lenses
  , savVectorType
  , savVectorCounters
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.SummarizedCounter as Types

-- | A summary of information about the attack.
--
-- /See:/ 'mkSummarizedAttackVector' smart constructor.
data SummarizedAttackVector = SummarizedAttackVector'
  { vectorType :: Core.Text
    -- ^ The attack type, for example, SNMP reflection or SYN flood.
  , vectorCounters :: Core.Maybe [Types.SummarizedCounter]
    -- ^ The list of counters that describe the details of the attack.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SummarizedAttackVector' value with any optional fields omitted.
mkSummarizedAttackVector
    :: Core.Text -- ^ 'vectorType'
    -> SummarizedAttackVector
mkSummarizedAttackVector vectorType
  = SummarizedAttackVector'{vectorType,
                            vectorCounters = Core.Nothing}

-- | The attack type, for example, SNMP reflection or SYN flood.
--
-- /Note:/ Consider using 'vectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
savVectorType :: Lens.Lens' SummarizedAttackVector Core.Text
savVectorType = Lens.field @"vectorType"
{-# INLINEABLE savVectorType #-}
{-# DEPRECATED vectorType "Use generic-lens or generic-optics with 'vectorType' instead"  #-}

-- | The list of counters that describe the details of the attack.
--
-- /Note:/ Consider using 'vectorCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
savVectorCounters :: Lens.Lens' SummarizedAttackVector (Core.Maybe [Types.SummarizedCounter])
savVectorCounters = Lens.field @"vectorCounters"
{-# INLINEABLE savVectorCounters #-}
{-# DEPRECATED vectorCounters "Use generic-lens or generic-optics with 'vectorCounters' instead"  #-}

instance Core.FromJSON SummarizedAttackVector where
        parseJSON
          = Core.withObject "SummarizedAttackVector" Core.$
              \ x ->
                SummarizedAttackVector' Core.<$>
                  (x Core..: "VectorType") Core.<*> x Core..:? "VectorCounters"
