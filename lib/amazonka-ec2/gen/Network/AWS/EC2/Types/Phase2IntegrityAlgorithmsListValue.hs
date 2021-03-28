{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
  ( Phase2IntegrityAlgorithmsListValue (..)
  -- * Smart constructor
  , mkPhase2IntegrityAlgorithmsListValue
  -- * Lenses
  , pialvfValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The integrity algorithm for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2IntegrityAlgorithmsListValue' smart constructor.
newtype Phase2IntegrityAlgorithmsListValue = Phase2IntegrityAlgorithmsListValue'
  { value :: Core.Maybe Core.Text
    -- ^ The integrity algorithm.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase2IntegrityAlgorithmsListValue' value with any optional fields omitted.
mkPhase2IntegrityAlgorithmsListValue
    :: Phase2IntegrityAlgorithmsListValue
mkPhase2IntegrityAlgorithmsListValue
  = Phase2IntegrityAlgorithmsListValue'{value = Core.Nothing}

-- | The integrity algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pialvfValue :: Lens.Lens' Phase2IntegrityAlgorithmsListValue (Core.Maybe Core.Text)
pialvfValue = Lens.field @"value"
{-# INLINEABLE pialvfValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML Phase2IntegrityAlgorithmsListValue where
        parseXML x
          = Phase2IntegrityAlgorithmsListValue' Core.<$> (x Core..@? "value")
