{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
  ( Phase1IntegrityAlgorithmsListValue (..),

    -- * Smart constructor
    mkPhase1IntegrityAlgorithmsListValue,

    -- * Lenses
    pialvValue,
  )
where

import qualified Network.AWS.EC2.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The integrity algorithm for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1IntegrityAlgorithmsListValue' smart constructor.
newtype Phase1IntegrityAlgorithmsListValue = Phase1IntegrityAlgorithmsListValue'
  { -- | The value for the integrity algorithm.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase1IntegrityAlgorithmsListValue' value with any optional fields omitted.
mkPhase1IntegrityAlgorithmsListValue ::
  Phase1IntegrityAlgorithmsListValue
mkPhase1IntegrityAlgorithmsListValue =
  Phase1IntegrityAlgorithmsListValue' {value = Core.Nothing}

-- | The value for the integrity algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pialvValue :: Lens.Lens' Phase1IntegrityAlgorithmsListValue (Core.Maybe Types.Value)
pialvValue = Lens.field @"value"
{-# DEPRECATED pialvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML Phase1IntegrityAlgorithmsListValue where
  parseXML x =
    Phase1IntegrityAlgorithmsListValue' Core.<$> (x Core..@? "value")
