{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue
  ( Phase2DHGroupNumbersListValue (..),

    -- * Smart constructor
    mkPhase2DHGroupNumbersListValue,

    -- * Lenses
    pValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Diffie-Hellmann group number for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2DHGroupNumbersListValue' smart constructor.
newtype Phase2DHGroupNumbersListValue = Phase2DHGroupNumbersListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase2DHGroupNumbersListValue' value with any optional fields omitted.
mkPhase2DHGroupNumbersListValue ::
  Phase2DHGroupNumbersListValue
mkPhase2DHGroupNumbersListValue =
  Phase2DHGroupNumbersListValue' {value = Core.Nothing}

-- | The Diffie-Hellmann group number.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pValue :: Lens.Lens' Phase2DHGroupNumbersListValue (Core.Maybe Core.Int)
pValue = Lens.field @"value"
{-# DEPRECATED pValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML Phase2DHGroupNumbersListValue where
  parseXML x =
    Phase2DHGroupNumbersListValue' Core.<$> (x Core..@? "value")
