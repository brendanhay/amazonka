{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
  ( Phase1DHGroupNumbersListValue (..),

    -- * Smart constructor
    mkPhase1DHGroupNumbersListValue,

    -- * Lenses
    pdhgnlvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Diffie-Hellmann group number for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1DHGroupNumbersListValue' smart constructor.
newtype Phase1DHGroupNumbersListValue = Phase1DHGroupNumbersListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase1DHGroupNumbersListValue' value with any optional fields omitted.
mkPhase1DHGroupNumbersListValue ::
  Phase1DHGroupNumbersListValue
mkPhase1DHGroupNumbersListValue =
  Phase1DHGroupNumbersListValue' {value = Core.Nothing}

-- | The Diffie-Hellmann group number.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdhgnlvValue :: Lens.Lens' Phase1DHGroupNumbersListValue (Core.Maybe Core.Int)
pdhgnlvValue = Lens.field @"value"
{-# DEPRECATED pdhgnlvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML Phase1DHGroupNumbersListValue where
  parseXML x =
    Phase1DHGroupNumbersListValue' Core.<$> (x Core..@? "value")
