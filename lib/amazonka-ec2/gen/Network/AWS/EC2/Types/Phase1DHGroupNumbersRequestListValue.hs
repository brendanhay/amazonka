{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
  ( Phase1DHGroupNumbersRequestListValue (..),

    -- * Smart constructor
    mkPhase1DHGroupNumbersRequestListValue,

    -- * Lenses
    pdhgnrlvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a Diffie-Hellman group number for the VPN tunnel for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1DHGroupNumbersRequestListValue' smart constructor.
newtype Phase1DHGroupNumbersRequestListValue = Phase1DHGroupNumbersRequestListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase1DHGroupNumbersRequestListValue' value with any optional fields omitted.
mkPhase1DHGroupNumbersRequestListValue ::
  Phase1DHGroupNumbersRequestListValue
mkPhase1DHGroupNumbersRequestListValue =
  Phase1DHGroupNumbersRequestListValue' {value = Core.Nothing}

-- | The Diffie-Hellmann group number.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdhgnrlvValue :: Lens.Lens' Phase1DHGroupNumbersRequestListValue (Core.Maybe Core.Int)
pdhgnrlvValue = Lens.field @"value"
{-# DEPRECATED pdhgnrlvValue "Use generic-lens or generic-optics with 'value' instead." #-}
