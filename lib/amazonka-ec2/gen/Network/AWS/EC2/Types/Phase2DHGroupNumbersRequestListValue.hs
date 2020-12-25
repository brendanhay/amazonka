{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
  ( Phase2DHGroupNumbersRequestListValue (..),

    -- * Smart constructor
    mkPhase2DHGroupNumbersRequestListValue,

    -- * Lenses
    pdhgnrlvfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a Diffie-Hellman group number for the VPN tunnel for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2DHGroupNumbersRequestListValue' smart constructor.
newtype Phase2DHGroupNumbersRequestListValue = Phase2DHGroupNumbersRequestListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase2DHGroupNumbersRequestListValue' value with any optional fields omitted.
mkPhase2DHGroupNumbersRequestListValue ::
  Phase2DHGroupNumbersRequestListValue
mkPhase2DHGroupNumbersRequestListValue =
  Phase2DHGroupNumbersRequestListValue' {value = Core.Nothing}

-- | The Diffie-Hellmann group number.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdhgnrlvfValue :: Lens.Lens' Phase2DHGroupNumbersRequestListValue (Core.Maybe Core.Int)
pdhgnrlvfValue = Lens.field @"value"
{-# DEPRECATED pdhgnrlvfValue "Use generic-lens or generic-optics with 'value' instead." #-}
