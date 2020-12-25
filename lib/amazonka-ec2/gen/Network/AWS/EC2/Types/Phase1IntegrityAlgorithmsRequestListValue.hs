{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
  ( Phase1IntegrityAlgorithmsRequestListValue (..),

    -- * Smart constructor
    mkPhase1IntegrityAlgorithmsRequestListValue,

    -- * Lenses
    piarlvfValue,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the integrity algorithm for the VPN tunnel for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1IntegrityAlgorithmsRequestListValue' smart constructor.
newtype Phase1IntegrityAlgorithmsRequestListValue = Phase1IntegrityAlgorithmsRequestListValue'
  { -- | The value for the integrity algorithm.
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase1IntegrityAlgorithmsRequestListValue' value with any optional fields omitted.
mkPhase1IntegrityAlgorithmsRequestListValue ::
  Phase1IntegrityAlgorithmsRequestListValue
mkPhase1IntegrityAlgorithmsRequestListValue =
  Phase1IntegrityAlgorithmsRequestListValue' {value = Core.Nothing}

-- | The value for the integrity algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piarlvfValue :: Lens.Lens' Phase1IntegrityAlgorithmsRequestListValue (Core.Maybe Types.String)
piarlvfValue = Lens.field @"value"
{-# DEPRECATED piarlvfValue "Use generic-lens or generic-optics with 'value' instead." #-}
