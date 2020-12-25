{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ScalingParametersStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ScalingParametersStatus
  ( ScalingParametersStatus (..),

    -- * Smart constructor
    mkScalingParametersStatus,

    -- * Lenses
    spsOptions,
    spsStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types.OptionStatus as Types
import qualified Network.AWS.CloudSearch.Types.ScalingParameters as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status and configuration of a search domain's scaling parameters.
--
-- /See:/ 'mkScalingParametersStatus' smart constructor.
data ScalingParametersStatus = ScalingParametersStatus'
  { options :: Types.ScalingParameters,
    status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ScalingParametersStatus' value with any optional fields omitted.
mkScalingParametersStatus ::
  -- | 'options'
  Types.ScalingParameters ->
  -- | 'status'
  Types.OptionStatus ->
  ScalingParametersStatus
mkScalingParametersStatus options status =
  ScalingParametersStatus' {options, status}

-- | Undocumented field.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsOptions :: Lens.Lens' ScalingParametersStatus Types.ScalingParameters
spsOptions = Lens.field @"options"
{-# DEPRECATED spsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsStatus :: Lens.Lens' ScalingParametersStatus Types.OptionStatus
spsStatus = Lens.field @"status"
{-# DEPRECATED spsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML ScalingParametersStatus where
  parseXML x =
    ScalingParametersStatus'
      Core.<$> (x Core..@ "Options") Core.<*> (x Core..@ "Status")
