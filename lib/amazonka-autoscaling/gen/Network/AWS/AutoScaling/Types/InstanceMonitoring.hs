{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceMonitoring
  ( InstanceMonitoring (..),

    -- * Smart constructor
    mkInstanceMonitoring,

    -- * Lenses
    imEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes whether detailed monitoring is enabled for the Auto Scaling instances.
--
-- /See:/ 'mkInstanceMonitoring' smart constructor.
newtype InstanceMonitoring = InstanceMonitoring'
  { -- | If @true@ , detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceMonitoring' value with any optional fields omitted.
mkInstanceMonitoring ::
  InstanceMonitoring
mkInstanceMonitoring = InstanceMonitoring' {enabled = Core.Nothing}

-- | If @true@ , detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEnabled :: Lens.Lens' InstanceMonitoring (Core.Maybe Core.Bool)
imEnabled = Lens.field @"enabled"
{-# DEPRECATED imEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromXML InstanceMonitoring where
  parseXML x = InstanceMonitoring' Core.<$> (x Core..@? "Enabled")
