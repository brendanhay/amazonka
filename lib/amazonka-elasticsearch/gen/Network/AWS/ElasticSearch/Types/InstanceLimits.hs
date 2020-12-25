{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InstanceLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InstanceLimits
  ( InstanceLimits (..),

    -- * Smart constructor
    mkInstanceLimits,

    -- * Lenses
    ilInstanceCountLimits,
  )
where

import qualified Network.AWS.ElasticSearch.Types.InstanceCountLimits as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | InstanceLimits represents the list of instance related attributes that are available for given InstanceType.
--
-- /See:/ 'mkInstanceLimits' smart constructor.
newtype InstanceLimits = InstanceLimits'
  { instanceCountLimits :: Core.Maybe Types.InstanceCountLimits
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceLimits' value with any optional fields omitted.
mkInstanceLimits ::
  InstanceLimits
mkInstanceLimits =
  InstanceLimits' {instanceCountLimits = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'instanceCountLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilInstanceCountLimits :: Lens.Lens' InstanceLimits (Core.Maybe Types.InstanceCountLimits)
ilInstanceCountLimits = Lens.field @"instanceCountLimits"
{-# DEPRECATED ilInstanceCountLimits "Use generic-lens or generic-optics with 'instanceCountLimits' instead." #-}

instance Core.FromJSON InstanceLimits where
  parseJSON =
    Core.withObject "InstanceLimits" Core.$
      \x -> InstanceLimits' Core.<$> (x Core..:? "InstanceCountLimits")
