{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCauseEntity
  ( ResponseTimeRootCauseEntity (..),

    -- * Smart constructor
    mkResponseTimeRootCauseEntity,

    -- * Lenses
    rtrceCoverage,
    rtrceName,
    rtrceRemote,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.Name as Types

-- | A collection of segments and corresponding subsegments associated to a response time warning.
--
-- /See:/ 'mkResponseTimeRootCauseEntity' smart constructor.
data ResponseTimeRootCauseEntity = ResponseTimeRootCauseEntity'
  { -- | The type and messages of the exceptions.
    coverage :: Core.Maybe Core.Double,
    -- | The name of the entity.
    name :: Core.Maybe Types.Name,
    -- | A flag that denotes a remote subsegment.
    remote :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResponseTimeRootCauseEntity' value with any optional fields omitted.
mkResponseTimeRootCauseEntity ::
  ResponseTimeRootCauseEntity
mkResponseTimeRootCauseEntity =
  ResponseTimeRootCauseEntity'
    { coverage = Core.Nothing,
      name = Core.Nothing,
      remote = Core.Nothing
    }

-- | The type and messages of the exceptions.
--
-- /Note:/ Consider using 'coverage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrceCoverage :: Lens.Lens' ResponseTimeRootCauseEntity (Core.Maybe Core.Double)
rtrceCoverage = Lens.field @"coverage"
{-# DEPRECATED rtrceCoverage "Use generic-lens or generic-optics with 'coverage' instead." #-}

-- | The name of the entity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrceName :: Lens.Lens' ResponseTimeRootCauseEntity (Core.Maybe Types.Name)
rtrceName = Lens.field @"name"
{-# DEPRECATED rtrceName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A flag that denotes a remote subsegment.
--
-- /Note:/ Consider using 'remote' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrceRemote :: Lens.Lens' ResponseTimeRootCauseEntity (Core.Maybe Core.Bool)
rtrceRemote = Lens.field @"remote"
{-# DEPRECATED rtrceRemote "Use generic-lens or generic-optics with 'remote' instead." #-}

instance Core.FromJSON ResponseTimeRootCauseEntity where
  parseJSON =
    Core.withObject "ResponseTimeRootCauseEntity" Core.$
      \x ->
        ResponseTimeRootCauseEntity'
          Core.<$> (x Core..:? "Coverage")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Remote")
