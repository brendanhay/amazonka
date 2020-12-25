{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IdFormat
  ( IdFormat (..),

    -- * Smart constructor
    mkIdFormat,

    -- * Lenses
    ifDeadline,
    ifResource,
    ifUseLongIds,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the ID format for a resource.
--
-- /See:/ 'mkIdFormat' smart constructor.
data IdFormat = IdFormat'
  { -- | The date in UTC at which you are permanently switched over to using longer IDs. If a deadline is not yet available for this resource type, this field is not returned.
    deadline :: Core.Maybe Core.UTCTime,
    -- | The type of resource.
    resource :: Core.Maybe Types.String,
    -- | Indicates whether longer IDs (17-character IDs) are enabled for the resource.
    useLongIds :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'IdFormat' value with any optional fields omitted.
mkIdFormat ::
  IdFormat
mkIdFormat =
  IdFormat'
    { deadline = Core.Nothing,
      resource = Core.Nothing,
      useLongIds = Core.Nothing
    }

-- | The date in UTC at which you are permanently switched over to using longer IDs. If a deadline is not yet available for this resource type, this field is not returned.
--
-- /Note:/ Consider using 'deadline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDeadline :: Lens.Lens' IdFormat (Core.Maybe Core.UTCTime)
ifDeadline = Lens.field @"deadline"
{-# DEPRECATED ifDeadline "Use generic-lens or generic-optics with 'deadline' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifResource :: Lens.Lens' IdFormat (Core.Maybe Types.String)
ifResource = Lens.field @"resource"
{-# DEPRECATED ifResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | Indicates whether longer IDs (17-character IDs) are enabled for the resource.
--
-- /Note:/ Consider using 'useLongIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifUseLongIds :: Lens.Lens' IdFormat (Core.Maybe Core.Bool)
ifUseLongIds = Lens.field @"useLongIds"
{-# DEPRECATED ifUseLongIds "Use generic-lens or generic-optics with 'useLongIds' instead." #-}

instance Core.FromXML IdFormat where
  parseXML x =
    IdFormat'
      Core.<$> (x Core..@? "deadline")
      Core.<*> (x Core..@? "resource")
      Core.<*> (x Core..@? "useLongIds")
