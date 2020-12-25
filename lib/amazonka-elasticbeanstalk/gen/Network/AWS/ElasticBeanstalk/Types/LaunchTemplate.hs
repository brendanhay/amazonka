{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
  ( LaunchTemplate (..),

    -- * Smart constructor
    mkLaunchTemplate,

    -- * Lenses
    ltId,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ResourceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon EC2 launch template.
--
-- /See:/ 'mkLaunchTemplate' smart constructor.
newtype LaunchTemplate = LaunchTemplate'
  { -- | The ID of the launch template.
    id :: Core.Maybe Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplate' value with any optional fields omitted.
mkLaunchTemplate ::
  LaunchTemplate
mkLaunchTemplate = LaunchTemplate' {id = Core.Nothing}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltId :: Lens.Lens' LaunchTemplate (Core.Maybe Types.ResourceId)
ltId = Lens.field @"id"
{-# DEPRECATED ltId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromXML LaunchTemplate where
  parseXML x = LaunchTemplate' Core.<$> (x Core..@? "Id")
