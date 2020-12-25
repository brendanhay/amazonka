{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
  ( AutoScalingGroup (..),

    -- * Smart constructor
    mkAutoScalingGroup,

    -- * Lenses
    asgName,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ResourceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'mkAutoScalingGroup' smart constructor.
newtype AutoScalingGroup = AutoScalingGroup'
  { -- | The name of the @AutoScalingGroup@ .
    name :: Core.Maybe Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingGroup' value with any optional fields omitted.
mkAutoScalingGroup ::
  AutoScalingGroup
mkAutoScalingGroup = AutoScalingGroup' {name = Core.Nothing}

-- | The name of the @AutoScalingGroup@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgName :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.ResourceId)
asgName = Lens.field @"name"
{-# DEPRECATED asgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromXML AutoScalingGroup where
  parseXML x = AutoScalingGroup' Core.<$> (x Core..@? "Name")
