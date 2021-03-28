{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.LaunchTemplateSpecification
  ( LaunchTemplateSpecification (..)
  -- * Smart constructor
  , mkLaunchTemplateSpecification
  -- * Lenses
  , ltsLaunchTemplateId
  , ltsLaunchTemplateName
  , ltsVersion
  ) where

import qualified Network.AWS.GameLift.Types.LaunchTemplateId as Types
import qualified Network.AWS.GameLift.Types.LaunchTemplateName as Types
import qualified Network.AWS.GameLift.Types.LaunchTemplateVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__ 
--
-- An EC2 launch template that contains configuration settings and game server code to be deployed to all instances in a game server group. The launch template is specified when creating a new game server group with 'CreateGameServerGroup' . 
--
-- /See:/ 'mkLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { launchTemplateId :: Core.Maybe Types.LaunchTemplateId
    -- ^ A unique identifier for an existing EC2 launch template.
  , launchTemplateName :: Core.Maybe Types.LaunchTemplateName
    -- ^ A readable identifier for an existing EC2 launch template. 
  , version :: Core.Maybe Types.LaunchTemplateVersion
    -- ^ The version of the EC2 launch template to use. If no version is specified, the default version will be used. With Amazon EC2, you can specify a default version for a launch template. If none is set, the default is the first version created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateSpecification' value with any optional fields omitted.
mkLaunchTemplateSpecification
    :: LaunchTemplateSpecification
mkLaunchTemplateSpecification
  = LaunchTemplateSpecification'{launchTemplateId = Core.Nothing,
                                 launchTemplateName = Core.Nothing, version = Core.Nothing}

-- | A unique identifier for an existing EC2 launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Types.LaunchTemplateId)
ltsLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE ltsLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | A readable identifier for an existing EC2 launch template. 
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Types.LaunchTemplateName)
ltsLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE ltsLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The version of the EC2 launch template to use. If no version is specified, the default version will be used. With Amazon EC2, you can specify a default version for a launch template. If none is set, the default is the first version created.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsVersion :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Types.LaunchTemplateVersion)
ltsVersion = Lens.field @"version"
{-# INLINEABLE ltsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON LaunchTemplateSpecification where
        toJSON LaunchTemplateSpecification{..}
          = Core.object
              (Core.catMaybes
                 [("LaunchTemplateId" Core..=) Core.<$> launchTemplateId,
                  ("LaunchTemplateName" Core..=) Core.<$> launchTemplateName,
                  ("Version" Core..=) Core.<$> version])
