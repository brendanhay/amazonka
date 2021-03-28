{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateSpecification
  ( LaunchTemplateSpecification (..)
  -- * Smart constructor
  , mkLaunchTemplateSpecification
  -- * Lenses
  , ltsLaunchTemplateId
  , ltsLaunchTemplateName
  , ltsVersion
  ) where

import qualified Network.AWS.EC2.Types.LaunchTemplateId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request, but not both.
--
-- /See:/ 'mkLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { launchTemplateId :: Core.Maybe Types.LaunchTemplateId
    -- ^ The ID of the launch template.
  , launchTemplateName :: Core.Maybe Core.Text
    -- ^ The name of the launch template.
  , version :: Core.Maybe Core.Text
    -- ^ The version number of the launch template.
--
-- Default: The default version for the launch template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateSpecification' value with any optional fields omitted.
mkLaunchTemplateSpecification
    :: LaunchTemplateSpecification
mkLaunchTemplateSpecification
  = LaunchTemplateSpecification'{launchTemplateId = Core.Nothing,
                                 launchTemplateName = Core.Nothing, version = Core.Nothing}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Types.LaunchTemplateId)
ltsLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE ltsLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Core.Text)
ltsLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE ltsLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The version number of the launch template.
--
-- Default: The default version for the launch template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsVersion :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Core.Text)
ltsVersion = Lens.field @"version"
{-# INLINEABLE ltsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery LaunchTemplateSpecification where
        toQuery LaunchTemplateSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateId")
              launchTemplateId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateName")
                launchTemplateName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Version") version
