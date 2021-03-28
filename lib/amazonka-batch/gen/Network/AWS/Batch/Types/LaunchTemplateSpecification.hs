{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.LaunchTemplateSpecification
  ( LaunchTemplateSpecification (..)
  -- * Smart constructor
  , mkLaunchTemplateSpecification
  -- * Lenses
  , ltsLaunchTemplateId
  , ltsLaunchTemplateName
  , ltsVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a launch template associated with a compute resource. You must specify either the launch template ID or launch template name in the request, but not both.
--
-- /See:/ 'mkLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { launchTemplateId :: Core.Maybe Core.Text
    -- ^ The ID of the launch template.
  , launchTemplateName :: Core.Maybe Core.Text
    -- ^ The name of the launch template.
  , version :: Core.Maybe Core.Text
    -- ^ The version number of the launch template, @> Latest@ , or @> Default@ .
--
-- If the value is @> Latest@ , the latest version of the launch template is used. If the value is @> Default@ , the default version of the launch template is used.
-- Default: @> Default@ .
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
ltsLaunchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Core.Text)
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

-- | The version number of the launch template, @> Latest@ , or @> Default@ .
--
-- If the value is @> Latest@ , the latest version of the launch template is used. If the value is @> Default@ , the default version of the launch template is used.
-- Default: @> Default@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsVersion :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Core.Text)
ltsVersion = Lens.field @"version"
{-# INLINEABLE ltsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON LaunchTemplateSpecification where
        toJSON LaunchTemplateSpecification{..}
          = Core.object
              (Core.catMaybes
                 [("launchTemplateId" Core..=) Core.<$> launchTemplateId,
                  ("launchTemplateName" Core..=) Core.<$> launchTemplateName,
                  ("version" Core..=) Core.<$> version])

instance Core.FromJSON LaunchTemplateSpecification where
        parseJSON
          = Core.withObject "LaunchTemplateSpecification" Core.$
              \ x ->
                LaunchTemplateSpecification' Core.<$>
                  (x Core..:? "launchTemplateId") Core.<*>
                    x Core..:? "launchTemplateName"
                    Core.<*> x Core..:? "version"
