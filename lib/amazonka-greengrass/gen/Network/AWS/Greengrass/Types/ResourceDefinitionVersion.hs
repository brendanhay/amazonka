{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDefinitionVersion
  ( ResourceDefinitionVersion (..),

    -- * Smart constructor
    mkResourceDefinitionVersion,

    -- * Lenses
    rdvResources,
  )
where

import qualified Network.AWS.Greengrass.Types.Resource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a resource definition version.
--
-- /See:/ 'mkResourceDefinitionVersion' smart constructor.
newtype ResourceDefinitionVersion = ResourceDefinitionVersion'
  { -- | A list of resources.
    resources :: Core.Maybe [Types.Resource]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDefinitionVersion' value with any optional fields omitted.
mkResourceDefinitionVersion ::
  ResourceDefinitionVersion
mkResourceDefinitionVersion =
  ResourceDefinitionVersion' {resources = Core.Nothing}

-- | A list of resources.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdvResources :: Lens.Lens' ResourceDefinitionVersion (Core.Maybe [Types.Resource])
rdvResources = Lens.field @"resources"
{-# DEPRECATED rdvResources "Use generic-lens or generic-optics with 'resources' instead." #-}

instance Core.FromJSON ResourceDefinitionVersion where
  toJSON ResourceDefinitionVersion {..} =
    Core.object
      (Core.catMaybes [("Resources" Core..=) Core.<$> resources])

instance Core.FromJSON ResourceDefinitionVersion where
  parseJSON =
    Core.withObject "ResourceDefinitionVersion" Core.$
      \x -> ResourceDefinitionVersion' Core.<$> (x Core..:? "Resources")
