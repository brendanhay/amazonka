{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Resource
  ( Resource (..),

    -- * Smart constructor
    mkResource,

    -- * Lenses
    rResourceDataContainer,
    rId,
    rName,
  )
where

import qualified Network.AWS.Greengrass.Types.ResourceDataContainer as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a resource.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { -- | A container of data for all resource types.
    resourceDataContainer :: Types.ResourceDataContainer,
    -- | The resource ID, used to refer to a resource in the Lambda function configuration. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
    id :: Core.Text,
    -- | The descriptive resource name, which is displayed on the AWS IoT Greengrass console. Max length 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
    name :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Resource' value with any optional fields omitted.
mkResource ::
  -- | 'resourceDataContainer'
  Types.ResourceDataContainer ->
  -- | 'id'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  Resource
mkResource resourceDataContainer id name =
  Resource' {resourceDataContainer, id, name}

-- | A container of data for all resource types.
--
-- /Note:/ Consider using 'resourceDataContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceDataContainer :: Lens.Lens' Resource Types.ResourceDataContainer
rResourceDataContainer = Lens.field @"resourceDataContainer"
{-# DEPRECATED rResourceDataContainer "Use generic-lens or generic-optics with 'resourceDataContainer' instead." #-}

-- | The resource ID, used to refer to a resource in the Lambda function configuration. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' Resource Core.Text
rId = Lens.field @"id"
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The descriptive resource name, which is displayed on the AWS IoT Greengrass console. Max length 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Resource Core.Text
rName = Lens.field @"name"
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON Resource where
  toJSON Resource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceDataContainer" Core..= resourceDataContainer),
            Core.Just ("Id" Core..= id),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject "Resource" Core.$
      \x ->
        Resource'
          Core.<$> (x Core..: "ResourceDataContainer")
          Core.<*> (x Core..: "Id")
          Core.<*> (x Core..: "Name")
