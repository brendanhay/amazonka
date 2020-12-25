{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExceptionResourceKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExceptionResourceKey
  ( RemediationExceptionResourceKey (..),

    -- * Smart constructor
    mkRemediationExceptionResourceKey,

    -- * Lenses
    rerkResourceId,
    rerkResourceType,
  )
where

import qualified Network.AWS.Config.Types.ResourceId as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details that identify a resource within AWS Config, including the resource type and resource ID.
--
-- /See:/ 'mkRemediationExceptionResourceKey' smart constructor.
data RemediationExceptionResourceKey = RemediationExceptionResourceKey'
  { -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The type of a resource.
    resourceType :: Core.Maybe Types.StringWithCharLimit256
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemediationExceptionResourceKey' value with any optional fields omitted.
mkRemediationExceptionResourceKey ::
  RemediationExceptionResourceKey
mkRemediationExceptionResourceKey =
  RemediationExceptionResourceKey'
    { resourceId = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerkResourceId :: Lens.Lens' RemediationExceptionResourceKey (Core.Maybe Types.ResourceId)
rerkResourceId = Lens.field @"resourceId"
{-# DEPRECATED rerkResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of a resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerkResourceType :: Lens.Lens' RemediationExceptionResourceKey (Core.Maybe Types.StringWithCharLimit256)
rerkResourceType = Lens.field @"resourceType"
{-# DEPRECATED rerkResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON RemediationExceptionResourceKey where
  toJSON RemediationExceptionResourceKey {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceId" Core..=) Core.<$> resourceId,
            ("ResourceType" Core..=) Core.<$> resourceType
          ]
      )

instance Core.FromJSON RemediationExceptionResourceKey where
  parseJSON =
    Core.withObject "RemediationExceptionResourceKey" Core.$
      \x ->
        RemediationExceptionResourceKey'
          Core.<$> (x Core..:? "ResourceId") Core.<*> (x Core..:? "ResourceType")
