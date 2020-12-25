{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RelatedResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RelatedResource
  ( RelatedResource (..),

    -- * Smart constructor
    mkRelatedResource,

    -- * Lenses
    rrAdditionalInfo,
    rrResourceIdentifier,
    rrResourceType,
  )
where

import qualified Network.AWS.IoT.Types.ResourceIdentifier as Types
import qualified Network.AWS.IoT.Types.ResourceType as Types
import qualified Network.AWS.IoT.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a related resource.
--
-- /See:/ 'mkRelatedResource' smart constructor.
data RelatedResource = RelatedResource'
  { -- | Other information about the resource.
    additionalInfo :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | Information that identifies the resource.
    resourceIdentifier :: Core.Maybe Types.ResourceIdentifier,
    -- | The type of resource.
    resourceType :: Core.Maybe Types.ResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RelatedResource' value with any optional fields omitted.
mkRelatedResource ::
  RelatedResource
mkRelatedResource =
  RelatedResource'
    { additionalInfo = Core.Nothing,
      resourceIdentifier = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | Other information about the resource.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrAdditionalInfo :: Lens.Lens' RelatedResource (Core.Maybe (Core.HashMap Types.String Types.String))
rrAdditionalInfo = Lens.field @"additionalInfo"
{-# DEPRECATED rrAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | Information that identifies the resource.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrResourceIdentifier :: Lens.Lens' RelatedResource (Core.Maybe Types.ResourceIdentifier)
rrResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED rrResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrResourceType :: Lens.Lens' RelatedResource (Core.Maybe Types.ResourceType)
rrResourceType = Lens.field @"resourceType"
{-# DEPRECATED rrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON RelatedResource where
  parseJSON =
    Core.withObject "RelatedResource" Core.$
      \x ->
        RelatedResource'
          Core.<$> (x Core..:? "additionalInfo")
          Core.<*> (x Core..:? "resourceIdentifier")
          Core.<*> (x Core..:? "resourceType")
