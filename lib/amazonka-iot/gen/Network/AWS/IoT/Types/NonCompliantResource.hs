{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.NonCompliantResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.NonCompliantResource
  ( NonCompliantResource (..),

    -- * Smart constructor
    mkNonCompliantResource,

    -- * Lenses
    ncrAdditionalInfo,
    ncrResourceIdentifier,
    ncrResourceType,
  )
where

import qualified Network.AWS.IoT.Types.ResourceIdentifier as Types
import qualified Network.AWS.IoT.Types.ResourceType as Types
import qualified Network.AWS.IoT.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the resource that was noncompliant with the audit check.
--
-- /See:/ 'mkNonCompliantResource' smart constructor.
data NonCompliantResource = NonCompliantResource'
  { -- | Other information about the noncompliant resource.
    additionalInfo :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | Information that identifies the noncompliant resource.
    resourceIdentifier :: Core.Maybe Types.ResourceIdentifier,
    -- | The type of the noncompliant resource.
    resourceType :: Core.Maybe Types.ResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NonCompliantResource' value with any optional fields omitted.
mkNonCompliantResource ::
  NonCompliantResource
mkNonCompliantResource =
  NonCompliantResource'
    { additionalInfo = Core.Nothing,
      resourceIdentifier = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | Other information about the noncompliant resource.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncrAdditionalInfo :: Lens.Lens' NonCompliantResource (Core.Maybe (Core.HashMap Types.String Types.String))
ncrAdditionalInfo = Lens.field @"additionalInfo"
{-# DEPRECATED ncrAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | Information that identifies the noncompliant resource.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncrResourceIdentifier :: Lens.Lens' NonCompliantResource (Core.Maybe Types.ResourceIdentifier)
ncrResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED ncrResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | The type of the noncompliant resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncrResourceType :: Lens.Lens' NonCompliantResource (Core.Maybe Types.ResourceType)
ncrResourceType = Lens.field @"resourceType"
{-# DEPRECATED ncrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON NonCompliantResource where
  parseJSON =
    Core.withObject "NonCompliantResource" Core.$
      \x ->
        NonCompliantResource'
          Core.<$> (x Core..:? "additionalInfo")
          Core.<*> (x Core..:? "resourceIdentifier")
          Core.<*> (x Core..:? "resourceType")
