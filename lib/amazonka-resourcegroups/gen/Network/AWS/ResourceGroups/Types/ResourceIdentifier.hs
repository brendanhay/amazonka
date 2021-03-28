{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.ResourceIdentifier
  ( ResourceIdentifier (..)
  -- * Smart constructor
  , mkResourceIdentifier
  -- * Lenses
  , riResourceArn
  , riResourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.ResourceArn as Types
import qualified Network.AWS.ResourceGroups.Types.ResourceType as Types

-- | The ARN of a resource, and its resource type.
--
-- /See:/ 'mkResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { resourceArn :: Core.Maybe Types.ResourceArn
    -- ^ The ARN of a resource.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type of a resource, such as @AWS::EC2::Instance@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceIdentifier' value with any optional fields omitted.
mkResourceIdentifier
    :: ResourceIdentifier
mkResourceIdentifier
  = ResourceIdentifier'{resourceArn = Core.Nothing,
                        resourceType = Core.Nothing}

-- | The ARN of a resource.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceArn :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.ResourceArn)
riResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE riResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The resource type of a resource, such as @AWS::EC2::Instance@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceType :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.ResourceType)
riResourceType = Lens.field @"resourceType"
{-# INLINEABLE riResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON ResourceIdentifier where
        parseJSON
          = Core.withObject "ResourceIdentifier" Core.$
              \ x ->
                ResourceIdentifier' Core.<$>
                  (x Core..:? "ResourceArn") Core.<*> x Core..:? "ResourceType"
