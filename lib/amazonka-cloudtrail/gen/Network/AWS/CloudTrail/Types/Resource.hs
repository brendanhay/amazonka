{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.Resource
  ( Resource (..)
  -- * Smart constructor
  , mkResource
  -- * Lenses
  , rResourceName
  , rResourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the type and name of a resource referenced by an event.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { resourceName :: Core.Maybe Core.Text
    -- ^ The name of the resource referenced by the event returned. These are user-created names whose values will depend on the environment. For example, the resource name might be "auto-scaling-test-group" for an Auto Scaling Group or "i-1234567" for an EC2 Instance.
  , resourceType :: Core.Maybe Core.Text
    -- ^ The type of a resource referenced by the event returned. When the resource type cannot be determined, null is returned. Some examples of resource types are: __Instance__ for EC2, __Trail__ for CloudTrail, __DBInstance__ for RDS, and __AccessKey__ for IAM. To learn more about how to look up and filter events by the resource types supported for a service, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/view-cloudtrail-events-console.html#filtering-cloudtrail-events Filtering CloudTrail Events> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Resource' value with any optional fields omitted.
mkResource
    :: Resource
mkResource
  = Resource'{resourceName = Core.Nothing,
              resourceType = Core.Nothing}

-- | The name of the resource referenced by the event returned. These are user-created names whose values will depend on the environment. For example, the resource name might be "auto-scaling-test-group" for an Auto Scaling Group or "i-1234567" for an EC2 Instance.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceName :: Lens.Lens' Resource (Core.Maybe Core.Text)
rResourceName = Lens.field @"resourceName"
{-# INLINEABLE rResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | The type of a resource referenced by the event returned. When the resource type cannot be determined, null is returned. Some examples of resource types are: __Instance__ for EC2, __Trail__ for CloudTrail, __DBInstance__ for RDS, and __AccessKey__ for IAM. To learn more about how to look up and filter events by the resource types supported for a service, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/view-cloudtrail-events-console.html#filtering-cloudtrail-events Filtering CloudTrail Events> .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceType :: Lens.Lens' Resource (Core.Maybe Core.Text)
rResourceType = Lens.field @"resourceType"
{-# INLINEABLE rResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON Resource where
        parseJSON
          = Core.withObject "Resource" Core.$
              \ x ->
                Resource' Core.<$>
                  (x Core..:? "ResourceName") Core.<*> x Core..:? "ResourceType"
