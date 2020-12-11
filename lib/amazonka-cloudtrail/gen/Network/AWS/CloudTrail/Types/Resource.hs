-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.Resource
  ( Resource (..),

    -- * Smart constructor
    mkResource,

    -- * Lenses
    rResourceType,
    rResourceName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the type and name of a resource referenced by an event.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { resourceType :: Lude.Maybe Lude.Text,
    resourceName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- * 'resourceName' - The name of the resource referenced by the event returned. These are user-created names whose values will depend on the environment. For example, the resource name might be "auto-scaling-test-group" for an Auto Scaling Group or "i-1234567" for an EC2 Instance.
-- * 'resourceType' - The type of a resource referenced by the event returned. When the resource type cannot be determined, null is returned. Some examples of resource types are: __Instance__ for EC2, __Trail__ for CloudTrail, __DBInstance__ for RDS, and __AccessKey__ for IAM. To learn more about how to look up and filter events by the resource types supported for a service, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/view-cloudtrail-events-console.html#filtering-cloudtrail-events Filtering CloudTrail Events> .
mkResource ::
  Resource
mkResource =
  Resource'
    { resourceType = Lude.Nothing,
      resourceName = Lude.Nothing
    }

-- | The type of a resource referenced by the event returned. When the resource type cannot be determined, null is returned. Some examples of resource types are: __Instance__ for EC2, __Trail__ for CloudTrail, __DBInstance__ for RDS, and __AccessKey__ for IAM. To learn more about how to look up and filter events by the resource types supported for a service, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/view-cloudtrail-events-console.html#filtering-cloudtrail-events Filtering CloudTrail Events> .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceType :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rResourceType = Lens.lens (resourceType :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: Resource)
{-# DEPRECATED rResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The name of the resource referenced by the event returned. These are user-created names whose values will depend on the environment. For example, the resource name might be "auto-scaling-test-group" for an Auto Scaling Group or "i-1234567" for an EC2 Instance.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceName :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rResourceName = Lens.lens (resourceName :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: Resource)
{-# DEPRECATED rResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Lude.FromJSON Resource where
  parseJSON =
    Lude.withObject
      "Resource"
      ( \x ->
          Resource'
            Lude.<$> (x Lude..:? "ResourceType") Lude.<*> (x Lude..:? "ResourceName")
      )
