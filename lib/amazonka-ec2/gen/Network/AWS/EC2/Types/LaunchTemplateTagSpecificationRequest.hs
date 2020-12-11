-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
  ( LaunchTemplateTagSpecificationRequest (..),

    -- * Smart constructor
    mkLaunchTemplateTagSpecificationRequest,

    -- * Lenses
    lttsrResourceType,
    lttsrTags,
  )
where

import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The tags specification for the launch template.
--
-- /See:/ 'mkLaunchTemplateTagSpecificationRequest' smart constructor.
data LaunchTemplateTagSpecificationRequest = LaunchTemplateTagSpecificationRequest'
  { resourceType ::
      Lude.Maybe
        ResourceType,
    tags ::
      Lude.Maybe
        [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateTagSpecificationRequest' with the minimum fields required to make a request.
--
-- * 'resourceType' - The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ . To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
-- * 'tags' - The tags to apply to the resource.
mkLaunchTemplateTagSpecificationRequest ::
  LaunchTemplateTagSpecificationRequest
mkLaunchTemplateTagSpecificationRequest =
  LaunchTemplateTagSpecificationRequest'
    { resourceType =
        Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ . To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttsrResourceType :: Lens.Lens' LaunchTemplateTagSpecificationRequest (Lude.Maybe ResourceType)
lttsrResourceType = Lens.lens (resourceType :: LaunchTemplateTagSpecificationRequest -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: LaunchTemplateTagSpecificationRequest)
{-# DEPRECATED lttsrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tags to apply to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttsrTags :: Lens.Lens' LaunchTemplateTagSpecificationRequest (Lude.Maybe [Tag])
lttsrTags = Lens.lens (tags :: LaunchTemplateTagSpecificationRequest -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LaunchTemplateTagSpecificationRequest)
{-# DEPRECATED lttsrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToQuery LaunchTemplateTagSpecificationRequest where
  toQuery LaunchTemplateTagSpecificationRequest' {..} =
    Lude.mconcat
      [ "ResourceType" Lude.=: resourceType,
        Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]
