{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ResourceTagSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResourceTagSet
  ( ResourceTagSet (..),

    -- * Smart constructor
    mkResourceTagSet,

    -- * Lenses
    rtsResourceId,
    rtsResourceType,
    rtsTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.Tag
import Network.AWS.Route53.Types.TagResourceType

-- | A complex type containing a resource and its associated tags.
--
-- /See:/ 'mkResourceTagSet' smart constructor.
data ResourceTagSet = ResourceTagSet'
  { -- | The ID for the specified resource.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The type of the resource.
    --
    --
    --     * The resource type for health checks is @healthcheck@ .
    --
    --
    --     * The resource type for hosted zones is @hostedzone@ .
    resourceType :: Lude.Maybe TagResourceType,
    -- | The tags associated with the specified resource.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceTagSet' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID for the specified resource.
-- * 'resourceType' - The type of the resource.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
--
--
-- * 'tags' - The tags associated with the specified resource.
mkResourceTagSet ::
  ResourceTagSet
mkResourceTagSet =
  ResourceTagSet'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID for the specified resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsResourceId :: Lens.Lens' ResourceTagSet (Lude.Maybe Lude.Text)
rtsResourceId = Lens.lens (resourceId :: ResourceTagSet -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ResourceTagSet)
{-# DEPRECATED rtsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the resource.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
--
--
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsResourceType :: Lens.Lens' ResourceTagSet (Lude.Maybe TagResourceType)
rtsResourceType = Lens.lens (resourceType :: ResourceTagSet -> Lude.Maybe TagResourceType) (\s a -> s {resourceType = a} :: ResourceTagSet)
{-# DEPRECATED rtsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tags associated with the specified resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTags :: Lens.Lens' ResourceTagSet (Lude.Maybe (Lude.NonEmpty Tag))
rtsTags = Lens.lens (tags :: ResourceTagSet -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: ResourceTagSet)
{-# DEPRECATED rtsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ResourceTagSet where
  parseXML x =
    ResourceTagSet'
      Lude.<$> (x Lude..@? "ResourceId")
      Lude.<*> (x Lude..@? "ResourceType")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLNonEmpty "Tag")
               )
