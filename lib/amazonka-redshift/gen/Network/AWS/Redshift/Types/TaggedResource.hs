{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.TaggedResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.TaggedResource
  ( TaggedResource (..),

    -- * Smart constructor
    mkTaggedResource,

    -- * Lenses
    trTag,
    trResourceType,
    trResourceName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | A tag and its associated resource.
--
-- /See:/ 'mkTaggedResource' smart constructor.
data TaggedResource = TaggedResource'
  { tag :: Lude.Maybe Tag,
    resourceType :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'TaggedResource' with the minimum fields required to make a request.
--
-- * 'resourceName' - The Amazon Resource Name (ARN) with which the tag is associated, for example: @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
-- * 'resourceType' - The type of resource with which the tag is associated. Valid resource types are:
--
--
--     * Cluster
--
--
--     * CIDR/IP
--
--
--     * EC2 security group
--
--
--     * Snapshot
--
--
--     * Cluster security group
--
--
--     * Subnet group
--
--
--     * HSM connection
--
--
--     * HSM certificate
--
--
--     * Parameter group
--
--
-- For more information about Amazon Redshift resource types and constructing ARNs, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Constructing an Amazon Redshift Amazon Resource Name (ARN)> in the Amazon Redshift Cluster Management Guide.
-- * 'tag' - The tag for the resource.
mkTaggedResource ::
  TaggedResource
mkTaggedResource =
  TaggedResource'
    { tag = Lude.Nothing,
      resourceType = Lude.Nothing,
      resourceName = Lude.Nothing
    }

-- | The tag for the resource.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTag :: Lens.Lens' TaggedResource (Lude.Maybe Tag)
trTag = Lens.lens (tag :: TaggedResource -> Lude.Maybe Tag) (\s a -> s {tag = a} :: TaggedResource)
{-# DEPRECATED trTag "Use generic-lens or generic-optics with 'tag' instead." #-}

-- | The type of resource with which the tag is associated. Valid resource types are:
--
--
--     * Cluster
--
--
--     * CIDR/IP
--
--
--     * EC2 security group
--
--
--     * Snapshot
--
--
--     * Cluster security group
--
--
--     * Subnet group
--
--
--     * HSM connection
--
--
--     * HSM certificate
--
--
--     * Parameter group
--
--
-- For more information about Amazon Redshift resource types and constructing ARNs, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Constructing an Amazon Redshift Amazon Resource Name (ARN)> in the Amazon Redshift Cluster Management Guide.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceType :: Lens.Lens' TaggedResource (Lude.Maybe Lude.Text)
trResourceType = Lens.lens (resourceType :: TaggedResource -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: TaggedResource)
{-# DEPRECATED trResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) with which the tag is associated, for example: @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceName :: Lens.Lens' TaggedResource (Lude.Maybe Lude.Text)
trResourceName = Lens.lens (resourceName :: TaggedResource -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: TaggedResource)
{-# DEPRECATED trResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Lude.FromXML TaggedResource where
  parseXML x =
    TaggedResource'
      Lude.<$> (x Lude..@? "Tag")
      Lude.<*> (x Lude..@? "ResourceType")
      Lude.<*> (x Lude..@? "ResourceName")
