{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.TaggedResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.TaggedResource
  ( TaggedResource (..)
  -- * Smart constructor
  , mkTaggedResource
  -- * Lenses
  , trResourceName
  , trResourceType
  , trTag
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | A tag and its associated resource.
--
-- /See:/ 'mkTaggedResource' smart constructor.
data TaggedResource = TaggedResource'
  { resourceName :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) with which the tag is associated, for example: @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
  , resourceType :: Core.Maybe Core.Text
    -- ^ The type of resource with which the tag is associated. Valid resource types are: 
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
  , tag :: Core.Maybe Types.Tag
    -- ^ The tag for the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaggedResource' value with any optional fields omitted.
mkTaggedResource
    :: TaggedResource
mkTaggedResource
  = TaggedResource'{resourceName = Core.Nothing,
                    resourceType = Core.Nothing, tag = Core.Nothing}

-- | The Amazon Resource Name (ARN) with which the tag is associated, for example: @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceName :: Lens.Lens' TaggedResource (Core.Maybe Core.Text)
trResourceName = Lens.field @"resourceName"
{-# INLINEABLE trResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

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
trResourceType :: Lens.Lens' TaggedResource (Core.Maybe Core.Text)
trResourceType = Lens.field @"resourceType"
{-# INLINEABLE trResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The tag for the resource.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTag :: Lens.Lens' TaggedResource (Core.Maybe Types.Tag)
trTag = Lens.field @"tag"
{-# INLINEABLE trTag #-}
{-# DEPRECATED tag "Use generic-lens or generic-optics with 'tag' instead"  #-}

instance Core.FromXML TaggedResource where
        parseXML x
          = TaggedResource' Core.<$>
              (x Core..@? "ResourceName") Core.<*> x Core..@? "ResourceType"
                Core.<*> x Core..@? "Tag"
