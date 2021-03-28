{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift subnet group. You must provide a list of one or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC) when creating Amazon Redshift subnet group.
--
-- For information about subnet groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-cluster-subnet-groups.html Amazon Redshift Cluster Subnet Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateClusterSubnetGroup
    (
    -- * Creating a request
      CreateClusterSubnetGroup (..)
    , mkCreateClusterSubnetGroup
    -- ** Request lenses
    , ccsgClusterSubnetGroupName
    , ccsgDescription
    , ccsgSubnetIds
    , ccsgTags

    -- * Destructuring the response
    , CreateClusterSubnetGroupResponse (..)
    , mkCreateClusterSubnetGroupResponse
    -- ** Response lenses
    , ccsgrrsClusterSubnetGroup
    , ccsgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateClusterSubnetGroup' smart constructor.
data CreateClusterSubnetGroup = CreateClusterSubnetGroup'
  { clusterSubnetGroupName :: Core.Text
    -- ^ The name for the subnet group. Amazon Redshift stores the value as a lowercase string.
--
-- Constraints:
--
--     * Must contain no more than 255 alphanumeric characters or hyphens.
--
--
--     * Must not be "Default".
--
--
--     * Must be unique for all subnet groups that are created by your AWS account.
--
--
-- Example: @examplesubnetgroup@ 
  , description :: Core.Text
    -- ^ A description for the subnet group.
  , subnetIds :: [Core.Text]
    -- ^ An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tag instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterSubnetGroup' value with any optional fields omitted.
mkCreateClusterSubnetGroup
    :: Core.Text -- ^ 'clusterSubnetGroupName'
    -> Core.Text -- ^ 'description'
    -> CreateClusterSubnetGroup
mkCreateClusterSubnetGroup clusterSubnetGroupName description
  = CreateClusterSubnetGroup'{clusterSubnetGroupName, description,
                              subnetIds = Core.mempty, tags = Core.Nothing}

-- | The name for the subnet group. Amazon Redshift stores the value as a lowercase string.
--
-- Constraints:
--
--     * Must contain no more than 255 alphanumeric characters or hyphens.
--
--
--     * Must not be "Default".
--
--
--     * Must be unique for all subnet groups that are created by your AWS account.
--
--
-- Example: @examplesubnetgroup@ 
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgClusterSubnetGroupName :: Lens.Lens' CreateClusterSubnetGroup Core.Text
ccsgClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# INLINEABLE ccsgClusterSubnetGroupName #-}
{-# DEPRECATED clusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead"  #-}

-- | A description for the subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgDescription :: Lens.Lens' CreateClusterSubnetGroup Core.Text
ccsgDescription = Lens.field @"description"
{-# INLINEABLE ccsgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgSubnetIds :: Lens.Lens' CreateClusterSubnetGroup [Core.Text]
ccsgSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE ccsgSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgTags :: Lens.Lens' CreateClusterSubnetGroup (Core.Maybe [Types.Tag])
ccsgTags = Lens.field @"tags"
{-# INLINEABLE ccsgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateClusterSubnetGroup where
        toQuery CreateClusterSubnetGroup{..}
          = Core.toQueryPair "Action"
              ("CreateClusterSubnetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ClusterSubnetGroupName" clusterSubnetGroupName
              Core.<> Core.toQueryPair "Description" description
              Core.<>
              Core.toQueryPair "SubnetIds"
                (Core.toQueryList "SubnetIdentifier" subnetIds)
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateClusterSubnetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateClusterSubnetGroup where
        type Rs CreateClusterSubnetGroup = CreateClusterSubnetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateClusterSubnetGroupResult"
              (\ s h x ->
                 CreateClusterSubnetGroupResponse' Core.<$>
                   (x Core..@? "ClusterSubnetGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClusterSubnetGroupResponse' smart constructor.
data CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse'
  { clusterSubnetGroup :: Core.Maybe Types.ClusterSubnetGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterSubnetGroupResponse' value with any optional fields omitted.
mkCreateClusterSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClusterSubnetGroupResponse
mkCreateClusterSubnetGroupResponse responseStatus
  = CreateClusterSubnetGroupResponse'{clusterSubnetGroup =
                                        Core.Nothing,
                                      responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgrrsClusterSubnetGroup :: Lens.Lens' CreateClusterSubnetGroupResponse (Core.Maybe Types.ClusterSubnetGroup)
ccsgrrsClusterSubnetGroup = Lens.field @"clusterSubnetGroup"
{-# INLINEABLE ccsgrrsClusterSubnetGroup #-}
{-# DEPRECATED clusterSubnetGroup "Use generic-lens or generic-optics with 'clusterSubnetGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgrrsResponseStatus :: Lens.Lens' CreateClusterSubnetGroupResponse Core.Int
ccsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
