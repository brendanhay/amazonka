{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift security group. You use security groups to control access to non-VPC clusters.
--
-- For information about managing security groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateClusterSecurityGroup
    (
    -- * Creating a request
      CreateClusterSecurityGroup (..)
    , mkCreateClusterSecurityGroup
    -- ** Request lenses
    , ccsgfClusterSecurityGroupName
    , ccsgfDescription
    , ccsgfTags

    -- * Destructuring the response
    , CreateClusterSecurityGroupResponse (..)
    , mkCreateClusterSecurityGroupResponse
    -- ** Response lenses
    , crsClusterSecurityGroup
    , crsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateClusterSecurityGroup' smart constructor.
data CreateClusterSecurityGroup = CreateClusterSecurityGroup'
  { clusterSecurityGroupName :: Core.Text
    -- ^ The name for the security group. Amazon Redshift stores the value as a lowercase string.
--
-- Constraints:
--
--     * Must contain no more than 255 alphanumeric characters or hyphens.
--
--
--     * Must not be "Default".
--
--
--     * Must be unique for all security groups that are created by your AWS account.
--
--
-- Example: @examplesecuritygroup@ 
  , description :: Core.Text
    -- ^ A description for the security group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tag instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterSecurityGroup' value with any optional fields omitted.
mkCreateClusterSecurityGroup
    :: Core.Text -- ^ 'clusterSecurityGroupName'
    -> Core.Text -- ^ 'description'
    -> CreateClusterSecurityGroup
mkCreateClusterSecurityGroup clusterSecurityGroupName description
  = CreateClusterSecurityGroup'{clusterSecurityGroupName,
                                description, tags = Core.Nothing}

-- | The name for the security group. Amazon Redshift stores the value as a lowercase string.
--
-- Constraints:
--
--     * Must contain no more than 255 alphanumeric characters or hyphens.
--
--
--     * Must not be "Default".
--
--
--     * Must be unique for all security groups that are created by your AWS account.
--
--
-- Example: @examplesecuritygroup@ 
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgfClusterSecurityGroupName :: Lens.Lens' CreateClusterSecurityGroup Core.Text
ccsgfClusterSecurityGroupName = Lens.field @"clusterSecurityGroupName"
{-# INLINEABLE ccsgfClusterSecurityGroupName #-}
{-# DEPRECATED clusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead"  #-}

-- | A description for the security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgfDescription :: Lens.Lens' CreateClusterSecurityGroup Core.Text
ccsgfDescription = Lens.field @"description"
{-# INLINEABLE ccsgfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgfTags :: Lens.Lens' CreateClusterSecurityGroup (Core.Maybe [Types.Tag])
ccsgfTags = Lens.field @"tags"
{-# INLINEABLE ccsgfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateClusterSecurityGroup where
        toQuery CreateClusterSecurityGroup{..}
          = Core.toQueryPair "Action"
              ("CreateClusterSecurityGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ClusterSecurityGroupName"
                clusterSecurityGroupName
              Core.<> Core.toQueryPair "Description" description
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateClusterSecurityGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateClusterSecurityGroup where
        type Rs CreateClusterSecurityGroup =
             CreateClusterSecurityGroupResponse
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
          = Response.receiveXMLWrapper "CreateClusterSecurityGroupResult"
              (\ s h x ->
                 CreateClusterSecurityGroupResponse' Core.<$>
                   (x Core..@? "ClusterSecurityGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClusterSecurityGroupResponse' smart constructor.
data CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse'
  { clusterSecurityGroup :: Core.Maybe Types.ClusterSecurityGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterSecurityGroupResponse' value with any optional fields omitted.
mkCreateClusterSecurityGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClusterSecurityGroupResponse
mkCreateClusterSecurityGroupResponse responseStatus
  = CreateClusterSecurityGroupResponse'{clusterSecurityGroup =
                                          Core.Nothing,
                                        responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsClusterSecurityGroup :: Lens.Lens' CreateClusterSecurityGroupResponse (Core.Maybe Types.ClusterSecurityGroup)
crsClusterSecurityGroup = Lens.field @"clusterSecurityGroup"
{-# INLINEABLE crsClusterSecurityGroup #-}
{-# DEPRECATED clusterSecurityGroup "Use generic-lens or generic-optics with 'clusterSecurityGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateClusterSecurityGroupResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
