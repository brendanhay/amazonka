{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB security group. DB security groups control access to a DB instance.
module Network.AWS.RDS.CreateDBSecurityGroup
    (
    -- * Creating a request
      CreateDBSecurityGroup (..)
    , mkCreateDBSecurityGroup
    -- ** Request lenses
    , cdbsgDBSecurityGroupName
    , cdbsgDBSecurityGroupDescription
    , cdbsgTags

    -- * Destructuring the response
    , CreateDBSecurityGroupResponse (..)
    , mkCreateDBSecurityGroupResponse
    -- ** Response lenses
    , cdbsgrrsDBSecurityGroup
    , cdbsgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateDBSecurityGroup' smart constructor.
data CreateDBSecurityGroup = CreateDBSecurityGroup'
  { dBSecurityGroupName :: Core.Text
    -- ^ The name for the DB security group. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--     * Must not be "Default"
--
--
-- Example: @mysecuritygroup@ 
  , dBSecurityGroupDescription :: Core.Text
    -- ^ The description for the DB security group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Tags to assign to the DB security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBSecurityGroup' value with any optional fields omitted.
mkCreateDBSecurityGroup
    :: Core.Text -- ^ 'dBSecurityGroupName'
    -> Core.Text -- ^ 'dBSecurityGroupDescription'
    -> CreateDBSecurityGroup
mkCreateDBSecurityGroup dBSecurityGroupName
  dBSecurityGroupDescription
  = CreateDBSecurityGroup'{dBSecurityGroupName,
                           dBSecurityGroupDescription, tags = Core.Nothing}

-- | The name for the DB security group. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--     * Must not be "Default"
--
--
-- Example: @mysecuritygroup@ 
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgDBSecurityGroupName :: Lens.Lens' CreateDBSecurityGroup Core.Text
cdbsgDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# INLINEABLE cdbsgDBSecurityGroupName #-}
{-# DEPRECATED dBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead"  #-}

-- | The description for the DB security group.
--
-- /Note:/ Consider using 'dBSecurityGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgDBSecurityGroupDescription :: Lens.Lens' CreateDBSecurityGroup Core.Text
cdbsgDBSecurityGroupDescription = Lens.field @"dBSecurityGroupDescription"
{-# INLINEABLE cdbsgDBSecurityGroupDescription #-}
{-# DEPRECATED dBSecurityGroupDescription "Use generic-lens or generic-optics with 'dBSecurityGroupDescription' instead"  #-}

-- | Tags to assign to the DB security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgTags :: Lens.Lens' CreateDBSecurityGroup (Core.Maybe [Types.Tag])
cdbsgTags = Lens.field @"tags"
{-# INLINEABLE cdbsgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDBSecurityGroup where
        toQuery CreateDBSecurityGroup{..}
          = Core.toQueryPair "Action" ("CreateDBSecurityGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBSecurityGroupName" dBSecurityGroupName
              Core.<>
              Core.toQueryPair "DBSecurityGroupDescription"
                dBSecurityGroupDescription
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateDBSecurityGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDBSecurityGroup where
        type Rs CreateDBSecurityGroup = CreateDBSecurityGroupResponse
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
          = Response.receiveXMLWrapper "CreateDBSecurityGroupResult"
              (\ s h x ->
                 CreateDBSecurityGroupResponse' Core.<$>
                   (x Core..@? "DBSecurityGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDBSecurityGroupResponse' smart constructor.
data CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse'
  { dBSecurityGroup :: Core.Maybe Types.DBSecurityGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBSecurityGroupResponse' value with any optional fields omitted.
mkCreateDBSecurityGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDBSecurityGroupResponse
mkCreateDBSecurityGroupResponse responseStatus
  = CreateDBSecurityGroupResponse'{dBSecurityGroup = Core.Nothing,
                                   responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgrrsDBSecurityGroup :: Lens.Lens' CreateDBSecurityGroupResponse (Core.Maybe Types.DBSecurityGroup)
cdbsgrrsDBSecurityGroup = Lens.field @"dBSecurityGroup"
{-# INLINEABLE cdbsgrrsDBSecurityGroup #-}
{-# DEPRECATED dBSecurityGroup "Use generic-lens or generic-optics with 'dBSecurityGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgrrsResponseStatus :: Lens.Lens' CreateDBSecurityGroupResponse Core.Int
cdbsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdbsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
