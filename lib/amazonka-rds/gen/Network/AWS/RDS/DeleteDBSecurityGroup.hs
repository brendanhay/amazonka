{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB security group.
module Network.AWS.RDS.DeleteDBSecurityGroup
    (
    -- * Creating a request
      DeleteDBSecurityGroup (..)
    , mkDeleteDBSecurityGroup
    -- ** Request lenses
    , ddbsgDBSecurityGroupName

    -- * Destructuring the response
    , DeleteDBSecurityGroupResponse (..)
    , mkDeleteDBSecurityGroupResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteDBSecurityGroup' smart constructor.
newtype DeleteDBSecurityGroup = DeleteDBSecurityGroup'
  { dBSecurityGroupName :: Core.Text
    -- ^ The name of the DB security group to delete.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSecurityGroup' value with any optional fields omitted.
mkDeleteDBSecurityGroup
    :: Core.Text -- ^ 'dBSecurityGroupName'
    -> DeleteDBSecurityGroup
mkDeleteDBSecurityGroup dBSecurityGroupName
  = DeleteDBSecurityGroup'{dBSecurityGroupName}

-- | The name of the DB security group to delete.
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
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgDBSecurityGroupName :: Lens.Lens' DeleteDBSecurityGroup Core.Text
ddbsgDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# INLINEABLE ddbsgDBSecurityGroupName #-}
{-# DEPRECATED dBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead"  #-}

instance Core.ToQuery DeleteDBSecurityGroup where
        toQuery DeleteDBSecurityGroup{..}
          = Core.toQueryPair "Action" ("DeleteDBSecurityGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBSecurityGroupName" dBSecurityGroupName

instance Core.ToHeaders DeleteDBSecurityGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDBSecurityGroup where
        type Rs DeleteDBSecurityGroup = DeleteDBSecurityGroupResponse
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
        parseResponse = Response.receiveNull DeleteDBSecurityGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDBSecurityGroupResponse' smart constructor.
data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSecurityGroupResponse' value with any optional fields omitted.
mkDeleteDBSecurityGroupResponse
    :: DeleteDBSecurityGroupResponse
mkDeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'
