{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB subnet group.
module Network.AWS.RDS.DeleteDBSubnetGroup
    (
    -- * Creating a request
      DeleteDBSubnetGroup (..)
    , mkDeleteDBSubnetGroup
    -- ** Request lenses
    , ddbsgfDBSubnetGroupName

    -- * Destructuring the response
    , DeleteDBSubnetGroupResponse (..)
    , mkDeleteDBSubnetGroupResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteDBSubnetGroup' smart constructor.
newtype DeleteDBSubnetGroup = DeleteDBSubnetGroup'
  { dBSubnetGroupName :: Core.Text
    -- ^ The name of the database subnet group to delete.
--
-- Constraints:
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSubnetGroup' value with any optional fields omitted.
mkDeleteDBSubnetGroup
    :: Core.Text -- ^ 'dBSubnetGroupName'
    -> DeleteDBSubnetGroup
mkDeleteDBSubnetGroup dBSubnetGroupName
  = DeleteDBSubnetGroup'{dBSubnetGroupName}

-- | The name of the database subnet group to delete.
--
-- Constraints:
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@ 
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgfDBSubnetGroupName :: Lens.Lens' DeleteDBSubnetGroup Core.Text
ddbsgfDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# INLINEABLE ddbsgfDBSubnetGroupName #-}
{-# DEPRECATED dBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead"  #-}

instance Core.ToQuery DeleteDBSubnetGroup where
        toQuery DeleteDBSubnetGroup{..}
          = Core.toQueryPair "Action" ("DeleteDBSubnetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBSubnetGroupName" dBSubnetGroupName

instance Core.ToHeaders DeleteDBSubnetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDBSubnetGroup where
        type Rs DeleteDBSubnetGroup = DeleteDBSubnetGroupResponse
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
        parseResponse = Response.receiveNull DeleteDBSubnetGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDBSubnetGroupResponse' smart constructor.
data DeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSubnetGroupResponse' value with any optional fields omitted.
mkDeleteDBSubnetGroupResponse
    :: DeleteDBSubnetGroupResponse
mkDeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse'
