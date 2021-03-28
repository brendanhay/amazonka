{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DB parameter group. The DB parameter group to be deleted can't be associated with any DB instances.
module Network.AWS.RDS.DeleteDBParameterGroup
    (
    -- * Creating a request
      DeleteDBParameterGroup (..)
    , mkDeleteDBParameterGroup
    -- ** Request lenses
    , dDBParameterGroupName

    -- * Destructuring the response
    , DeleteDBParameterGroupResponse (..)
    , mkDeleteDBParameterGroupResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteDBParameterGroup' smart constructor.
newtype DeleteDBParameterGroup = DeleteDBParameterGroup'
  { dBParameterGroupName :: Core.Text
    -- ^ The name of the DB parameter group.
--
-- Constraints:
--
--     * Must be the name of an existing DB parameter group
--
--
--     * You can't delete a default DB parameter group
--
--
--     * Can't be associated with any DB instances
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBParameterGroup' value with any optional fields omitted.
mkDeleteDBParameterGroup
    :: Core.Text -- ^ 'dBParameterGroupName'
    -> DeleteDBParameterGroup
mkDeleteDBParameterGroup dBParameterGroupName
  = DeleteDBParameterGroup'{dBParameterGroupName}

-- | The name of the DB parameter group.
--
-- Constraints:
--
--     * Must be the name of an existing DB parameter group
--
--
--     * You can't delete a default DB parameter group
--
--
--     * Can't be associated with any DB instances
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBParameterGroupName :: Lens.Lens' DeleteDBParameterGroup Core.Text
dDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE dDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

instance Core.ToQuery DeleteDBParameterGroup where
        toQuery DeleteDBParameterGroup{..}
          = Core.toQueryPair "Action" ("DeleteDBParameterGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBParameterGroupName" dBParameterGroupName

instance Core.ToHeaders DeleteDBParameterGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDBParameterGroup where
        type Rs DeleteDBParameterGroup = DeleteDBParameterGroupResponse
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
          = Response.receiveNull DeleteDBParameterGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDBParameterGroupResponse' smart constructor.
data DeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBParameterGroupResponse' value with any optional fields omitted.
mkDeleteDBParameterGroupResponse
    :: DeleteDBParameterGroupResponse
mkDeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse'
