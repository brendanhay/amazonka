{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified Amazon Redshift parameter group.
module Network.AWS.Redshift.DeleteClusterParameterGroup
    (
    -- * Creating a request
      DeleteClusterParameterGroup (..)
    , mkDeleteClusterParameterGroup
    -- ** Request lenses
    , dParameterGroupName

    -- * Destructuring the response
    , DeleteClusterParameterGroupResponse (..)
    , mkDeleteClusterParameterGroupResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteClusterParameterGroup' smart constructor.
newtype DeleteClusterParameterGroup = DeleteClusterParameterGroup'
  { parameterGroupName :: Core.Text
    -- ^ The name of the parameter group to be deleted.
--
-- Constraints:
--
--     * Must be the name of an existing cluster parameter group.
--
--
--     * Cannot delete a default cluster parameter group.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterParameterGroup' value with any optional fields omitted.
mkDeleteClusterParameterGroup
    :: Core.Text -- ^ 'parameterGroupName'
    -> DeleteClusterParameterGroup
mkDeleteClusterParameterGroup parameterGroupName
  = DeleteClusterParameterGroup'{parameterGroupName}

-- | The name of the parameter group to be deleted.
--
-- Constraints:
--
--     * Must be the name of an existing cluster parameter group.
--
--
--     * Cannot delete a default cluster parameter group.
--
--
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dParameterGroupName :: Lens.Lens' DeleteClusterParameterGroup Core.Text
dParameterGroupName = Lens.field @"parameterGroupName"
{-# INLINEABLE dParameterGroupName #-}
{-# DEPRECATED parameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead"  #-}

instance Core.ToQuery DeleteClusterParameterGroup where
        toQuery DeleteClusterParameterGroup{..}
          = Core.toQueryPair "Action"
              ("DeleteClusterParameterGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ParameterGroupName" parameterGroupName

instance Core.ToHeaders DeleteClusterParameterGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteClusterParameterGroup where
        type Rs DeleteClusterParameterGroup =
             DeleteClusterParameterGroupResponse
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
          = Response.receiveNull DeleteClusterParameterGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClusterParameterGroupResponse' smart constructor.
data DeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterParameterGroupResponse' value with any optional fields omitted.
mkDeleteClusterParameterGroupResponse
    :: DeleteClusterParameterGroupResponse
mkDeleteClusterParameterGroupResponse
  = DeleteClusterParameterGroupResponse'
