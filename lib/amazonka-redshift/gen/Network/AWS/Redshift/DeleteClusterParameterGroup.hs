{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteClusterParameterGroup (..),
    mkDeleteClusterParameterGroup,

    -- ** Request lenses
    dParameterGroupName,

    -- * Destructuring the response
    DeleteClusterParameterGroupResponse (..),
    mkDeleteClusterParameterGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteClusterParameterGroup' smart constructor.
newtype DeleteClusterParameterGroup = DeleteClusterParameterGroup'
  { -- | The name of the parameter group to be deleted.
    --
    -- Constraints:
    --
    --     * Must be the name of an existing cluster parameter group.
    --
    --
    --     * Cannot delete a default cluster parameter group.
    parameterGroupName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterParameterGroup' value with any optional fields omitted.
mkDeleteClusterParameterGroup ::
  -- | 'parameterGroupName'
  Types.String ->
  DeleteClusterParameterGroup
mkDeleteClusterParameterGroup parameterGroupName =
  DeleteClusterParameterGroup' {parameterGroupName}

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
dParameterGroupName :: Lens.Lens' DeleteClusterParameterGroup Types.String
dParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED dParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Core.AWSRequest DeleteClusterParameterGroup where
  type
    Rs DeleteClusterParameterGroup =
      DeleteClusterParameterGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteClusterParameterGroup")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ParameterGroupName" parameterGroupName)
            )
      }
  response =
    Response.receiveNull DeleteClusterParameterGroupResponse'

-- | /See:/ 'mkDeleteClusterParameterGroupResponse' smart constructor.
data DeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterParameterGroupResponse' value with any optional fields omitted.
mkDeleteClusterParameterGroupResponse ::
  DeleteClusterParameterGroupResponse
mkDeleteClusterParameterGroupResponse =
  DeleteClusterParameterGroupResponse'
