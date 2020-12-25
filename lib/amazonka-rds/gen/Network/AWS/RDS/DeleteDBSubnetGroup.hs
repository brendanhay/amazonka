{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteDBSubnetGroup (..),
    mkDeleteDBSubnetGroup,

    -- ** Request lenses
    ddbsgfDBSubnetGroupName,

    -- * Destructuring the response
    DeleteDBSubnetGroupResponse (..),
    mkDeleteDBSubnetGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteDBSubnetGroup' smart constructor.
newtype DeleteDBSubnetGroup = DeleteDBSubnetGroup'
  { -- | The name of the database subnet group to delete.
    --
    -- Constraints:
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
    -- Example: @mySubnetgroup@
    dBSubnetGroupName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSubnetGroup' value with any optional fields omitted.
mkDeleteDBSubnetGroup ::
  -- | 'dBSubnetGroupName'
  Types.String ->
  DeleteDBSubnetGroup
mkDeleteDBSubnetGroup dBSubnetGroupName =
  DeleteDBSubnetGroup' {dBSubnetGroupName}

-- | The name of the database subnet group to delete.
--
-- Constraints:
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgfDBSubnetGroupName :: Lens.Lens' DeleteDBSubnetGroup Types.String
ddbsgfDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED ddbsgfDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

instance Core.AWSRequest DeleteDBSubnetGroup where
  type Rs DeleteDBSubnetGroup = DeleteDBSubnetGroupResponse
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
            ( Core.pure ("Action", "DeleteDBSubnetGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSubnetGroupName" dBSubnetGroupName)
            )
      }
  response = Response.receiveNull DeleteDBSubnetGroupResponse'

-- | /See:/ 'mkDeleteDBSubnetGroupResponse' smart constructor.
data DeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSubnetGroupResponse' value with any optional fields omitted.
mkDeleteDBSubnetGroupResponse ::
  DeleteDBSubnetGroupResponse
mkDeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse'
