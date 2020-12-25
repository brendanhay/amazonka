{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteDBSecurityGroup (..),
    mkDeleteDBSecurityGroup,

    -- ** Request lenses
    ddbsgDBSecurityGroupName,

    -- * Destructuring the response
    DeleteDBSecurityGroupResponse (..),
    mkDeleteDBSecurityGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteDBSecurityGroup' smart constructor.
newtype DeleteDBSecurityGroup = DeleteDBSecurityGroup'
  { -- | The name of the DB security group to delete.
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
    dBSecurityGroupName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSecurityGroup' value with any optional fields omitted.
mkDeleteDBSecurityGroup ::
  -- | 'dBSecurityGroupName'
  Types.String ->
  DeleteDBSecurityGroup
mkDeleteDBSecurityGroup dBSecurityGroupName =
  DeleteDBSecurityGroup' {dBSecurityGroupName}

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
ddbsgDBSecurityGroupName :: Lens.Lens' DeleteDBSecurityGroup Types.String
ddbsgDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# DEPRECATED ddbsgDBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead." #-}

instance Core.AWSRequest DeleteDBSecurityGroup where
  type Rs DeleteDBSecurityGroup = DeleteDBSecurityGroupResponse
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
            ( Core.pure ("Action", "DeleteDBSecurityGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSecurityGroupName" dBSecurityGroupName)
            )
      }
  response = Response.receiveNull DeleteDBSecurityGroupResponse'

-- | /See:/ 'mkDeleteDBSecurityGroupResponse' smart constructor.
data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSecurityGroupResponse' value with any optional fields omitted.
mkDeleteDBSecurityGroupResponse ::
  DeleteDBSecurityGroupResponse
mkDeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'
