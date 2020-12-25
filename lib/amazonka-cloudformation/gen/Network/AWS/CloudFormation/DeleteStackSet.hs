{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DeleteStackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a stack set. Before you can delete a stack set, all of its member stack instances must be deleted. For more information about how to do this, see 'DeleteStackInstances' .
module Network.AWS.CloudFormation.DeleteStackSet
  ( -- * Creating a request
    DeleteStackSet (..),
    mkDeleteStackSet,

    -- ** Request lenses
    dssStackSetName,

    -- * Destructuring the response
    DeleteStackSetResponse (..),
    mkDeleteStackSetResponse,

    -- ** Response lenses
    dssrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteStackSet' smart constructor.
newtype DeleteStackSet = DeleteStackSet'
  { -- | The name or unique ID of the stack set that you're deleting. You can obtain this value by running 'ListStackSets' .
    stackSetName :: Types.StackSetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStackSet' value with any optional fields omitted.
mkDeleteStackSet ::
  -- | 'stackSetName'
  Types.StackSetName ->
  DeleteStackSet
mkDeleteStackSet stackSetName = DeleteStackSet' {stackSetName}

-- | The name or unique ID of the stack set that you're deleting. You can obtain this value by running 'ListStackSets' .
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssStackSetName :: Lens.Lens' DeleteStackSet Types.StackSetName
dssStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED dssStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

instance Core.AWSRequest DeleteStackSet where
  type Rs DeleteStackSet = DeleteStackSetResponse
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
            ( Core.pure ("Action", "DeleteStackSet")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteStackSetResult"
      ( \s h x ->
          DeleteStackSetResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteStackSetResponse' smart constructor.
newtype DeleteStackSetResponse = DeleteStackSetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStackSetResponse' value with any optional fields omitted.
mkDeleteStackSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteStackSetResponse
mkDeleteStackSetResponse responseStatus =
  DeleteStackSetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsResponseStatus :: Lens.Lens' DeleteStackSetResponse Core.Int
dssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
