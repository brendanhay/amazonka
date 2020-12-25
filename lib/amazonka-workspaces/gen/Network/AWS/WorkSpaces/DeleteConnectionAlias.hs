{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DeleteConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified connection alias. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
-- /Important:/ __If you will no longer be using a fully qualified domain name (FQDN) as the registration code for your WorkSpaces users, you must take certain precautions to prevent potential security issues.__ For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html#cross-region-redirection-security-considerations Security Considerations if You Stop Using Cross-Region Redirection> .
module Network.AWS.WorkSpaces.DeleteConnectionAlias
  ( -- * Creating a request
    DeleteConnectionAlias (..),
    mkDeleteConnectionAlias,

    -- ** Request lenses
    dAliasId,

    -- * Destructuring the response
    DeleteConnectionAliasResponse (..),
    mkDeleteConnectionAliasResponse,

    -- ** Response lenses
    dcarfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDeleteConnectionAlias' smart constructor.
newtype DeleteConnectionAlias = DeleteConnectionAlias'
  { -- | The identifier of the connection alias to delete.
    aliasId :: Types.AliasId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnectionAlias' value with any optional fields omitted.
mkDeleteConnectionAlias ::
  -- | 'aliasId'
  Types.AliasId ->
  DeleteConnectionAlias
mkDeleteConnectionAlias aliasId = DeleteConnectionAlias' {aliasId}

-- | The identifier of the connection alias to delete.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAliasId :: Lens.Lens' DeleteConnectionAlias Types.AliasId
dAliasId = Lens.field @"aliasId"
{-# DEPRECATED dAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

instance Core.FromJSON DeleteConnectionAlias where
  toJSON DeleteConnectionAlias {..} =
    Core.object
      (Core.catMaybes [Core.Just ("AliasId" Core..= aliasId)])

instance Core.AWSRequest DeleteConnectionAlias where
  type Rs DeleteConnectionAlias = DeleteConnectionAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.DeleteConnectionAlias")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConnectionAliasResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteConnectionAliasResponse' smart constructor.
newtype DeleteConnectionAliasResponse = DeleteConnectionAliasResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnectionAliasResponse' value with any optional fields omitted.
mkDeleteConnectionAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteConnectionAliasResponse
mkDeleteConnectionAliasResponse responseStatus =
  DeleteConnectionAliasResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarfrsResponseStatus :: Lens.Lens' DeleteConnectionAliasResponse Core.Int
dcarfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
