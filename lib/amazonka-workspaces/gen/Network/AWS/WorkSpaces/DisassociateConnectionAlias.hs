{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DisassociateConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a connection alias from a directory. Disassociating a connection alias disables cross-Region redirection between two directories in different AWS Regions. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.DisassociateConnectionAlias
  ( -- * Creating a request
    DisassociateConnectionAlias (..),
    mkDisassociateConnectionAlias,

    -- ** Request lenses
    dcaAliasId,

    -- * Destructuring the response
    DisassociateConnectionAliasResponse (..),
    mkDisassociateConnectionAliasResponse,

    -- ** Response lenses
    dcarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDisassociateConnectionAlias' smart constructor.
newtype DisassociateConnectionAlias = DisassociateConnectionAlias'
  { -- | The identifier of the connection alias to disassociate.
    aliasId :: Types.ConnectionAliasId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateConnectionAlias' value with any optional fields omitted.
mkDisassociateConnectionAlias ::
  -- | 'aliasId'
  Types.ConnectionAliasId ->
  DisassociateConnectionAlias
mkDisassociateConnectionAlias aliasId =
  DisassociateConnectionAlias' {aliasId}

-- | The identifier of the connection alias to disassociate.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaAliasId :: Lens.Lens' DisassociateConnectionAlias Types.ConnectionAliasId
dcaAliasId = Lens.field @"aliasId"
{-# DEPRECATED dcaAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

instance Core.FromJSON DisassociateConnectionAlias where
  toJSON DisassociateConnectionAlias {..} =
    Core.object
      (Core.catMaybes [Core.Just ("AliasId" Core..= aliasId)])

instance Core.AWSRequest DisassociateConnectionAlias where
  type
    Rs DisassociateConnectionAlias =
      DisassociateConnectionAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.DisassociateConnectionAlias")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateConnectionAliasResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateConnectionAliasResponse' smart constructor.
newtype DisassociateConnectionAliasResponse = DisassociateConnectionAliasResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateConnectionAliasResponse' value with any optional fields omitted.
mkDisassociateConnectionAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateConnectionAliasResponse
mkDisassociateConnectionAliasResponse responseStatus =
  DisassociateConnectionAliasResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarrsResponseStatus :: Lens.Lens' DisassociateConnectionAliasResponse Core.Int
dcarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
