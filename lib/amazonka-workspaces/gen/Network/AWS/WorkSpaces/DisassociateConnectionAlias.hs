{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DisassociateConnectionAlias (..)
    , mkDisassociateConnectionAlias
    -- ** Request lenses
    , dcaAliasId

    -- * Destructuring the response
    , DisassociateConnectionAliasResponse (..)
    , mkDisassociateConnectionAliasResponse
    -- ** Response lenses
    , dcarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDisassociateConnectionAlias' smart constructor.
newtype DisassociateConnectionAlias = DisassociateConnectionAlias'
  { aliasId :: Types.ConnectionAliasId
    -- ^ The identifier of the connection alias to disassociate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateConnectionAlias' value with any optional fields omitted.
mkDisassociateConnectionAlias
    :: Types.ConnectionAliasId -- ^ 'aliasId'
    -> DisassociateConnectionAlias
mkDisassociateConnectionAlias aliasId
  = DisassociateConnectionAlias'{aliasId}

-- | The identifier of the connection alias to disassociate.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaAliasId :: Lens.Lens' DisassociateConnectionAlias Types.ConnectionAliasId
dcaAliasId = Lens.field @"aliasId"
{-# INLINEABLE dcaAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

instance Core.ToQuery DisassociateConnectionAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateConnectionAlias where
        toHeaders DisassociateConnectionAlias{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.DisassociateConnectionAlias")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateConnectionAlias where
        toJSON DisassociateConnectionAlias{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AliasId" Core..= aliasId)])

instance Core.AWSRequest DisassociateConnectionAlias where
        type Rs DisassociateConnectionAlias =
             DisassociateConnectionAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateConnectionAliasResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateConnectionAliasResponse' smart constructor.
newtype DisassociateConnectionAliasResponse = DisassociateConnectionAliasResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateConnectionAliasResponse' value with any optional fields omitted.
mkDisassociateConnectionAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateConnectionAliasResponse
mkDisassociateConnectionAliasResponse responseStatus
  = DisassociateConnectionAliasResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarrsResponseStatus :: Lens.Lens' DisassociateConnectionAliasResponse Core.Int
dcarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
