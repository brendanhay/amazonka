{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.AssociateConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified connection alias with the specified directory to enable cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.AssociateConnectionAlias
    (
    -- * Creating a request
      AssociateConnectionAlias (..)
    , mkAssociateConnectionAlias
    -- ** Request lenses
    , acaAliasId
    , acaResourceId

    -- * Destructuring the response
    , AssociateConnectionAliasResponse (..)
    , mkAssociateConnectionAliasResponse
    -- ** Response lenses
    , acarrsConnectionIdentifier
    , acarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkAssociateConnectionAlias' smart constructor.
data AssociateConnectionAlias = AssociateConnectionAlias'
  { aliasId :: Types.AliasId
    -- ^ The identifier of the connection alias.
  , resourceId :: Types.NonEmptyString
    -- ^ The identifier of the directory to associate the connection alias with.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateConnectionAlias' value with any optional fields omitted.
mkAssociateConnectionAlias
    :: Types.AliasId -- ^ 'aliasId'
    -> Types.NonEmptyString -- ^ 'resourceId'
    -> AssociateConnectionAlias
mkAssociateConnectionAlias aliasId resourceId
  = AssociateConnectionAlias'{aliasId, resourceId}

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaAliasId :: Lens.Lens' AssociateConnectionAlias Types.AliasId
acaAliasId = Lens.field @"aliasId"
{-# INLINEABLE acaAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

-- | The identifier of the directory to associate the connection alias with.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaResourceId :: Lens.Lens' AssociateConnectionAlias Types.NonEmptyString
acaResourceId = Lens.field @"resourceId"
{-# INLINEABLE acaResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.ToQuery AssociateConnectionAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateConnectionAlias where
        toHeaders AssociateConnectionAlias{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.AssociateConnectionAlias")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateConnectionAlias where
        toJSON AssociateConnectionAlias{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AliasId" Core..= aliasId),
                  Core.Just ("ResourceId" Core..= resourceId)])

instance Core.AWSRequest AssociateConnectionAlias where
        type Rs AssociateConnectionAlias = AssociateConnectionAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AssociateConnectionAliasResponse' Core.<$>
                   (x Core..:? "ConnectionIdentifier") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateConnectionAliasResponse' smart constructor.
data AssociateConnectionAliasResponse = AssociateConnectionAliasResponse'
  { connectionIdentifier :: Core.Maybe Types.ConnectionIdentifier
    -- ^ The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateConnectionAliasResponse' value with any optional fields omitted.
mkAssociateConnectionAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateConnectionAliasResponse
mkAssociateConnectionAliasResponse responseStatus
  = AssociateConnectionAliasResponse'{connectionIdentifier =
                                        Core.Nothing,
                                      responseStatus}

-- | The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies. 
--
-- /Note:/ Consider using 'connectionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acarrsConnectionIdentifier :: Lens.Lens' AssociateConnectionAliasResponse (Core.Maybe Types.ConnectionIdentifier)
acarrsConnectionIdentifier = Lens.field @"connectionIdentifier"
{-# INLINEABLE acarrsConnectionIdentifier #-}
{-# DEPRECATED connectionIdentifier "Use generic-lens or generic-optics with 'connectionIdentifier' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acarrsResponseStatus :: Lens.Lens' AssociateConnectionAliasResponse Core.Int
acarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE acarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
