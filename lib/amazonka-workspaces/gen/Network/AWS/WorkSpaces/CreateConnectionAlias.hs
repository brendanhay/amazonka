{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CreateConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified connection alias for use with cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.CreateConnectionAlias
    (
    -- * Creating a request
      CreateConnectionAlias (..)
    , mkCreateConnectionAlias
    -- ** Request lenses
    , ccaConnectionString
    , ccaTags

    -- * Destructuring the response
    , CreateConnectionAliasResponse (..)
    , mkCreateConnectionAliasResponse
    -- ** Response lenses
    , ccarrsAliasId
    , ccarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkCreateConnectionAlias' smart constructor.
data CreateConnectionAlias = CreateConnectionAlias'
  { connectionString :: Types.ConnectionString
    -- ^ A connection string in the form of a fully qualified domain name (FQDN), such as @www.example.com@ .
--
-- /Important:/ After you create a connection string, it is always associated to your AWS account. You cannot recreate the same connection string with a different account, even if you delete all instances of it from the original account. The connection string is globally reserved for your account.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to associate with the connection alias.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConnectionAlias' value with any optional fields omitted.
mkCreateConnectionAlias
    :: Types.ConnectionString -- ^ 'connectionString'
    -> CreateConnectionAlias
mkCreateConnectionAlias connectionString
  = CreateConnectionAlias'{connectionString, tags = Core.Nothing}

-- | A connection string in the form of a fully qualified domain name (FQDN), such as @www.example.com@ .
--
-- /Important:/ After you create a connection string, it is always associated to your AWS account. You cannot recreate the same connection string with a different account, even if you delete all instances of it from the original account. The connection string is globally reserved for your account.
--
-- /Note:/ Consider using 'connectionString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaConnectionString :: Lens.Lens' CreateConnectionAlias Types.ConnectionString
ccaConnectionString = Lens.field @"connectionString"
{-# INLINEABLE ccaConnectionString #-}
{-# DEPRECATED connectionString "Use generic-lens or generic-optics with 'connectionString' instead"  #-}

-- | The tags to associate with the connection alias.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaTags :: Lens.Lens' CreateConnectionAlias (Core.Maybe [Types.Tag])
ccaTags = Lens.field @"tags"
{-# INLINEABLE ccaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateConnectionAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateConnectionAlias where
        toHeaders CreateConnectionAlias{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.CreateConnectionAlias")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateConnectionAlias where
        toJSON CreateConnectionAlias{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConnectionString" Core..= connectionString),
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateConnectionAlias where
        type Rs CreateConnectionAlias = CreateConnectionAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateConnectionAliasResponse' Core.<$>
                   (x Core..:? "AliasId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateConnectionAliasResponse' smart constructor.
data CreateConnectionAliasResponse = CreateConnectionAliasResponse'
  { aliasId :: Core.Maybe Types.AliasId
    -- ^ The identifier of the connection alias.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConnectionAliasResponse' value with any optional fields omitted.
mkCreateConnectionAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateConnectionAliasResponse
mkCreateConnectionAliasResponse responseStatus
  = CreateConnectionAliasResponse'{aliasId = Core.Nothing,
                                   responseStatus}

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccarrsAliasId :: Lens.Lens' CreateConnectionAliasResponse (Core.Maybe Types.AliasId)
ccarrsAliasId = Lens.field @"aliasId"
{-# INLINEABLE ccarrsAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccarrsResponseStatus :: Lens.Lens' CreateConnectionAliasResponse Core.Int
ccarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
