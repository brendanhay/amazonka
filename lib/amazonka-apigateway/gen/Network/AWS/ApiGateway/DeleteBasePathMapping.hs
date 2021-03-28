{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteBasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'BasePathMapping' resource.
module Network.AWS.ApiGateway.DeleteBasePathMapping
    (
    -- * Creating a request
      DeleteBasePathMapping (..)
    , mkDeleteBasePathMapping
    -- ** Request lenses
    , dbpmDomainName
    , dbpmBasePath

    -- * Destructuring the response
    , DeleteBasePathMappingResponse (..)
    , mkDeleteBasePathMappingResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete the 'BasePathMapping' resource.
--
-- /See:/ 'mkDeleteBasePathMapping' smart constructor.
data DeleteBasePathMapping = DeleteBasePathMapping'
  { domainName :: Core.Text
    -- ^ [Required] The domain name of the 'BasePathMapping' resource to delete.
  , basePath :: Core.Text
    -- ^ [Required] The base path name of the 'BasePathMapping' resource to delete.
--
-- To specify an empty base path, set this parameter to @'(none)'@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBasePathMapping' value with any optional fields omitted.
mkDeleteBasePathMapping
    :: Core.Text -- ^ 'domainName'
    -> Core.Text -- ^ 'basePath'
    -> DeleteBasePathMapping
mkDeleteBasePathMapping domainName basePath
  = DeleteBasePathMapping'{domainName, basePath}

-- | [Required] The domain name of the 'BasePathMapping' resource to delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpmDomainName :: Lens.Lens' DeleteBasePathMapping Core.Text
dbpmDomainName = Lens.field @"domainName"
{-# INLINEABLE dbpmDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | [Required] The base path name of the 'BasePathMapping' resource to delete.
--
-- To specify an empty base path, set this parameter to @'(none)'@ .
--
-- /Note:/ Consider using 'basePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpmBasePath :: Lens.Lens' DeleteBasePathMapping Core.Text
dbpmBasePath = Lens.field @"basePath"
{-# INLINEABLE dbpmBasePath #-}
{-# DEPRECATED basePath "Use generic-lens or generic-optics with 'basePath' instead"  #-}

instance Core.ToQuery DeleteBasePathMapping where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBasePathMapping where
        toHeaders DeleteBasePathMapping{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteBasePathMapping where
        type Rs DeleteBasePathMapping = DeleteBasePathMappingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/domainnames/" Core.<> Core.toText domainName Core.<>
                             "/basepathmappings/"
                             Core.<> Core.toText basePath,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteBasePathMappingResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBasePathMappingResponse' smart constructor.
data DeleteBasePathMappingResponse = DeleteBasePathMappingResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBasePathMappingResponse' value with any optional fields omitted.
mkDeleteBasePathMappingResponse
    :: DeleteBasePathMappingResponse
mkDeleteBasePathMappingResponse = DeleteBasePathMappingResponse'
