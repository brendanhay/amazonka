{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteDocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.DeleteDocumentationVersion
    (
    -- * Creating a request
      DeleteDocumentationVersion (..)
    , mkDeleteDocumentationVersion
    -- ** Request lenses
    , ddvRestApiId
    , ddvDocumentationVersion

    -- * Destructuring the response
    , DeleteDocumentationVersionResponse (..)
    , mkDeleteDocumentationVersionResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes an existing documentation version of an API.
--
-- /See:/ 'mkDeleteDocumentationVersion' smart constructor.
data DeleteDocumentationVersion = DeleteDocumentationVersion'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , documentationVersion :: Core.Text
    -- ^ [Required] The version identifier of a to-be-deleted documentation snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocumentationVersion' value with any optional fields omitted.
mkDeleteDocumentationVersion
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'documentationVersion'
    -> DeleteDocumentationVersion
mkDeleteDocumentationVersion restApiId documentationVersion
  = DeleteDocumentationVersion'{restApiId, documentationVersion}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvRestApiId :: Lens.Lens' DeleteDocumentationVersion Core.Text
ddvRestApiId = Lens.field @"restApiId"
{-# INLINEABLE ddvRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The version identifier of a to-be-deleted documentation snapshot.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvDocumentationVersion :: Lens.Lens' DeleteDocumentationVersion Core.Text
ddvDocumentationVersion = Lens.field @"documentationVersion"
{-# INLINEABLE ddvDocumentationVersion #-}
{-# DEPRECATED documentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead"  #-}

instance Core.ToQuery DeleteDocumentationVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDocumentationVersion where
        toHeaders DeleteDocumentationVersion{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteDocumentationVersion where
        type Rs DeleteDocumentationVersion =
             DeleteDocumentationVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/versions/"
                             Core.<> Core.toText documentationVersion,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteDocumentationVersionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDocumentationVersionResponse' smart constructor.
data DeleteDocumentationVersionResponse = DeleteDocumentationVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocumentationVersionResponse' value with any optional fields omitted.
mkDeleteDocumentationVersionResponse
    :: DeleteDocumentationVersionResponse
mkDeleteDocumentationVersionResponse
  = DeleteDocumentationVersionResponse'
