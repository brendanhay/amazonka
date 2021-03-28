{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified identity (an email address or a domain) from the list of verified identities.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteIdentity
    (
    -- * Creating a request
      DeleteIdentity (..)
    , mkDeleteIdentity
    -- ** Request lenses
    , diIdentity

    -- * Destructuring the response
    , DeleteIdentityResponse (..)
    , mkDeleteIdentityResponse
    -- ** Response lenses
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete one of your Amazon SES identities (an email address or domain).
--
-- /See:/ 'mkDeleteIdentity' smart constructor.
newtype DeleteIdentity = DeleteIdentity'
  { identity :: Types.Identity
    -- ^ The identity to be removed from the list of identities for the AWS Account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentity' value with any optional fields omitted.
mkDeleteIdentity
    :: Types.Identity -- ^ 'identity'
    -> DeleteIdentity
mkDeleteIdentity identity = DeleteIdentity'{identity}

-- | The identity to be removed from the list of identities for the AWS Account.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIdentity :: Lens.Lens' DeleteIdentity Types.Identity
diIdentity = Lens.field @"identity"
{-# INLINEABLE diIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

instance Core.ToQuery DeleteIdentity where
        toQuery DeleteIdentity{..}
          = Core.toQueryPair "Action" ("DeleteIdentity" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Identity" identity

instance Core.ToHeaders DeleteIdentity where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteIdentity where
        type Rs DeleteIdentity = DeleteIdentityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DeleteIdentityResult"
              (\ s h x ->
                 DeleteIdentityResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteIdentityResponse' smart constructor.
newtype DeleteIdentityResponse = DeleteIdentityResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityResponse' value with any optional fields omitted.
mkDeleteIdentityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteIdentityResponse
mkDeleteIdentityResponse responseStatus
  = DeleteIdentityResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteIdentityResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
