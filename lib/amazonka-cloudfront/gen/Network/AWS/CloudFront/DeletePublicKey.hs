{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeletePublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a public key you previously added to CloudFront.
module Network.AWS.CloudFront.DeletePublicKey
    (
    -- * Creating a request
      DeletePublicKey (..)
    , mkDeletePublicKey
    -- ** Request lenses
    , dpkId
    , dpkIfMatch

    -- * Destructuring the response
    , DeletePublicKeyResponse (..)
    , mkDeletePublicKeyResponse
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePublicKey' smart constructor.
data DeletePublicKey = DeletePublicKey'
  { id :: Core.Text
    -- ^ The ID of the public key you want to remove from CloudFront.
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The value of the @ETag@ header that you received when retrieving the public key identity to delete. For example: @E2QWRUHAPOMQZL@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePublicKey' value with any optional fields omitted.
mkDeletePublicKey
    :: Core.Text -- ^ 'id'
    -> DeletePublicKey
mkDeletePublicKey id = DeletePublicKey'{id, ifMatch = Core.Nothing}

-- | The ID of the public key you want to remove from CloudFront.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpkId :: Lens.Lens' DeletePublicKey Core.Text
dpkId = Lens.field @"id"
{-# INLINEABLE dpkId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The value of the @ETag@ header that you received when retrieving the public key identity to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpkIfMatch :: Lens.Lens' DeletePublicKey (Core.Maybe Core.Text)
dpkIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE dpkIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery DeletePublicKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePublicKey where
        toHeaders DeletePublicKey{..} = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest DeletePublicKey where
        type Rs DeletePublicKey = DeletePublicKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/2020-05-31/public-key/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeletePublicKeyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePublicKeyResponse' smart constructor.
data DeletePublicKeyResponse = DeletePublicKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePublicKeyResponse' value with any optional fields omitted.
mkDeletePublicKeyResponse
    :: DeletePublicKeyResponse
mkDeletePublicKeyResponse = DeletePublicKeyResponse'
