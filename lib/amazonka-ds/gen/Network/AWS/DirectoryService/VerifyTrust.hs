{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.VerifyTrust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service for Microsoft Active Directory allows you to configure and verify trust relationships.
--
-- This action verifies a trust relationship between your AWS Managed Microsoft AD directory and an external domain.
module Network.AWS.DirectoryService.VerifyTrust
    (
    -- * Creating a request
      VerifyTrust (..)
    , mkVerifyTrust
    -- ** Request lenses
    , vtTrustId

    -- * Destructuring the response
    , VerifyTrustResponse (..)
    , mkVerifyTrustResponse
    -- ** Response lenses
    , vtrrsTrustId
    , vtrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the verification of an existing trust relationship between an AWS Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'mkVerifyTrust' smart constructor.
newtype VerifyTrust = VerifyTrust'
  { trustId :: Types.TrustId
    -- ^ The unique Trust ID of the trust relationship to verify.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyTrust' value with any optional fields omitted.
mkVerifyTrust
    :: Types.TrustId -- ^ 'trustId'
    -> VerifyTrust
mkVerifyTrust trustId = VerifyTrust'{trustId}

-- | The unique Trust ID of the trust relationship to verify.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTrustId :: Lens.Lens' VerifyTrust Types.TrustId
vtTrustId = Lens.field @"trustId"
{-# INLINEABLE vtTrustId #-}
{-# DEPRECATED trustId "Use generic-lens or generic-optics with 'trustId' instead"  #-}

instance Core.ToQuery VerifyTrust where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders VerifyTrust where
        toHeaders VerifyTrust{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.VerifyTrust")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON VerifyTrust where
        toJSON VerifyTrust{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TrustId" Core..= trustId)])

instance Core.AWSRequest VerifyTrust where
        type Rs VerifyTrust = VerifyTrustResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 VerifyTrustResponse' Core.<$>
                   (x Core..:? "TrustId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Result of a VerifyTrust request.
--
-- /See:/ 'mkVerifyTrustResponse' smart constructor.
data VerifyTrustResponse = VerifyTrustResponse'
  { trustId :: Core.Maybe Types.TrustId
    -- ^ The unique Trust ID of the trust relationship that was verified.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyTrustResponse' value with any optional fields omitted.
mkVerifyTrustResponse
    :: Core.Int -- ^ 'responseStatus'
    -> VerifyTrustResponse
mkVerifyTrustResponse responseStatus
  = VerifyTrustResponse'{trustId = Core.Nothing, responseStatus}

-- | The unique Trust ID of the trust relationship that was verified.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsTrustId :: Lens.Lens' VerifyTrustResponse (Core.Maybe Types.TrustId)
vtrrsTrustId = Lens.field @"trustId"
{-# INLINEABLE vtrrsTrustId #-}
{-# DEPRECATED trustId "Use generic-lens or generic-optics with 'trustId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsResponseStatus :: Lens.Lens' VerifyTrustResponse Core.Int
vtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE vtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
