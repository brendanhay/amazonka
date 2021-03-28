{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.AcceptQualificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AcceptQualificationRequest@ operation approves a Worker's request for a Qualification. 
--
-- Only the owner of the Qualification type can grant a Qualification request for that type. 
-- A successful request for the @AcceptQualificationRequest@ operation returns with no errors and an empty body. 
module Network.AWS.MechanicalTurk.AcceptQualificationRequest
    (
    -- * Creating a request
      AcceptQualificationRequest (..)
    , mkAcceptQualificationRequest
    -- ** Request lenses
    , aqrQualificationRequestId
    , aqrIntegerValue

    -- * Destructuring the response
    , AcceptQualificationRequestResponse (..)
    , mkAcceptQualificationRequestResponse
    -- ** Response lenses
    , aqrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptQualificationRequest' smart constructor.
data AcceptQualificationRequest = AcceptQualificationRequest'
  { qualificationRequestId :: Core.Text
    -- ^ The ID of the Qualification request, as returned by the @GetQualificationRequests@ operation.
  , integerValue :: Core.Maybe Core.Int
    -- ^ The value of the Qualification. You can omit this value if you are using the presence or absence of the Qualification as the basis for a HIT requirement. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptQualificationRequest' value with any optional fields omitted.
mkAcceptQualificationRequest
    :: Core.Text -- ^ 'qualificationRequestId'
    -> AcceptQualificationRequest
mkAcceptQualificationRequest qualificationRequestId
  = AcceptQualificationRequest'{qualificationRequestId,
                                integerValue = Core.Nothing}

-- | The ID of the Qualification request, as returned by the @GetQualificationRequests@ operation.
--
-- /Note:/ Consider using 'qualificationRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqrQualificationRequestId :: Lens.Lens' AcceptQualificationRequest Core.Text
aqrQualificationRequestId = Lens.field @"qualificationRequestId"
{-# INLINEABLE aqrQualificationRequestId #-}
{-# DEPRECATED qualificationRequestId "Use generic-lens or generic-optics with 'qualificationRequestId' instead"  #-}

-- | The value of the Qualification. You can omit this value if you are using the presence or absence of the Qualification as the basis for a HIT requirement. 
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqrIntegerValue :: Lens.Lens' AcceptQualificationRequest (Core.Maybe Core.Int)
aqrIntegerValue = Lens.field @"integerValue"
{-# INLINEABLE aqrIntegerValue #-}
{-# DEPRECATED integerValue "Use generic-lens or generic-optics with 'integerValue' instead"  #-}

instance Core.ToQuery AcceptQualificationRequest where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AcceptQualificationRequest where
        toHeaders AcceptQualificationRequest{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.AcceptQualificationRequest")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AcceptQualificationRequest where
        toJSON AcceptQualificationRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("QualificationRequestId" Core..= qualificationRequestId),
                  ("IntegerValue" Core..=) Core.<$> integerValue])

instance Core.AWSRequest AcceptQualificationRequest where
        type Rs AcceptQualificationRequest =
             AcceptQualificationRequestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AcceptQualificationRequestResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptQualificationRequestResponse' smart constructor.
newtype AcceptQualificationRequestResponse = AcceptQualificationRequestResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptQualificationRequestResponse' value with any optional fields omitted.
mkAcceptQualificationRequestResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptQualificationRequestResponse
mkAcceptQualificationRequestResponse responseStatus
  = AcceptQualificationRequestResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqrrrsResponseStatus :: Lens.Lens' AcceptQualificationRequestResponse Core.Int
aqrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aqrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
