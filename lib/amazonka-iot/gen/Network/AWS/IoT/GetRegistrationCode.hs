{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetRegistrationCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a registration code used to register a CA certificate with AWS IoT.
module Network.AWS.IoT.GetRegistrationCode
    (
    -- * Creating a request
      GetRegistrationCode (..)
    , mkGetRegistrationCode

    -- * Destructuring the response
    , GetRegistrationCodeResponse (..)
    , mkGetRegistrationCodeResponse
    -- ** Response lenses
    , grcrrsRegistrationCode
    , grcrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the GetRegistrationCode operation.
--
-- /See:/ 'mkGetRegistrationCode' smart constructor.
data GetRegistrationCode = GetRegistrationCode'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegistrationCode' value with any optional fields omitted.
mkGetRegistrationCode
    :: GetRegistrationCode
mkGetRegistrationCode = GetRegistrationCode'

instance Core.ToQuery GetRegistrationCode where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRegistrationCode where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetRegistrationCode where
        type Rs GetRegistrationCode = GetRegistrationCodeResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/registrationcode",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRegistrationCodeResponse' Core.<$>
                   (x Core..:? "registrationCode") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the GetRegistrationCode operation.
--
-- /See:/ 'mkGetRegistrationCodeResponse' smart constructor.
data GetRegistrationCodeResponse = GetRegistrationCodeResponse'
  { registrationCode :: Core.Maybe Types.RegistrationCode
    -- ^ The CA certificate registration code.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegistrationCodeResponse' value with any optional fields omitted.
mkGetRegistrationCodeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRegistrationCodeResponse
mkGetRegistrationCodeResponse responseStatus
  = GetRegistrationCodeResponse'{registrationCode = Core.Nothing,
                                 responseStatus}

-- | The CA certificate registration code.
--
-- /Note:/ Consider using 'registrationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrrsRegistrationCode :: Lens.Lens' GetRegistrationCodeResponse (Core.Maybe Types.RegistrationCode)
grcrrsRegistrationCode = Lens.field @"registrationCode"
{-# INLINEABLE grcrrsRegistrationCode #-}
{-# DEPRECATED registrationCode "Use generic-lens or generic-optics with 'registrationCode' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrrsResponseStatus :: Lens.Lens' GetRegistrationCodeResponse Core.Int
grcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
