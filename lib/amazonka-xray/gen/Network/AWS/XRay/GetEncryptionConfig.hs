{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current encryption configuration for X-Ray data.
module Network.AWS.XRay.GetEncryptionConfig
    (
    -- * Creating a request
      GetEncryptionConfig (..)
    , mkGetEncryptionConfig

    -- * Destructuring the response
    , GetEncryptionConfigResponse (..)
    , mkGetEncryptionConfigResponse
    -- ** Response lenses
    , gecrrsEncryptionConfig
    , gecrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetEncryptionConfig' smart constructor.
data GetEncryptionConfig = GetEncryptionConfig'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEncryptionConfig' value with any optional fields omitted.
mkGetEncryptionConfig
    :: GetEncryptionConfig
mkGetEncryptionConfig = GetEncryptionConfig'

instance Core.ToQuery GetEncryptionConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetEncryptionConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetEncryptionConfig where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetEncryptionConfig where
        type Rs GetEncryptionConfig = GetEncryptionConfigResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/EncryptionConfig",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetEncryptionConfigResponse' Core.<$>
                   (x Core..:? "EncryptionConfig") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetEncryptionConfigResponse' smart constructor.
data GetEncryptionConfigResponse = GetEncryptionConfigResponse'
  { encryptionConfig :: Core.Maybe Types.EncryptionConfig
    -- ^ The encryption configuration document.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEncryptionConfigResponse' value with any optional fields omitted.
mkGetEncryptionConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetEncryptionConfigResponse
mkGetEncryptionConfigResponse responseStatus
  = GetEncryptionConfigResponse'{encryptionConfig = Core.Nothing,
                                 responseStatus}

-- | The encryption configuration document.
--
-- /Note:/ Consider using 'encryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecrrsEncryptionConfig :: Lens.Lens' GetEncryptionConfigResponse (Core.Maybe Types.EncryptionConfig)
gecrrsEncryptionConfig = Lens.field @"encryptionConfig"
{-# INLINEABLE gecrrsEncryptionConfig #-}
{-# DEPRECATED encryptionConfig "Use generic-lens or generic-optics with 'encryptionConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecrrsResponseStatus :: Lens.Lens' GetEncryptionConfigResponse Core.Int
gecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
