{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.InstallToRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Installs an application to the device in a remote access session. For Android applications, the file must be in .apk format. For iOS applications, the file must be in .ipa format.
module Network.AWS.DeviceFarm.InstallToRemoteAccessSession
    (
    -- * Creating a request
      InstallToRemoteAccessSession (..)
    , mkInstallToRemoteAccessSession
    -- ** Request lenses
    , itrasRemoteAccessSessionArn
    , itrasAppArn

    -- * Destructuring the response
    , InstallToRemoteAccessSessionResponse (..)
    , mkInstallToRemoteAccessSessionResponse
    -- ** Response lenses
    , itrasrrsAppUpload
    , itrasrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to install an Android application (in .apk format) or an iOS application (in .ipa format) as part of a remote access session.
--
-- /See:/ 'mkInstallToRemoteAccessSession' smart constructor.
data InstallToRemoteAccessSession = InstallToRemoteAccessSession'
  { remoteAccessSessionArn :: Types.RemoteAccessSessionArn
    -- ^ The Amazon Resource Name (ARN) of the remote access session about which you are requesting information.
  , appArn :: Types.AppArn
    -- ^ The ARN of the app about which you are requesting information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstallToRemoteAccessSession' value with any optional fields omitted.
mkInstallToRemoteAccessSession
    :: Types.RemoteAccessSessionArn -- ^ 'remoteAccessSessionArn'
    -> Types.AppArn -- ^ 'appArn'
    -> InstallToRemoteAccessSession
mkInstallToRemoteAccessSession remoteAccessSessionArn appArn
  = InstallToRemoteAccessSession'{remoteAccessSessionArn, appArn}

-- | The Amazon Resource Name (ARN) of the remote access session about which you are requesting information.
--
-- /Note:/ Consider using 'remoteAccessSessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrasRemoteAccessSessionArn :: Lens.Lens' InstallToRemoteAccessSession Types.RemoteAccessSessionArn
itrasRemoteAccessSessionArn = Lens.field @"remoteAccessSessionArn"
{-# INLINEABLE itrasRemoteAccessSessionArn #-}
{-# DEPRECATED remoteAccessSessionArn "Use generic-lens or generic-optics with 'remoteAccessSessionArn' instead"  #-}

-- | The ARN of the app about which you are requesting information.
--
-- /Note:/ Consider using 'appArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrasAppArn :: Lens.Lens' InstallToRemoteAccessSession Types.AppArn
itrasAppArn = Lens.field @"appArn"
{-# INLINEABLE itrasAppArn #-}
{-# DEPRECATED appArn "Use generic-lens or generic-optics with 'appArn' instead"  #-}

instance Core.ToQuery InstallToRemoteAccessSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders InstallToRemoteAccessSession where
        toHeaders InstallToRemoteAccessSession{..}
          = Core.pure
              ("X-Amz-Target",
               "DeviceFarm_20150623.InstallToRemoteAccessSession")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON InstallToRemoteAccessSession where
        toJSON InstallToRemoteAccessSession{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("remoteAccessSessionArn" Core..= remoteAccessSessionArn),
                  Core.Just ("appArn" Core..= appArn)])

instance Core.AWSRequest InstallToRemoteAccessSession where
        type Rs InstallToRemoteAccessSession =
             InstallToRemoteAccessSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 InstallToRemoteAccessSessionResponse' Core.<$>
                   (x Core..:? "appUpload") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server after AWS Device Farm makes a request to install to a remote access session.
--
-- /See:/ 'mkInstallToRemoteAccessSessionResponse' smart constructor.
data InstallToRemoteAccessSessionResponse = InstallToRemoteAccessSessionResponse'
  { appUpload :: Core.Maybe Types.Upload
    -- ^ An app to upload or that has been uploaded.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstallToRemoteAccessSessionResponse' value with any optional fields omitted.
mkInstallToRemoteAccessSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> InstallToRemoteAccessSessionResponse
mkInstallToRemoteAccessSessionResponse responseStatus
  = InstallToRemoteAccessSessionResponse'{appUpload = Core.Nothing,
                                          responseStatus}

-- | An app to upload or that has been uploaded.
--
-- /Note:/ Consider using 'appUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrasrrsAppUpload :: Lens.Lens' InstallToRemoteAccessSessionResponse (Core.Maybe Types.Upload)
itrasrrsAppUpload = Lens.field @"appUpload"
{-# INLINEABLE itrasrrsAppUpload #-}
{-# DEPRECATED appUpload "Use generic-lens or generic-optics with 'appUpload' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrasrrsResponseStatus :: Lens.Lens' InstallToRemoteAccessSessionResponse Core.Int
itrasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE itrasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
