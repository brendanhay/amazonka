{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.GetConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Gets the configuration files necessary to connect to all high availability partition groups the client is associated with.
module Network.AWS.CloudHSM.GetConfig
    (
    -- * Creating a request
      GetConfig (..)
    , mkGetConfig
    -- ** Request lenses
    , gcClientArn
    , gcClientVersion
    , gcHapgList

    -- * Destructuring the response
    , GetConfigResponse (..)
    , mkGetConfigResponse
    -- ** Response lenses
    , gcrrsConfigCred
    , gcrrsConfigFile
    , gcrrsConfigType
    , gcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConfig' smart constructor.
data GetConfig = GetConfig'
  { clientArn :: Types.ClientArn
    -- ^ The ARN of the client.
  , clientVersion :: Types.ClientVersion
    -- ^ The client version.
  , hapgList :: [Types.HapgArn]
    -- ^ A list of ARNs that identify the high-availability partition groups that are associated with the client.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConfig' value with any optional fields omitted.
mkGetConfig
    :: Types.ClientArn -- ^ 'clientArn'
    -> Types.ClientVersion -- ^ 'clientVersion'
    -> GetConfig
mkGetConfig clientArn clientVersion
  = GetConfig'{clientArn, clientVersion, hapgList = Core.mempty}

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClientArn :: Lens.Lens' GetConfig Types.ClientArn
gcClientArn = Lens.field @"clientArn"
{-# INLINEABLE gcClientArn #-}
{-# DEPRECATED clientArn "Use generic-lens or generic-optics with 'clientArn' instead"  #-}

-- | The client version.
--
-- /Note:/ Consider using 'clientVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClientVersion :: Lens.Lens' GetConfig Types.ClientVersion
gcClientVersion = Lens.field @"clientVersion"
{-# INLINEABLE gcClientVersion #-}
{-# DEPRECATED clientVersion "Use generic-lens or generic-optics with 'clientVersion' instead"  #-}

-- | A list of ARNs that identify the high-availability partition groups that are associated with the client.
--
-- /Note:/ Consider using 'hapgList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcHapgList :: Lens.Lens' GetConfig [Types.HapgArn]
gcHapgList = Lens.field @"hapgList"
{-# INLINEABLE gcHapgList #-}
{-# DEPRECATED hapgList "Use generic-lens or generic-optics with 'hapgList' instead"  #-}

instance Core.ToQuery GetConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetConfig where
        toHeaders GetConfig{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.GetConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetConfig where
        toJSON GetConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClientArn" Core..= clientArn),
                  Core.Just ("ClientVersion" Core..= clientVersion),
                  Core.Just ("HapgList" Core..= hapgList)])

instance Core.AWSRequest GetConfig where
        type Rs GetConfig = GetConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetConfigResponse' Core.<$>
                   (x Core..:? "ConfigCred") Core.<*> x Core..:? "ConfigFile" Core.<*>
                     x Core..:? "ConfigType"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetConfigResponse' smart constructor.
data GetConfigResponse = GetConfigResponse'
  { configCred :: Core.Maybe Core.Text
    -- ^ The certificate file containing the server.pem files of the HSMs.
  , configFile :: Core.Maybe Core.Text
    -- ^ The chrystoki.conf configuration file.
  , configType :: Core.Maybe Core.Text
    -- ^ The type of credentials.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConfigResponse' value with any optional fields omitted.
mkGetConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetConfigResponse
mkGetConfigResponse responseStatus
  = GetConfigResponse'{configCred = Core.Nothing,
                       configFile = Core.Nothing, configType = Core.Nothing,
                       responseStatus}

-- | The certificate file containing the server.pem files of the HSMs.
--
-- /Note:/ Consider using 'configCred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsConfigCred :: Lens.Lens' GetConfigResponse (Core.Maybe Core.Text)
gcrrsConfigCred = Lens.field @"configCred"
{-# INLINEABLE gcrrsConfigCred #-}
{-# DEPRECATED configCred "Use generic-lens or generic-optics with 'configCred' instead"  #-}

-- | The chrystoki.conf configuration file.
--
-- /Note:/ Consider using 'configFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsConfigFile :: Lens.Lens' GetConfigResponse (Core.Maybe Core.Text)
gcrrsConfigFile = Lens.field @"configFile"
{-# INLINEABLE gcrrsConfigFile #-}
{-# DEPRECATED configFile "Use generic-lens or generic-optics with 'configFile' instead"  #-}

-- | The type of credentials.
--
-- /Note:/ Consider using 'configType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsConfigType :: Lens.Lens' GetConfigResponse (Core.Maybe Core.Text)
gcrrsConfigType = Lens.field @"configType"
{-# INLINEABLE gcrrsConfigType #-}
{-# DEPRECATED configType "Use generic-lens or generic-optics with 'configType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetConfigResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
