{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetConfig (..),
    mkGetConfig,

    -- ** Request lenses
    gcClientArn,
    gcClientVersion,
    gcHapgList,

    -- * Destructuring the response
    GetConfigResponse (..),
    mkGetConfigResponse,

    -- ** Response lenses
    gcrrsConfigCred,
    gcrrsConfigFile,
    gcrrsConfigType,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConfig' smart constructor.
data GetConfig = GetConfig'
  { -- | The ARN of the client.
    clientArn :: Types.ClientArn,
    -- | The client version.
    clientVersion :: Types.ClientVersion,
    -- | A list of ARNs that identify the high-availability partition groups that are associated with the client.
    hapgList :: [Types.HapgArn]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConfig' value with any optional fields omitted.
mkGetConfig ::
  -- | 'clientArn'
  Types.ClientArn ->
  -- | 'clientVersion'
  Types.ClientVersion ->
  GetConfig
mkGetConfig clientArn clientVersion =
  GetConfig' {clientArn, clientVersion, hapgList = Core.mempty}

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClientArn :: Lens.Lens' GetConfig Types.ClientArn
gcClientArn = Lens.field @"clientArn"
{-# DEPRECATED gcClientArn "Use generic-lens or generic-optics with 'clientArn' instead." #-}

-- | The client version.
--
-- /Note:/ Consider using 'clientVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClientVersion :: Lens.Lens' GetConfig Types.ClientVersion
gcClientVersion = Lens.field @"clientVersion"
{-# DEPRECATED gcClientVersion "Use generic-lens or generic-optics with 'clientVersion' instead." #-}

-- | A list of ARNs that identify the high-availability partition groups that are associated with the client.
--
-- /Note:/ Consider using 'hapgList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcHapgList :: Lens.Lens' GetConfig [Types.HapgArn]
gcHapgList = Lens.field @"hapgList"
{-# DEPRECATED gcHapgList "Use generic-lens or generic-optics with 'hapgList' instead." #-}

instance Core.FromJSON GetConfig where
  toJSON GetConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientArn" Core..= clientArn),
            Core.Just ("ClientVersion" Core..= clientVersion),
            Core.Just ("HapgList" Core..= hapgList)
          ]
      )

instance Core.AWSRequest GetConfig where
  type Rs GetConfig = GetConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CloudHsmFrontendService.GetConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfigResponse'
            Core.<$> (x Core..:? "ConfigCred")
            Core.<*> (x Core..:? "ConfigFile")
            Core.<*> (x Core..:? "ConfigType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetConfigResponse' smart constructor.
data GetConfigResponse = GetConfigResponse'
  { -- | The certificate file containing the server.pem files of the HSMs.
    configCred :: Core.Maybe Types.String,
    -- | The chrystoki.conf configuration file.
    configFile :: Core.Maybe Types.String,
    -- | The type of credentials.
    configType :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConfigResponse' value with any optional fields omitted.
mkGetConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetConfigResponse
mkGetConfigResponse responseStatus =
  GetConfigResponse'
    { configCred = Core.Nothing,
      configFile = Core.Nothing,
      configType = Core.Nothing,
      responseStatus
    }

-- | The certificate file containing the server.pem files of the HSMs.
--
-- /Note:/ Consider using 'configCred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsConfigCred :: Lens.Lens' GetConfigResponse (Core.Maybe Types.String)
gcrrsConfigCred = Lens.field @"configCred"
{-# DEPRECATED gcrrsConfigCred "Use generic-lens or generic-optics with 'configCred' instead." #-}

-- | The chrystoki.conf configuration file.
--
-- /Note:/ Consider using 'configFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsConfigFile :: Lens.Lens' GetConfigResponse (Core.Maybe Types.String)
gcrrsConfigFile = Lens.field @"configFile"
{-# DEPRECATED gcrrsConfigFile "Use generic-lens or generic-optics with 'configFile' instead." #-}

-- | The type of credentials.
--
-- /Note:/ Consider using 'configType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsConfigType :: Lens.Lens' GetConfigResponse (Core.Maybe Types.String)
gcrrsConfigType = Lens.field @"configType"
{-# DEPRECATED gcrrsConfigType "Use generic-lens or generic-optics with 'configType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetConfigResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
