{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real-time log configuration.
--
-- You cannot delete a real-time log configuration if it’s attached to a cache behavior. First update your distributions to remove the real-time log configuration from all cache behaviors, then delete the real-time log configuration.
-- To delete a real-time log configuration, you can provide the configuration’s name or its Amazon Resource Name (ARN). You must provide at least one. If you provide both, CloudFront uses the name to identify the real-time log configuration to delete.
module Network.AWS.CloudFront.DeleteRealtimeLogConfig
  ( -- * Creating a request
    DeleteRealtimeLogConfig (..),
    mkDeleteRealtimeLogConfig,

    -- ** Request lenses
    drlcARN,
    drlcName,

    -- * Destructuring the response
    DeleteRealtimeLogConfigResponse (..),
    mkDeleteRealtimeLogConfigResponse,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRealtimeLogConfig' smart constructor.
data DeleteRealtimeLogConfig = DeleteRealtimeLogConfig'
  { -- | The Amazon Resource Name (ARN) of the real-time log configuration to delete.
    arn :: Core.Maybe Types.String,
    -- | The name of the real-time log configuration to delete.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRealtimeLogConfig' value with any optional fields omitted.
mkDeleteRealtimeLogConfig ::
  DeleteRealtimeLogConfig
mkDeleteRealtimeLogConfig =
  DeleteRealtimeLogConfig' {arn = Core.Nothing, name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the real-time log configuration to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drlcARN :: Lens.Lens' DeleteRealtimeLogConfig (Core.Maybe Types.String)
drlcARN = Lens.field @"arn"
{-# DEPRECATED drlcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the real-time log configuration to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drlcName :: Lens.Lens' DeleteRealtimeLogConfig (Core.Maybe Types.String)
drlcName = Lens.field @"name"
{-# DEPRECATED drlcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.ToXML DeleteRealtimeLogConfig where
  toXML DeleteRealtimeLogConfig {..} =
    Core.toXMLNode "ARN" Core.<$> arn
      Core.<> Core.toXMLNode "Name" Core.<$> name
  toXMLDocument =
    Core.mkXMLElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DeleteRealtimeLogConfigRequest"

instance Core.AWSRequest DeleteRealtimeLogConfig where
  type Rs DeleteRealtimeLogConfig = DeleteRealtimeLogConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/2020-05-31/delete-realtime-log-config/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response = Response.receiveNull DeleteRealtimeLogConfigResponse'

-- | /See:/ 'mkDeleteRealtimeLogConfigResponse' smart constructor.
data DeleteRealtimeLogConfigResponse = DeleteRealtimeLogConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRealtimeLogConfigResponse' value with any optional fields omitted.
mkDeleteRealtimeLogConfigResponse ::
  DeleteRealtimeLogConfigResponse
mkDeleteRealtimeLogConfigResponse =
  DeleteRealtimeLogConfigResponse'
