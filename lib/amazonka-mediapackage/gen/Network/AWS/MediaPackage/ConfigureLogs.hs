{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.ConfigureLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Channel's properities to configure log subscription
module Network.AWS.MediaPackage.ConfigureLogs
  ( -- * Creating a request
    ConfigureLogs (..),
    mkConfigureLogs,

    -- ** Request lenses
    clId,
    clEgressAccessLogs,
    clIngressAccessLogs,

    -- * Destructuring the response
    ConfigureLogsResponse (..),
    mkConfigureLogsResponse,

    -- ** Response lenses
    clrrsArn,
    clrrsDescription,
    clrrsEgressAccessLogs,
    clrrsHlsIngest,
    clrrsId,
    clrrsIngressAccessLogs,
    clrrsTags,
    clrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | the option to configure log subscription.
--
-- /See:/ 'mkConfigureLogs' smart constructor.
data ConfigureLogs = ConfigureLogs'
  { -- | The ID of the channel to log subscription.
    id :: Core.Text,
    egressAccessLogs :: Core.Maybe Types.EgressAccessLogs,
    ingressAccessLogs :: Core.Maybe Types.IngressAccessLogs
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigureLogs' value with any optional fields omitted.
mkConfigureLogs ::
  -- | 'id'
  Core.Text ->
  ConfigureLogs
mkConfigureLogs id =
  ConfigureLogs'
    { id,
      egressAccessLogs = Core.Nothing,
      ingressAccessLogs = Core.Nothing
    }

-- | The ID of the channel to log subscription.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clId :: Lens.Lens' ConfigureLogs Core.Text
clId = Lens.field @"id"
{-# DEPRECATED clId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clEgressAccessLogs :: Lens.Lens' ConfigureLogs (Core.Maybe Types.EgressAccessLogs)
clEgressAccessLogs = Lens.field @"egressAccessLogs"
{-# DEPRECATED clEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clIngressAccessLogs :: Lens.Lens' ConfigureLogs (Core.Maybe Types.IngressAccessLogs)
clIngressAccessLogs = Lens.field @"ingressAccessLogs"
{-# DEPRECATED clIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

instance Core.FromJSON ConfigureLogs where
  toJSON ConfigureLogs {..} =
    Core.object
      ( Core.catMaybes
          [ ("egressAccessLogs" Core..=) Core.<$> egressAccessLogs,
            ("ingressAccessLogs" Core..=) Core.<$> ingressAccessLogs
          ]
      )

instance Core.AWSRequest ConfigureLogs where
  type Rs ConfigureLogs = ConfigureLogsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/channels/" Core.<> (Core.toText id)
                Core.<> ("/configure_logs")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfigureLogsResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "egressAccessLogs")
            Core.<*> (x Core..:? "hlsIngest")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "ingressAccessLogs")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkConfigureLogsResponse' smart constructor.
data ConfigureLogsResponse = ConfigureLogsResponse'
  { -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Core.Maybe Core.Text,
    -- | A short text description of the Channel.
    description :: Core.Maybe Core.Text,
    egressAccessLogs :: Core.Maybe Types.EgressAccessLogs,
    hlsIngest :: Core.Maybe Types.HlsIngest,
    -- | The ID of the Channel.
    id :: Core.Maybe Core.Text,
    ingressAccessLogs :: Core.Maybe Types.IngressAccessLogs,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigureLogsResponse' value with any optional fields omitted.
mkConfigureLogsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ConfigureLogsResponse
mkConfigureLogsResponse responseStatus =
  ConfigureLogsResponse'
    { arn = Core.Nothing,
      description = Core.Nothing,
      egressAccessLogs = Core.Nothing,
      hlsIngest = Core.Nothing,
      id = Core.Nothing,
      ingressAccessLogs = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsArn :: Lens.Lens' ConfigureLogsResponse (Core.Maybe Core.Text)
clrrsArn = Lens.field @"arn"
{-# DEPRECATED clrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsDescription :: Lens.Lens' ConfigureLogsResponse (Core.Maybe Core.Text)
clrrsDescription = Lens.field @"description"
{-# DEPRECATED clrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsEgressAccessLogs :: Lens.Lens' ConfigureLogsResponse (Core.Maybe Types.EgressAccessLogs)
clrrsEgressAccessLogs = Lens.field @"egressAccessLogs"
{-# DEPRECATED clrrsEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsHlsIngest :: Lens.Lens' ConfigureLogsResponse (Core.Maybe Types.HlsIngest)
clrrsHlsIngest = Lens.field @"hlsIngest"
{-# DEPRECATED clrrsHlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead." #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsId :: Lens.Lens' ConfigureLogsResponse (Core.Maybe Core.Text)
clrrsId = Lens.field @"id"
{-# DEPRECATED clrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsIngressAccessLogs :: Lens.Lens' ConfigureLogsResponse (Core.Maybe Types.IngressAccessLogs)
clrrsIngressAccessLogs = Lens.field @"ingressAccessLogs"
{-# DEPRECATED clrrsIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsTags :: Lens.Lens' ConfigureLogsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
clrrsTags = Lens.field @"tags"
{-# DEPRECATED clrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsResponseStatus :: Lens.Lens' ConfigureLogsResponse Core.Int
clrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED clrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
