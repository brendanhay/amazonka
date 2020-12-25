{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetConnectivityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the connectivity information for a core.
module Network.AWS.Greengrass.GetConnectivityInfo
  ( -- * Creating a request
    GetConnectivityInfo (..),
    mkGetConnectivityInfo,

    -- ** Request lenses
    gciThingName,

    -- * Destructuring the response
    GetConnectivityInfoResponse (..),
    mkGetConnectivityInfoResponse,

    -- ** Response lenses
    gcirrsConnectivityInfo,
    gcirrsMessage,
    gcirrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConnectivityInfo' smart constructor.
newtype GetConnectivityInfo = GetConnectivityInfo'
  { -- | The thing name.
    thingName :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectivityInfo' value with any optional fields omitted.
mkGetConnectivityInfo ::
  -- | 'thingName'
  Core.Text ->
  GetConnectivityInfo
mkGetConnectivityInfo thingName = GetConnectivityInfo' {thingName}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciThingName :: Lens.Lens' GetConnectivityInfo Core.Text
gciThingName = Lens.field @"thingName"
{-# DEPRECATED gciThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Core.AWSRequest GetConnectivityInfo where
  type Rs GetConnectivityInfo = GetConnectivityInfoResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/things/" Core.<> (Core.toText thingName)
                Core.<> ("/connectivityInfo")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectivityInfoResponse'
            Core.<$> (x Core..:? "ConnectivityInfo")
            Core.<*> (x Core..:? "message")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetConnectivityInfoResponse' smart constructor.
data GetConnectivityInfoResponse = GetConnectivityInfoResponse'
  { -- | Connectivity info list.
    connectivityInfo :: Core.Maybe [Types.ConnectivityInfo],
    -- | A message about the connectivity info request.
    message :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectivityInfoResponse' value with any optional fields omitted.
mkGetConnectivityInfoResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetConnectivityInfoResponse
mkGetConnectivityInfoResponse responseStatus =
  GetConnectivityInfoResponse'
    { connectivityInfo = Core.Nothing,
      message = Core.Nothing,
      responseStatus
    }

-- | Connectivity info list.
--
-- /Note:/ Consider using 'connectivityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsConnectivityInfo :: Lens.Lens' GetConnectivityInfoResponse (Core.Maybe [Types.ConnectivityInfo])
gcirrsConnectivityInfo = Lens.field @"connectivityInfo"
{-# DEPRECATED gcirrsConnectivityInfo "Use generic-lens or generic-optics with 'connectivityInfo' instead." #-}

-- | A message about the connectivity info request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsMessage :: Lens.Lens' GetConnectivityInfoResponse (Core.Maybe Core.Text)
gcirrsMessage = Lens.field @"message"
{-# DEPRECATED gcirrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsResponseStatus :: Lens.Lens' GetConnectivityInfoResponse Core.Int
gcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
