{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateConnectivityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the connectivity information for the core. Any devices that belong to the group which has this core will receive this information in order to find the location of the core and connect to it.
module Network.AWS.Greengrass.UpdateConnectivityInfo
  ( -- * Creating a request
    UpdateConnectivityInfo (..),
    mkUpdateConnectivityInfo,

    -- ** Request lenses
    uciThingName,
    uciConnectivityInfo,

    -- * Destructuring the response
    UpdateConnectivityInfoResponse (..),
    mkUpdateConnectivityInfoResponse,

    -- ** Response lenses
    ucirrsMessage,
    ucirrsVersion,
    ucirrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Connectivity information.
--
-- /See:/ 'mkUpdateConnectivityInfo' smart constructor.
data UpdateConnectivityInfo = UpdateConnectivityInfo'
  { -- | The thing name.
    thingName :: Core.Text,
    -- | A list of connectivity info.
    connectivityInfo :: Core.Maybe [Types.ConnectivityInfo]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnectivityInfo' value with any optional fields omitted.
mkUpdateConnectivityInfo ::
  -- | 'thingName'
  Core.Text ->
  UpdateConnectivityInfo
mkUpdateConnectivityInfo thingName =
  UpdateConnectivityInfo'
    { thingName,
      connectivityInfo = Core.Nothing
    }

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciThingName :: Lens.Lens' UpdateConnectivityInfo Core.Text
uciThingName = Lens.field @"thingName"
{-# DEPRECATED uciThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | A list of connectivity info.
--
-- /Note:/ Consider using 'connectivityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciConnectivityInfo :: Lens.Lens' UpdateConnectivityInfo (Core.Maybe [Types.ConnectivityInfo])
uciConnectivityInfo = Lens.field @"connectivityInfo"
{-# DEPRECATED uciConnectivityInfo "Use generic-lens or generic-optics with 'connectivityInfo' instead." #-}

instance Core.FromJSON UpdateConnectivityInfo where
  toJSON UpdateConnectivityInfo {..} =
    Core.object
      ( Core.catMaybes
          [("ConnectivityInfo" Core..=) Core.<$> connectivityInfo]
      )

instance Core.AWSRequest UpdateConnectivityInfo where
  type Rs UpdateConnectivityInfo = UpdateConnectivityInfoResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/things/" Core.<> (Core.toText thingName)
                Core.<> ("/connectivityInfo")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConnectivityInfoResponse'
            Core.<$> (x Core..:? "message")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateConnectivityInfoResponse' smart constructor.
data UpdateConnectivityInfoResponse = UpdateConnectivityInfoResponse'
  { -- | A message about the connectivity info update request.
    message :: Core.Maybe Core.Text,
    -- | The new version of the connectivity info.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnectivityInfoResponse' value with any optional fields omitted.
mkUpdateConnectivityInfoResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateConnectivityInfoResponse
mkUpdateConnectivityInfoResponse responseStatus =
  UpdateConnectivityInfoResponse'
    { message = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | A message about the connectivity info update request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirrsMessage :: Lens.Lens' UpdateConnectivityInfoResponse (Core.Maybe Core.Text)
ucirrsMessage = Lens.field @"message"
{-# DEPRECATED ucirrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The new version of the connectivity info.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirrsVersion :: Lens.Lens' UpdateConnectivityInfoResponse (Core.Maybe Core.Text)
ucirrsVersion = Lens.field @"version"
{-# DEPRECATED ucirrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirrsResponseStatus :: Lens.Lens' UpdateConnectivityInfoResponse Core.Int
ucirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
