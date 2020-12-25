{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstanceAccessDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns temporary SSH keys you can use to connect to a specific virtual private server, or /instance/ .
--
-- The @get instance access details@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.GetInstanceAccessDetails
  ( -- * Creating a request
    GetInstanceAccessDetails (..),
    mkGetInstanceAccessDetails,

    -- ** Request lenses
    giadInstanceName,
    giadProtocol,

    -- * Destructuring the response
    GetInstanceAccessDetailsResponse (..),
    mkGetInstanceAccessDetailsResponse,

    -- ** Response lenses
    giadrrsAccessDetails,
    giadrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstanceAccessDetails' smart constructor.
data GetInstanceAccessDetails = GetInstanceAccessDetails'
  { -- | The name of the instance to access.
    instanceName :: Types.ResourceName,
    -- | The protocol to use to connect to your instance. Defaults to @ssh@ .
    protocol :: Core.Maybe Types.InstanceAccessProtocol
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceAccessDetails' value with any optional fields omitted.
mkGetInstanceAccessDetails ::
  -- | 'instanceName'
  Types.ResourceName ->
  GetInstanceAccessDetails
mkGetInstanceAccessDetails instanceName =
  GetInstanceAccessDetails' {instanceName, protocol = Core.Nothing}

-- | The name of the instance to access.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giadInstanceName :: Lens.Lens' GetInstanceAccessDetails Types.ResourceName
giadInstanceName = Lens.field @"instanceName"
{-# DEPRECATED giadInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | The protocol to use to connect to your instance. Defaults to @ssh@ .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giadProtocol :: Lens.Lens' GetInstanceAccessDetails (Core.Maybe Types.InstanceAccessProtocol)
giadProtocol = Lens.field @"protocol"
{-# DEPRECATED giadProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

instance Core.FromJSON GetInstanceAccessDetails where
  toJSON GetInstanceAccessDetails {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("instanceName" Core..= instanceName),
            ("protocol" Core..=) Core.<$> protocol
          ]
      )

instance Core.AWSRequest GetInstanceAccessDetails where
  type Rs GetInstanceAccessDetails = GetInstanceAccessDetailsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetInstanceAccessDetails")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceAccessDetailsResponse'
            Core.<$> (x Core..:? "accessDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInstanceAccessDetailsResponse' smart constructor.
data GetInstanceAccessDetailsResponse = GetInstanceAccessDetailsResponse'
  { -- | An array of key-value pairs containing information about a get instance access request.
    accessDetails :: Core.Maybe Types.InstanceAccessDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInstanceAccessDetailsResponse' value with any optional fields omitted.
mkGetInstanceAccessDetailsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInstanceAccessDetailsResponse
mkGetInstanceAccessDetailsResponse responseStatus =
  GetInstanceAccessDetailsResponse'
    { accessDetails = Core.Nothing,
      responseStatus
    }

-- | An array of key-value pairs containing information about a get instance access request.
--
-- /Note:/ Consider using 'accessDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giadrrsAccessDetails :: Lens.Lens' GetInstanceAccessDetailsResponse (Core.Maybe Types.InstanceAccessDetails)
giadrrsAccessDetails = Lens.field @"accessDetails"
{-# DEPRECATED giadrrsAccessDetails "Use generic-lens or generic-optics with 'accessDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giadrrsResponseStatus :: Lens.Lens' GetInstanceAccessDetailsResponse Core.Int
giadrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED giadrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
