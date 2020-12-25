{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerAPIMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon Lightsail containers, such as the current version of the Lightsail Control (lightsailctl) plugin.
module Network.AWS.Lightsail.GetContainerAPIMetadata
  ( -- * Creating a request
    GetContainerAPIMetadata (..),
    mkGetContainerAPIMetadata,

    -- * Destructuring the response
    GetContainerAPIMetadataResponse (..),
    mkGetContainerAPIMetadataResponse,

    -- ** Response lenses
    gcapimrrsMetadata,
    gcapimrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerAPIMetadata' smart constructor.
data GetContainerAPIMetadata = GetContainerAPIMetadata'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerAPIMetadata' value with any optional fields omitted.
mkGetContainerAPIMetadata ::
  GetContainerAPIMetadata
mkGetContainerAPIMetadata = GetContainerAPIMetadata'

instance Core.FromJSON GetContainerAPIMetadata where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetContainerAPIMetadata where
  type Rs GetContainerAPIMetadata = GetContainerAPIMetadataResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetContainerAPIMetadata")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerAPIMetadataResponse'
            Core.<$> (x Core..:? "metadata") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetContainerAPIMetadataResponse' smart constructor.
data GetContainerAPIMetadataResponse = GetContainerAPIMetadataResponse'
  { -- | Metadata about Lightsail containers, such as the current version of the Lightsail Control (lightsailctl) plugin.
    metadata :: Core.Maybe [Core.HashMap Types.String Types.String],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerAPIMetadataResponse' value with any optional fields omitted.
mkGetContainerAPIMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetContainerAPIMetadataResponse
mkGetContainerAPIMetadataResponse responseStatus =
  GetContainerAPIMetadataResponse'
    { metadata = Core.Nothing,
      responseStatus
    }

-- | Metadata about Lightsail containers, such as the current version of the Lightsail Control (lightsailctl) plugin.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcapimrrsMetadata :: Lens.Lens' GetContainerAPIMetadataResponse (Core.Maybe [Core.HashMap Types.String Types.String])
gcapimrrsMetadata = Lens.field @"metadata"
{-# DEPRECATED gcapimrrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcapimrrsResponseStatus :: Lens.Lens' GetContainerAPIMetadataResponse Core.Int
gcapimrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcapimrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
