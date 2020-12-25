{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific key pair.
module Network.AWS.Lightsail.GetKeyPair
  ( -- * Creating a request
    GetKeyPair (..),
    mkGetKeyPair,

    -- ** Request lenses
    gkpKeyPairName,

    -- * Destructuring the response
    GetKeyPairResponse (..),
    mkGetKeyPairResponse,

    -- ** Response lenses
    gkprfrsKeyPair,
    gkprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetKeyPair' smart constructor.
newtype GetKeyPair = GetKeyPair'
  { -- | The name of the key pair for which you are requesting information.
    keyPairName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyPair' value with any optional fields omitted.
mkGetKeyPair ::
  -- | 'keyPairName'
  Types.ResourceName ->
  GetKeyPair
mkGetKeyPair keyPairName = GetKeyPair' {keyPairName}

-- | The name of the key pair for which you are requesting information.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpKeyPairName :: Lens.Lens' GetKeyPair Types.ResourceName
gkpKeyPairName = Lens.field @"keyPairName"
{-# DEPRECATED gkpKeyPairName "Use generic-lens or generic-optics with 'keyPairName' instead." #-}

instance Core.FromJSON GetKeyPair where
  toJSON GetKeyPair {..} =
    Core.object
      (Core.catMaybes [Core.Just ("keyPairName" Core..= keyPairName)])

instance Core.AWSRequest GetKeyPair where
  type Rs GetKeyPair = GetKeyPairResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetKeyPair")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyPairResponse'
            Core.<$> (x Core..:? "keyPair") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetKeyPairResponse' smart constructor.
data GetKeyPairResponse = GetKeyPairResponse'
  { -- | An array of key-value pairs containing information about the key pair.
    keyPair :: Core.Maybe Types.KeyPair,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetKeyPairResponse' value with any optional fields omitted.
mkGetKeyPairResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetKeyPairResponse
mkGetKeyPairResponse responseStatus =
  GetKeyPairResponse' {keyPair = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about the key pair.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprfrsKeyPair :: Lens.Lens' GetKeyPairResponse (Core.Maybe Types.KeyPair)
gkprfrsKeyPair = Lens.field @"keyPair"
{-# DEPRECATED gkprfrsKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprfrsResponseStatus :: Lens.Lens' GetKeyPairResponse Core.Int
gkprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gkprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
