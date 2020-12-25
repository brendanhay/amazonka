{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeHandshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a previously requested handshake. The handshake ID comes from the response to the original 'InviteAccountToOrganization' operation that generated the handshake.
--
-- You can access handshakes that are @ACCEPTED@ , @DECLINED@ , or @CANCELED@ for only 30 days after they change to that state. They're then deleted and no longer accessible.
-- This operation can be called from any account in the organization.
module Network.AWS.Organizations.DescribeHandshake
  ( -- * Creating a request
    DescribeHandshake (..),
    mkDescribeHandshake,

    -- ** Request lenses
    dhHandshakeId,

    -- * Destructuring the response
    DescribeHandshakeResponse (..),
    mkDescribeHandshakeResponse,

    -- ** Response lenses
    dhrrsHandshake,
    dhrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeHandshake' smart constructor.
newtype DescribeHandshake = DescribeHandshake'
  { -- | The unique identifier (ID) of the handshake that you want information about. You can get the ID from the original call to 'InviteAccountToOrganization' , or from a call to 'ListHandshakesForAccount' or 'ListHandshakesForOrganization' .
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
    handshakeId :: Types.HandshakeId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHandshake' value with any optional fields omitted.
mkDescribeHandshake ::
  -- | 'handshakeId'
  Types.HandshakeId ->
  DescribeHandshake
mkDescribeHandshake handshakeId = DescribeHandshake' {handshakeId}

-- | The unique identifier (ID) of the handshake that you want information about. You can get the ID from the original call to 'InviteAccountToOrganization' , or from a call to 'ListHandshakesForAccount' or 'ListHandshakesForOrganization' .
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'handshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHandshakeId :: Lens.Lens' DescribeHandshake Types.HandshakeId
dhHandshakeId = Lens.field @"handshakeId"
{-# DEPRECATED dhHandshakeId "Use generic-lens or generic-optics with 'handshakeId' instead." #-}

instance Core.FromJSON DescribeHandshake where
  toJSON DescribeHandshake {..} =
    Core.object
      (Core.catMaybes [Core.Just ("HandshakeId" Core..= handshakeId)])

instance Core.AWSRequest DescribeHandshake where
  type Rs DescribeHandshake = DescribeHandshakeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSOrganizationsV20161128.DescribeHandshake")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHandshakeResponse'
            Core.<$> (x Core..:? "Handshake") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeHandshakeResponse' smart constructor.
data DescribeHandshakeResponse = DescribeHandshakeResponse'
  { -- | A structure that contains information about the specified handshake.
    handshake :: Core.Maybe Types.Handshake,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeHandshakeResponse' value with any optional fields omitted.
mkDescribeHandshakeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeHandshakeResponse
mkDescribeHandshakeResponse responseStatus =
  DescribeHandshakeResponse'
    { handshake = Core.Nothing,
      responseStatus
    }

-- | A structure that contains information about the specified handshake.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsHandshake :: Lens.Lens' DescribeHandshakeResponse (Core.Maybe Types.Handshake)
dhrrsHandshake = Lens.field @"handshake"
{-# DEPRECATED dhrrsHandshake "Use generic-lens or generic-optics with 'handshake' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsResponseStatus :: Lens.Lens' DescribeHandshakeResponse Core.Int
dhrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dhrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
