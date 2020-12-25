{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHostedZoneCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the number of hosted zones that are associated with the current AWS account.
module Network.AWS.Route53.GetHostedZoneCount
  ( -- * Creating a request
    GetHostedZoneCount (..),
    mkGetHostedZoneCount,

    -- * Destructuring the response
    GetHostedZoneCountResponse (..),
    mkGetHostedZoneCountResponse,

    -- ** Response lenses
    ghzcrrsHostedZoneCount,
    ghzcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to retrieve a count of all the hosted zones that are associated with the current AWS account.
--
-- /See:/ 'mkGetHostedZoneCount' smart constructor.
data GetHostedZoneCount = GetHostedZoneCount'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetHostedZoneCount' value with any optional fields omitted.
mkGetHostedZoneCount ::
  GetHostedZoneCount
mkGetHostedZoneCount = GetHostedZoneCount'

instance Core.AWSRequest GetHostedZoneCount where
  type Rs GetHostedZoneCount = GetHostedZoneCountResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2013-04-01/hostedzonecount",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetHostedZoneCountResponse'
            Core.<$> (x Core..@ "HostedZoneCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response to a @GetHostedZoneCount@ request.
--
-- /See:/ 'mkGetHostedZoneCountResponse' smart constructor.
data GetHostedZoneCountResponse = GetHostedZoneCountResponse'
  { -- | The total number of public and private hosted zones that are associated with the current AWS account.
    hostedZoneCount :: Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetHostedZoneCountResponse' value with any optional fields omitted.
mkGetHostedZoneCountResponse ::
  -- | 'hostedZoneCount'
  Core.Integer ->
  -- | 'responseStatus'
  Core.Int ->
  GetHostedZoneCountResponse
mkGetHostedZoneCountResponse hostedZoneCount responseStatus =
  GetHostedZoneCountResponse' {hostedZoneCount, responseStatus}

-- | The total number of public and private hosted zones that are associated with the current AWS account.
--
-- /Note:/ Consider using 'hostedZoneCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzcrrsHostedZoneCount :: Lens.Lens' GetHostedZoneCountResponse Core.Integer
ghzcrrsHostedZoneCount = Lens.field @"hostedZoneCount"
{-# DEPRECATED ghzcrrsHostedZoneCount "Use generic-lens or generic-optics with 'hostedZoneCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzcrrsResponseStatus :: Lens.Lens' GetHostedZoneCountResponse Core.Int
ghzcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ghzcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
