{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetReusableDelegationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified reusable delegation set, including the four name servers that are assigned to the delegation set.
module Network.AWS.Route53.GetReusableDelegationSet
  ( -- * Creating a request
    GetReusableDelegationSet (..),
    mkGetReusableDelegationSet,

    -- ** Request lenses
    grdsId,

    -- * Destructuring the response
    GetReusableDelegationSetResponse (..),
    mkGetReusableDelegationSetResponse,

    -- ** Response lenses
    grdsrrsDelegationSet,
    grdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to get information about a specified reusable delegation set.
--
-- /See:/ 'mkGetReusableDelegationSet' smart constructor.
newtype GetReusableDelegationSet = GetReusableDelegationSet'
  { -- | The ID of the reusable delegation set that you want to get a list of name servers for.
    id :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetReusableDelegationSet' value with any optional fields omitted.
mkGetReusableDelegationSet ::
  -- | 'id'
  Types.ResourceId ->
  GetReusableDelegationSet
mkGetReusableDelegationSet id = GetReusableDelegationSet' {id}

-- | The ID of the reusable delegation set that you want to get a list of name servers for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsId :: Lens.Lens' GetReusableDelegationSet Types.ResourceId
grdsId = Lens.field @"id"
{-# DEPRECATED grdsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetReusableDelegationSet where
  type Rs GetReusableDelegationSet = GetReusableDelegationSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/2013-04-01/delegationset/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetReusableDelegationSetResponse'
            Core.<$> (x Core..@ "DelegationSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response to the @GetReusableDelegationSet@ request.
--
-- /See:/ 'mkGetReusableDelegationSetResponse' smart constructor.
data GetReusableDelegationSetResponse = GetReusableDelegationSetResponse'
  { -- | A complex type that contains information about the reusable delegation set.
    delegationSet :: Types.DelegationSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReusableDelegationSetResponse' value with any optional fields omitted.
mkGetReusableDelegationSetResponse ::
  -- | 'delegationSet'
  Types.DelegationSet ->
  -- | 'responseStatus'
  Core.Int ->
  GetReusableDelegationSetResponse
mkGetReusableDelegationSetResponse delegationSet responseStatus =
  GetReusableDelegationSetResponse' {delegationSet, responseStatus}

-- | A complex type that contains information about the reusable delegation set.
--
-- /Note:/ Consider using 'delegationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsrrsDelegationSet :: Lens.Lens' GetReusableDelegationSetResponse Types.DelegationSet
grdsrrsDelegationSet = Lens.field @"delegationSet"
{-# DEPRECATED grdsrrsDelegationSet "Use generic-lens or generic-optics with 'delegationSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsrrsResponseStatus :: Lens.Lens' GetReusableDelegationSetResponse Core.Int
grdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
