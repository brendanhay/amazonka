{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateReusableDelegationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delegation set (a group of four name servers) that can be reused by multiple hosted zones that were created by the same AWS account.
--
-- You can also create a reusable delegation set that uses the four name servers that are associated with an existing hosted zone. Specify the hosted zone ID in the @CreateReusableDelegationSet@ request.
-- For information about using a reusable delegation set to configure white label name servers, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/white-label-name-servers.html Configuring White Label Name Servers> .
-- The process for migrating existing hosted zones to use a reusable delegation set is comparable to the process for configuring white label name servers. You need to perform the following steps:
--
--     * Create a reusable delegation set.
--
--
--     * Recreate hosted zones, and reduce the TTL to 60 seconds or less.
--
--
--     * Recreate resource record sets in the new hosted zones.
--
--
--     * Change the registrar's name servers to use the name servers for the new hosted zones.
--
--
--     * Monitor traffic for the website or application.
--
--
--     * Change TTLs back to their original values.
--
--
-- If you want to migrate existing hosted zones to use a reusable delegation set, the existing hosted zones can't use any of the name servers that are assigned to the reusable delegation set. If one or more hosted zones do use one or more name servers that are assigned to the reusable delegation set, you can do one of the following:
--
--     * For small numbers of hosted zones—up to a few hundred—it's relatively easy to create reusable delegation sets until you get one that has four name servers that don't overlap with any of the name servers in your hosted zones.
--
--
--     * For larger numbers of hosted zones, the easiest solution is to use more than one reusable delegation set.
--
--
--     * For larger numbers of hosted zones, you can also migrate hosted zones that have overlapping name servers to hosted zones that don't have overlapping name servers, then migrate the hosted zones again to use the reusable delegation set.
module Network.AWS.Route53.CreateReusableDelegationSet
  ( -- * Creating a request
    CreateReusableDelegationSet (..),
    mkCreateReusableDelegationSet,

    -- ** Request lenses
    crdsCallerReference,
    crdsHostedZoneId,

    -- * Destructuring the response
    CreateReusableDelegationSetResponse (..),
    mkCreateReusableDelegationSetResponse,

    -- ** Response lenses
    crdsrrsDelegationSet,
    crdsrrsLocation,
    crdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | /See:/ 'mkCreateReusableDelegationSet' smart constructor.
data CreateReusableDelegationSet = CreateReusableDelegationSet'
  { -- | A unique string that identifies the request, and that allows you to retry failed @CreateReusableDelegationSet@ requests without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateReusableDelegationSet@ request. @CallerReference@ can be any unique string, for example a date/time stamp.
    callerReference :: Types.CallerReference,
    -- | If you want to mark the delegation set for an existing hosted zone as reusable, the ID for that hosted zone.
    hostedZoneId :: Core.Maybe Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReusableDelegationSet' value with any optional fields omitted.
mkCreateReusableDelegationSet ::
  -- | 'callerReference'
  Types.CallerReference ->
  CreateReusableDelegationSet
mkCreateReusableDelegationSet callerReference =
  CreateReusableDelegationSet'
    { callerReference,
      hostedZoneId = Core.Nothing
    }

-- | A unique string that identifies the request, and that allows you to retry failed @CreateReusableDelegationSet@ requests without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateReusableDelegationSet@ request. @CallerReference@ can be any unique string, for example a date/time stamp.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsCallerReference :: Lens.Lens' CreateReusableDelegationSet Types.CallerReference
crdsCallerReference = Lens.field @"callerReference"
{-# DEPRECATED crdsCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | If you want to mark the delegation set for an existing hosted zone as reusable, the ID for that hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsHostedZoneId :: Lens.Lens' CreateReusableDelegationSet (Core.Maybe Types.ResourceId)
crdsHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED crdsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

instance Core.ToXML CreateReusableDelegationSet where
  toXML CreateReusableDelegationSet {..} =
    Core.toXMLNode "CallerReference" callerReference
      Core.<> Core.toXMLNode "HostedZoneId" Core.<$> hostedZoneId
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateReusableDelegationSetRequest"

instance Core.AWSRequest CreateReusableDelegationSet where
  type
    Rs CreateReusableDelegationSet =
      CreateReusableDelegationSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2013-04-01/delegationset",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateReusableDelegationSetResponse'
            Core.<$> (x Core..@ "DelegationSet")
            Core.<*> (Core.parseHeader "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateReusableDelegationSetResponse' smart constructor.
data CreateReusableDelegationSetResponse = CreateReusableDelegationSetResponse'
  { -- | A complex type that contains name server information.
    delegationSet :: Types.DelegationSet,
    -- | The unique URL representing the new reusable delegation set.
    location :: Types.ResourceURI,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReusableDelegationSetResponse' value with any optional fields omitted.
mkCreateReusableDelegationSetResponse ::
  -- | 'delegationSet'
  Types.DelegationSet ->
  -- | 'location'
  Types.ResourceURI ->
  -- | 'responseStatus'
  Core.Int ->
  CreateReusableDelegationSetResponse
mkCreateReusableDelegationSetResponse
  delegationSet
  location
  responseStatus =
    CreateReusableDelegationSetResponse'
      { delegationSet,
        location,
        responseStatus
      }

-- | A complex type that contains name server information.
--
-- /Note:/ Consider using 'delegationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrrsDelegationSet :: Lens.Lens' CreateReusableDelegationSetResponse Types.DelegationSet
crdsrrsDelegationSet = Lens.field @"delegationSet"
{-# DEPRECATED crdsrrsDelegationSet "Use generic-lens or generic-optics with 'delegationSet' instead." #-}

-- | The unique URL representing the new reusable delegation set.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrrsLocation :: Lens.Lens' CreateReusableDelegationSetResponse Types.ResourceURI
crdsrrsLocation = Lens.field @"location"
{-# DEPRECATED crdsrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrrsResponseStatus :: Lens.Lens' CreateReusableDelegationSetResponse Core.Int
crdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
