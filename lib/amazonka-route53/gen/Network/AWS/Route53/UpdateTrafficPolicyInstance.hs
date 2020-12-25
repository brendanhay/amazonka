{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.UpdateTrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource record sets in a specified hosted zone that were created based on the settings in a specified traffic policy version.
--
-- When you update a traffic policy instance, Amazon Route 53 continues to respond to DNS queries for the root resource record set name (such as example.com) while it replaces one group of resource record sets with another. Route 53 performs the following operations:
--
--     * Route 53 creates a new group of resource record sets based on the specified traffic policy. This is true regardless of how significant the differences are between the existing resource record sets and the new resource record sets.
--
--
--     * When all of the new resource record sets have been created, Route 53 starts to respond to DNS queries for the root resource record set name (such as example.com) by using the new resource record sets.
--
--
--     * Route 53 deletes the old group of resource record sets that are associated with the root resource record set name.
module Network.AWS.Route53.UpdateTrafficPolicyInstance
  ( -- * Creating a request
    UpdateTrafficPolicyInstance (..),
    mkUpdateTrafficPolicyInstance,

    -- ** Request lenses
    utpiId,
    utpiTTL,
    utpiTrafficPolicyId,
    utpiTrafficPolicyVersion,

    -- * Destructuring the response
    UpdateTrafficPolicyInstanceResponse (..),
    mkUpdateTrafficPolicyInstanceResponse,

    -- ** Response lenses
    utpirrsTrafficPolicyInstance,
    utpirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the resource record sets that you want to update based on a specified traffic policy instance.
--
-- /See:/ 'mkUpdateTrafficPolicyInstance' smart constructor.
data UpdateTrafficPolicyInstance = UpdateTrafficPolicyInstance'
  { -- | The ID of the traffic policy instance that you want to update.
    id :: Types.TrafficPolicyInstanceId,
    -- | The TTL that you want Amazon Route 53 to assign to all of the updated resource record sets.
    ttl :: Core.Natural,
    -- | The ID of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
    trafficPolicyId :: Types.TrafficPolicyId,
    -- | The version of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
    trafficPolicyVersion :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrafficPolicyInstance' value with any optional fields omitted.
mkUpdateTrafficPolicyInstance ::
  -- | 'id'
  Types.TrafficPolicyInstanceId ->
  -- | 'ttl'
  Core.Natural ->
  -- | 'trafficPolicyId'
  Types.TrafficPolicyId ->
  -- | 'trafficPolicyVersion'
  Core.Natural ->
  UpdateTrafficPolicyInstance
mkUpdateTrafficPolicyInstance
  id
  ttl
  trafficPolicyId
  trafficPolicyVersion =
    UpdateTrafficPolicyInstance'
      { id,
        ttl,
        trafficPolicyId,
        trafficPolicyVersion
      }

-- | The ID of the traffic policy instance that you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpiId :: Lens.Lens' UpdateTrafficPolicyInstance Types.TrafficPolicyInstanceId
utpiId = Lens.field @"id"
{-# DEPRECATED utpiId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The TTL that you want Amazon Route 53 to assign to all of the updated resource record sets.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpiTTL :: Lens.Lens' UpdateTrafficPolicyInstance Core.Natural
utpiTTL = Lens.field @"ttl"
{-# DEPRECATED utpiTTL "Use generic-lens or generic-optics with 'ttl' instead." #-}

-- | The ID of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpiTrafficPolicyId :: Lens.Lens' UpdateTrafficPolicyInstance Types.TrafficPolicyId
utpiTrafficPolicyId = Lens.field @"trafficPolicyId"
{-# DEPRECATED utpiTrafficPolicyId "Use generic-lens or generic-optics with 'trafficPolicyId' instead." #-}

-- | The version of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpiTrafficPolicyVersion :: Lens.Lens' UpdateTrafficPolicyInstance Core.Natural
utpiTrafficPolicyVersion = Lens.field @"trafficPolicyVersion"
{-# DEPRECATED utpiTrafficPolicyVersion "Use generic-lens or generic-optics with 'trafficPolicyVersion' instead." #-}

instance Core.ToXML UpdateTrafficPolicyInstance where
  toXML UpdateTrafficPolicyInstance {..} =
    Core.toXMLNode "TTL" ttl
      Core.<> Core.toXMLNode "TrafficPolicyId" trafficPolicyId
      Core.<> Core.toXMLNode "TrafficPolicyVersion" trafficPolicyVersion
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateTrafficPolicyInstanceRequest"

instance Core.AWSRequest UpdateTrafficPolicyInstance where
  type
    Rs UpdateTrafficPolicyInstance =
      UpdateTrafficPolicyInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/2013-04-01/trafficpolicyinstance/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateTrafficPolicyInstanceResponse'
            Core.<$> (x Core..@ "TrafficPolicyInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.
--
-- /See:/ 'mkUpdateTrafficPolicyInstanceResponse' smart constructor.
data UpdateTrafficPolicyInstanceResponse = UpdateTrafficPolicyInstanceResponse'
  { -- | A complex type that contains settings for the updated traffic policy instance.
    trafficPolicyInstance :: Types.TrafficPolicyInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrafficPolicyInstanceResponse' value with any optional fields omitted.
mkUpdateTrafficPolicyInstanceResponse ::
  -- | 'trafficPolicyInstance'
  Types.TrafficPolicyInstance ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateTrafficPolicyInstanceResponse
mkUpdateTrafficPolicyInstanceResponse
  trafficPolicyInstance
  responseStatus =
    UpdateTrafficPolicyInstanceResponse'
      { trafficPolicyInstance,
        responseStatus
      }

-- | A complex type that contains settings for the updated traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpirrsTrafficPolicyInstance :: Lens.Lens' UpdateTrafficPolicyInstanceResponse Types.TrafficPolicyInstance
utpirrsTrafficPolicyInstance = Lens.field @"trafficPolicyInstance"
{-# DEPRECATED utpirrsTrafficPolicyInstance "Use generic-lens or generic-optics with 'trafficPolicyInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpirrsResponseStatus :: Lens.Lens' UpdateTrafficPolicyInstanceResponse Core.Int
utpirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utpirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
