{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateTrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates resource record sets in a specified hosted zone based on the settings in a specified traffic policy version. In addition, @CreateTrafficPolicyInstance@ associates the resource record sets with a specified domain name (such as example.com) or subdomain name (such as www.example.com). Amazon Route 53 responds to DNS queries for the domain or subdomain name by using the resource record sets that @CreateTrafficPolicyInstance@ created.
module Network.AWS.Route53.CreateTrafficPolicyInstance
    (
    -- * Creating a request
      CreateTrafficPolicyInstance (..)
    , mkCreateTrafficPolicyInstance
    -- ** Request lenses
    , ctpiHostedZoneId
    , ctpiName
    , ctpiTTL
    , ctpiTrafficPolicyId
    , ctpiTrafficPolicyVersion

    -- * Destructuring the response
    , CreateTrafficPolicyInstanceResponse (..)
    , mkCreateTrafficPolicyInstanceResponse
    -- ** Response lenses
    , ctpirrsTrafficPolicyInstance
    , ctpirrsLocation
    , ctpirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the resource record sets that you want to create based on a specified traffic policy.
--
-- /See:/ 'mkCreateTrafficPolicyInstance' smart constructor.
data CreateTrafficPolicyInstance = CreateTrafficPolicyInstance'
  { hostedZoneId :: Types.ResourceId
    -- ^ The ID of the hosted zone that you want Amazon Route 53 to create resource record sets in by using the configuration in a traffic policy.
  , name :: Types.DNSName
    -- ^ The domain name (such as example.com) or subdomain name (such as www.example.com) for which Amazon Route 53 responds to DNS queries by using the resource record sets that Route 53 creates for this traffic policy instance.
  , ttl :: Core.Natural
    -- ^ (Optional) The TTL that you want Amazon Route 53 to assign to all of the resource record sets that it creates in the specified hosted zone.
  , trafficPolicyId :: Types.TrafficPolicyId
    -- ^ The ID of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
  , trafficPolicyVersion :: Core.Natural
    -- ^ The version of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicyInstance' value with any optional fields omitted.
mkCreateTrafficPolicyInstance
    :: Types.ResourceId -- ^ 'hostedZoneId'
    -> Types.DNSName -- ^ 'name'
    -> Core.Natural -- ^ 'ttl'
    -> Types.TrafficPolicyId -- ^ 'trafficPolicyId'
    -> Core.Natural -- ^ 'trafficPolicyVersion'
    -> CreateTrafficPolicyInstance
mkCreateTrafficPolicyInstance hostedZoneId name ttl trafficPolicyId
  trafficPolicyVersion
  = CreateTrafficPolicyInstance'{hostedZoneId, name, ttl,
                                 trafficPolicyId, trafficPolicyVersion}

-- | The ID of the hosted zone that you want Amazon Route 53 to create resource record sets in by using the configuration in a traffic policy.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiHostedZoneId :: Lens.Lens' CreateTrafficPolicyInstance Types.ResourceId
ctpiHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE ctpiHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | The domain name (such as example.com) or subdomain name (such as www.example.com) for which Amazon Route 53 responds to DNS queries by using the resource record sets that Route 53 creates for this traffic policy instance.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiName :: Lens.Lens' CreateTrafficPolicyInstance Types.DNSName
ctpiName = Lens.field @"name"
{-# INLINEABLE ctpiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | (Optional) The TTL that you want Amazon Route 53 to assign to all of the resource record sets that it creates in the specified hosted zone.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiTTL :: Lens.Lens' CreateTrafficPolicyInstance Core.Natural
ctpiTTL = Lens.field @"ttl"
{-# INLINEABLE ctpiTTL #-}
{-# DEPRECATED ttl "Use generic-lens or generic-optics with 'ttl' instead"  #-}

-- | The ID of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiTrafficPolicyId :: Lens.Lens' CreateTrafficPolicyInstance Types.TrafficPolicyId
ctpiTrafficPolicyId = Lens.field @"trafficPolicyId"
{-# INLINEABLE ctpiTrafficPolicyId #-}
{-# DEPRECATED trafficPolicyId "Use generic-lens or generic-optics with 'trafficPolicyId' instead"  #-}

-- | The version of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiTrafficPolicyVersion :: Lens.Lens' CreateTrafficPolicyInstance Core.Natural
ctpiTrafficPolicyVersion = Lens.field @"trafficPolicyVersion"
{-# INLINEABLE ctpiTrafficPolicyVersion #-}
{-# DEPRECATED trafficPolicyVersion "Use generic-lens or generic-optics with 'trafficPolicyVersion' instead"  #-}

instance Core.ToQuery CreateTrafficPolicyInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTrafficPolicyInstance where
        toHeaders _ = Core.pure Core.mempty

instance Core.ToXML CreateTrafficPolicyInstance where
        toXML CreateTrafficPolicyInstance{..}
          = Core.toXMLElement "HostedZoneId" hostedZoneId Core.<>
              Core.toXMLElement "Name" name
              Core.<> Core.toXMLElement "TTL" ttl
              Core.<> Core.toXMLElement "TrafficPolicyId" trafficPolicyId
              Core.<>
              Core.toXMLElement "TrafficPolicyVersion" trafficPolicyVersion
        toXMLDocument
          = Core.newXMLDocument
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyInstanceRequest"

instance Core.AWSRequest CreateTrafficPolicyInstance where
        type Rs CreateTrafficPolicyInstance =
             CreateTrafficPolicyInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2013-04-01/trafficpolicyinstance",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateTrafficPolicyInstanceResponse' Core.<$>
                   (x Core..@ "TrafficPolicyInstance") Core.<*>
                     Core.parseHeader "Location" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response information for the @CreateTrafficPolicyInstance@ request.
--
-- /See:/ 'mkCreateTrafficPolicyInstanceResponse' smart constructor.
data CreateTrafficPolicyInstanceResponse = CreateTrafficPolicyInstanceResponse'
  { trafficPolicyInstance :: Types.TrafficPolicyInstance
    -- ^ A complex type that contains settings for the new traffic policy instance.
  , location :: Types.ResourceURI
    -- ^ A unique URL that represents a new traffic policy instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicyInstanceResponse' value with any optional fields omitted.
mkCreateTrafficPolicyInstanceResponse
    :: Types.TrafficPolicyInstance -- ^ 'trafficPolicyInstance'
    -> Types.ResourceURI -- ^ 'location'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateTrafficPolicyInstanceResponse
mkCreateTrafficPolicyInstanceResponse trafficPolicyInstance
  location responseStatus
  = CreateTrafficPolicyInstanceResponse'{trafficPolicyInstance,
                                         location, responseStatus}

-- | A complex type that contains settings for the new traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpirrsTrafficPolicyInstance :: Lens.Lens' CreateTrafficPolicyInstanceResponse Types.TrafficPolicyInstance
ctpirrsTrafficPolicyInstance = Lens.field @"trafficPolicyInstance"
{-# INLINEABLE ctpirrsTrafficPolicyInstance #-}
{-# DEPRECATED trafficPolicyInstance "Use generic-lens or generic-optics with 'trafficPolicyInstance' instead"  #-}

-- | A unique URL that represents a new traffic policy instance.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpirrsLocation :: Lens.Lens' CreateTrafficPolicyInstanceResponse Types.ResourceURI
ctpirrsLocation = Lens.field @"location"
{-# INLINEABLE ctpirrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpirrsResponseStatus :: Lens.Lens' CreateTrafficPolicyInstanceResponse Core.Int
ctpirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctpirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
