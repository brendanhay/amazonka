{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateTrafficPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a traffic policy, which you use to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com).
module Network.AWS.Route53.CreateTrafficPolicy
    (
    -- * Creating a request
      CreateTrafficPolicy (..)
    , mkCreateTrafficPolicy
    -- ** Request lenses
    , ctpName
    , ctpDocument
    , ctpComment

    -- * Destructuring the response
    , CreateTrafficPolicyResponse (..)
    , mkCreateTrafficPolicyResponse
    -- ** Response lenses
    , ctprrsTrafficPolicy
    , ctprrsLocation
    , ctprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the traffic policy that you want to create.
--
-- /See:/ 'mkCreateTrafficPolicy' smart constructor.
data CreateTrafficPolicy = CreateTrafficPolicy'
  { name :: Types.TrafficPolicyName
    -- ^ The name of the traffic policy.
  , document :: Types.TrafficPolicyDocument
    -- ^ The definition of this traffic policy in JSON format. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
  , comment :: Core.Maybe Types.Comment
    -- ^ (Optional) Any comments that you want to include about the traffic policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicy' value with any optional fields omitted.
mkCreateTrafficPolicy
    :: Types.TrafficPolicyName -- ^ 'name'
    -> Types.TrafficPolicyDocument -- ^ 'document'
    -> CreateTrafficPolicy
mkCreateTrafficPolicy name document
  = CreateTrafficPolicy'{name, document, comment = Core.Nothing}

-- | The name of the traffic policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpName :: Lens.Lens' CreateTrafficPolicy Types.TrafficPolicyName
ctpName = Lens.field @"name"
{-# INLINEABLE ctpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The definition of this traffic policy in JSON format. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpDocument :: Lens.Lens' CreateTrafficPolicy Types.TrafficPolicyDocument
ctpDocument = Lens.field @"document"
{-# INLINEABLE ctpDocument #-}
{-# DEPRECATED document "Use generic-lens or generic-optics with 'document' instead"  #-}

-- | (Optional) Any comments that you want to include about the traffic policy.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpComment :: Lens.Lens' CreateTrafficPolicy (Core.Maybe Types.Comment)
ctpComment = Lens.field @"comment"
{-# INLINEABLE ctpComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.ToQuery CreateTrafficPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTrafficPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.ToXML CreateTrafficPolicy where
        toXML CreateTrafficPolicy{..}
          = Core.toXMLElement "Name" name Core.<>
              Core.toXMLElement "Document" document
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Comment") comment
        toXMLDocument
          = Core.newXMLDocument
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyRequest"

instance Core.AWSRequest CreateTrafficPolicy where
        type Rs CreateTrafficPolicy = CreateTrafficPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2013-04-01/trafficpolicy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateTrafficPolicyResponse' Core.<$>
                   (x Core..@ "TrafficPolicy") Core.<*> Core.parseHeader "Location" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response information for the @CreateTrafficPolicy@ request.
--
-- /See:/ 'mkCreateTrafficPolicyResponse' smart constructor.
data CreateTrafficPolicyResponse = CreateTrafficPolicyResponse'
  { trafficPolicy :: Types.TrafficPolicy
    -- ^ A complex type that contains settings for the new traffic policy.
  , location :: Types.ResourceURI
    -- ^ A unique URL that represents a new traffic policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicyResponse' value with any optional fields omitted.
mkCreateTrafficPolicyResponse
    :: Types.TrafficPolicy -- ^ 'trafficPolicy'
    -> Types.ResourceURI -- ^ 'location'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateTrafficPolicyResponse
mkCreateTrafficPolicyResponse trafficPolicy location responseStatus
  = CreateTrafficPolicyResponse'{trafficPolicy, location,
                                 responseStatus}

-- | A complex type that contains settings for the new traffic policy.
--
-- /Note:/ Consider using 'trafficPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprrsTrafficPolicy :: Lens.Lens' CreateTrafficPolicyResponse Types.TrafficPolicy
ctprrsTrafficPolicy = Lens.field @"trafficPolicy"
{-# INLINEABLE ctprrsTrafficPolicy #-}
{-# DEPRECATED trafficPolicy "Use generic-lens or generic-optics with 'trafficPolicy' instead"  #-}

-- | A unique URL that represents a new traffic policy.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprrsLocation :: Lens.Lens' CreateTrafficPolicyResponse Types.ResourceURI
ctprrsLocation = Lens.field @"location"
{-# INLINEABLE ctprrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprrsResponseStatus :: Lens.Lens' CreateTrafficPolicyResponse Core.Int
ctprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
