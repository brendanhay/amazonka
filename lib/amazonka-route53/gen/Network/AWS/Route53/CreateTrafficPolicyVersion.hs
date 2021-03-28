{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateTrafficPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of an existing traffic policy. When you create a new version of a traffic policy, you specify the ID of the traffic policy that you want to update and a JSON-formatted document that describes the new version. You use traffic policies to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com). You can create a maximum of 1000 versions of a traffic policy. If you reach the limit and need to create another version, you'll need to start a new traffic policy.
module Network.AWS.Route53.CreateTrafficPolicyVersion
    (
    -- * Creating a request
      CreateTrafficPolicyVersion (..)
    , mkCreateTrafficPolicyVersion
    -- ** Request lenses
    , ctpvId
    , ctpvDocument
    , ctpvComment

    -- * Destructuring the response
    , CreateTrafficPolicyVersionResponse (..)
    , mkCreateTrafficPolicyVersionResponse
    -- ** Response lenses
    , ctpvrrsTrafficPolicy
    , ctpvrrsLocation
    , ctpvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the traffic policy that you want to create a new version for.
--
-- /See:/ 'mkCreateTrafficPolicyVersion' smart constructor.
data CreateTrafficPolicyVersion = CreateTrafficPolicyVersion'
  { id :: Types.Id
    -- ^ The ID of the traffic policy for which you want to create a new version.
  , document :: Types.Document
    -- ^ The definition of this version of the traffic policy, in JSON format. You specified the JSON in the @CreateTrafficPolicyVersion@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy> .
  , comment :: Core.Maybe Types.Comment
    -- ^ The comment that you specified in the @CreateTrafficPolicyVersion@ request, if any.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicyVersion' value with any optional fields omitted.
mkCreateTrafficPolicyVersion
    :: Types.Id -- ^ 'id'
    -> Types.Document -- ^ 'document'
    -> CreateTrafficPolicyVersion
mkCreateTrafficPolicyVersion id document
  = CreateTrafficPolicyVersion'{id, document, comment = Core.Nothing}

-- | The ID of the traffic policy for which you want to create a new version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvId :: Lens.Lens' CreateTrafficPolicyVersion Types.Id
ctpvId = Lens.field @"id"
{-# INLINEABLE ctpvId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The definition of this version of the traffic policy, in JSON format. You specified the JSON in the @CreateTrafficPolicyVersion@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy> .
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvDocument :: Lens.Lens' CreateTrafficPolicyVersion Types.Document
ctpvDocument = Lens.field @"document"
{-# INLINEABLE ctpvDocument #-}
{-# DEPRECATED document "Use generic-lens or generic-optics with 'document' instead"  #-}

-- | The comment that you specified in the @CreateTrafficPolicyVersion@ request, if any.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvComment :: Lens.Lens' CreateTrafficPolicyVersion (Core.Maybe Types.Comment)
ctpvComment = Lens.field @"comment"
{-# INLINEABLE ctpvComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.ToQuery CreateTrafficPolicyVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTrafficPolicyVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.ToXML CreateTrafficPolicyVersion where
        toXML CreateTrafficPolicyVersion{..}
          = Core.toXMLElement "Document" document Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Comment") comment
        toXMLDocument
          = Core.newXMLDocument
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyVersionRequest"

instance Core.AWSRequest CreateTrafficPolicyVersion where
        type Rs CreateTrafficPolicyVersion =
             CreateTrafficPolicyVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2013-04-01/trafficpolicy/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateTrafficPolicyVersionResponse' Core.<$>
                   (x Core..@ "TrafficPolicy") Core.<*> Core.parseHeader "Location" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response information for the @CreateTrafficPolicyVersion@ request.
--
-- /See:/ 'mkCreateTrafficPolicyVersionResponse' smart constructor.
data CreateTrafficPolicyVersionResponse = CreateTrafficPolicyVersionResponse'
  { trafficPolicy :: Types.TrafficPolicy
    -- ^ A complex type that contains settings for the new version of the traffic policy.
  , location :: Types.ResourceURI
    -- ^ A unique URL that represents a new traffic policy version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicyVersionResponse' value with any optional fields omitted.
mkCreateTrafficPolicyVersionResponse
    :: Types.TrafficPolicy -- ^ 'trafficPolicy'
    -> Types.ResourceURI -- ^ 'location'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateTrafficPolicyVersionResponse
mkCreateTrafficPolicyVersionResponse trafficPolicy location
  responseStatus
  = CreateTrafficPolicyVersionResponse'{trafficPolicy, location,
                                        responseStatus}

-- | A complex type that contains settings for the new version of the traffic policy.
--
-- /Note:/ Consider using 'trafficPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvrrsTrafficPolicy :: Lens.Lens' CreateTrafficPolicyVersionResponse Types.TrafficPolicy
ctpvrrsTrafficPolicy = Lens.field @"trafficPolicy"
{-# INLINEABLE ctpvrrsTrafficPolicy #-}
{-# DEPRECATED trafficPolicy "Use generic-lens or generic-optics with 'trafficPolicy' instead"  #-}

-- | A unique URL that represents a new traffic policy version.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvrrsLocation :: Lens.Lens' CreateTrafficPolicyVersionResponse Types.ResourceURI
ctpvrrsLocation = Lens.field @"location"
{-# INLINEABLE ctpvrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvrrsResponseStatus :: Lens.Lens' CreateTrafficPolicyVersionResponse Core.Int
ctpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
