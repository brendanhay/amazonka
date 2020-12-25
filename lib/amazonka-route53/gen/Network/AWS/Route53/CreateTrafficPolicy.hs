{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateTrafficPolicy (..),
    mkCreateTrafficPolicy,

    -- ** Request lenses
    ctpName,
    ctpDocument,
    ctpComment,

    -- * Destructuring the response
    CreateTrafficPolicyResponse (..),
    mkCreateTrafficPolicyResponse,

    -- ** Response lenses
    ctprrsTrafficPolicy,
    ctprrsLocation,
    ctprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the traffic policy that you want to create.
--
-- /See:/ 'mkCreateTrafficPolicy' smart constructor.
data CreateTrafficPolicy = CreateTrafficPolicy'
  { -- | The name of the traffic policy.
    name :: Types.TrafficPolicyName,
    -- | The definition of this traffic policy in JSON format. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
    document :: Types.TrafficPolicyDocument,
    -- | (Optional) Any comments that you want to include about the traffic policy.
    comment :: Core.Maybe Types.Comment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicy' value with any optional fields omitted.
mkCreateTrafficPolicy ::
  -- | 'name'
  Types.TrafficPolicyName ->
  -- | 'document'
  Types.TrafficPolicyDocument ->
  CreateTrafficPolicy
mkCreateTrafficPolicy name document =
  CreateTrafficPolicy' {name, document, comment = Core.Nothing}

-- | The name of the traffic policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpName :: Lens.Lens' CreateTrafficPolicy Types.TrafficPolicyName
ctpName = Lens.field @"name"
{-# DEPRECATED ctpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The definition of this traffic policy in JSON format. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpDocument :: Lens.Lens' CreateTrafficPolicy Types.TrafficPolicyDocument
ctpDocument = Lens.field @"document"
{-# DEPRECATED ctpDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | (Optional) Any comments that you want to include about the traffic policy.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpComment :: Lens.Lens' CreateTrafficPolicy (Core.Maybe Types.Comment)
ctpComment = Lens.field @"comment"
{-# DEPRECATED ctpComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.ToXML CreateTrafficPolicy where
  toXML CreateTrafficPolicy {..} =
    Core.toXMLNode "Name" name
      Core.<> Core.toXMLNode "Document" document
      Core.<> Core.toXMLNode "Comment" Core.<$> comment
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyRequest"

instance Core.AWSRequest CreateTrafficPolicy where
  type Rs CreateTrafficPolicy = CreateTrafficPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2013-04-01/trafficpolicy",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficPolicyResponse'
            Core.<$> (x Core..@ "TrafficPolicy")
            Core.<*> (Core.parseHeader "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information for the @CreateTrafficPolicy@ request.
--
-- /See:/ 'mkCreateTrafficPolicyResponse' smart constructor.
data CreateTrafficPolicyResponse = CreateTrafficPolicyResponse'
  { -- | A complex type that contains settings for the new traffic policy.
    trafficPolicy :: Types.TrafficPolicy,
    -- | A unique URL that represents a new traffic policy.
    location :: Types.ResourceURI,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicyResponse' value with any optional fields omitted.
mkCreateTrafficPolicyResponse ::
  -- | 'trafficPolicy'
  Types.TrafficPolicy ->
  -- | 'location'
  Types.ResourceURI ->
  -- | 'responseStatus'
  Core.Int ->
  CreateTrafficPolicyResponse
mkCreateTrafficPolicyResponse trafficPolicy location responseStatus =
  CreateTrafficPolicyResponse'
    { trafficPolicy,
      location,
      responseStatus
    }

-- | A complex type that contains settings for the new traffic policy.
--
-- /Note:/ Consider using 'trafficPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprrsTrafficPolicy :: Lens.Lens' CreateTrafficPolicyResponse Types.TrafficPolicy
ctprrsTrafficPolicy = Lens.field @"trafficPolicy"
{-# DEPRECATED ctprrsTrafficPolicy "Use generic-lens or generic-optics with 'trafficPolicy' instead." #-}

-- | A unique URL that represents a new traffic policy.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprrsLocation :: Lens.Lens' CreateTrafficPolicyResponse Types.ResourceURI
ctprrsLocation = Lens.field @"location"
{-# DEPRECATED ctprrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprrsResponseStatus :: Lens.Lens' CreateTrafficPolicyResponse Core.Int
ctprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
