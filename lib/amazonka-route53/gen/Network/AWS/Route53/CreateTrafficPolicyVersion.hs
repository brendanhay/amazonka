{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateTrafficPolicyVersion (..),
    mkCreateTrafficPolicyVersion,

    -- ** Request lenses
    ctpvId,
    ctpvDocument,
    ctpvComment,

    -- * Destructuring the response
    CreateTrafficPolicyVersionResponse (..),
    mkCreateTrafficPolicyVersionResponse,

    -- ** Response lenses
    ctpvrrsTrafficPolicy,
    ctpvrrsLocation,
    ctpvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the traffic policy that you want to create a new version for.
--
-- /See:/ 'mkCreateTrafficPolicyVersion' smart constructor.
data CreateTrafficPolicyVersion = CreateTrafficPolicyVersion'
  { -- | The ID of the traffic policy for which you want to create a new version.
    id :: Types.Id,
    -- | The definition of this version of the traffic policy, in JSON format. You specified the JSON in the @CreateTrafficPolicyVersion@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy> .
    document :: Types.Document,
    -- | The comment that you specified in the @CreateTrafficPolicyVersion@ request, if any.
    comment :: Core.Maybe Types.Comment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicyVersion' value with any optional fields omitted.
mkCreateTrafficPolicyVersion ::
  -- | 'id'
  Types.Id ->
  -- | 'document'
  Types.Document ->
  CreateTrafficPolicyVersion
mkCreateTrafficPolicyVersion id document =
  CreateTrafficPolicyVersion' {id, document, comment = Core.Nothing}

-- | The ID of the traffic policy for which you want to create a new version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvId :: Lens.Lens' CreateTrafficPolicyVersion Types.Id
ctpvId = Lens.field @"id"
{-# DEPRECATED ctpvId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The definition of this version of the traffic policy, in JSON format. You specified the JSON in the @CreateTrafficPolicyVersion@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy> .
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvDocument :: Lens.Lens' CreateTrafficPolicyVersion Types.Document
ctpvDocument = Lens.field @"document"
{-# DEPRECATED ctpvDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | The comment that you specified in the @CreateTrafficPolicyVersion@ request, if any.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvComment :: Lens.Lens' CreateTrafficPolicyVersion (Core.Maybe Types.Comment)
ctpvComment = Lens.field @"comment"
{-# DEPRECATED ctpvComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.ToXML CreateTrafficPolicyVersion where
  toXML CreateTrafficPolicyVersion {..} =
    Core.toXMLNode "Document" document
      Core.<> Core.toXMLNode "Comment" Core.<$> comment
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyVersionRequest"

instance Core.AWSRequest CreateTrafficPolicyVersion where
  type
    Rs CreateTrafficPolicyVersion =
      CreateTrafficPolicyVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/2013-04-01/trafficpolicy/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficPolicyVersionResponse'
            Core.<$> (x Core..@ "TrafficPolicy")
            Core.<*> (Core.parseHeader "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information for the @CreateTrafficPolicyVersion@ request.
--
-- /See:/ 'mkCreateTrafficPolicyVersionResponse' smart constructor.
data CreateTrafficPolicyVersionResponse = CreateTrafficPolicyVersionResponse'
  { -- | A complex type that contains settings for the new version of the traffic policy.
    trafficPolicy :: Types.TrafficPolicy,
    -- | A unique URL that represents a new traffic policy version.
    location :: Types.ResourceURI,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficPolicyVersionResponse' value with any optional fields omitted.
mkCreateTrafficPolicyVersionResponse ::
  -- | 'trafficPolicy'
  Types.TrafficPolicy ->
  -- | 'location'
  Types.ResourceURI ->
  -- | 'responseStatus'
  Core.Int ->
  CreateTrafficPolicyVersionResponse
mkCreateTrafficPolicyVersionResponse
  trafficPolicy
  location
  responseStatus =
    CreateTrafficPolicyVersionResponse'
      { trafficPolicy,
        location,
        responseStatus
      }

-- | A complex type that contains settings for the new version of the traffic policy.
--
-- /Note:/ Consider using 'trafficPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvrrsTrafficPolicy :: Lens.Lens' CreateTrafficPolicyVersionResponse Types.TrafficPolicy
ctpvrrsTrafficPolicy = Lens.field @"trafficPolicy"
{-# DEPRECATED ctpvrrsTrafficPolicy "Use generic-lens or generic-optics with 'trafficPolicy' instead." #-}

-- | A unique URL that represents a new traffic policy version.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvrrsLocation :: Lens.Lens' CreateTrafficPolicyVersionResponse Types.ResourceURI
ctpvrrsLocation = Lens.field @"location"
{-# DEPRECATED ctpvrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvrrsResponseStatus :: Lens.Lens' CreateTrafficPolicyVersionResponse Core.Int
ctpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
