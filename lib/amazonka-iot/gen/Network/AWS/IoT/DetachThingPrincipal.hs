{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DetachThingPrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified principal from the specified thing. A principal can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito identities or federated identities.
module Network.AWS.IoT.DetachThingPrincipal
  ( -- * Creating a request
    DetachThingPrincipal (..),
    mkDetachThingPrincipal,

    -- ** Request lenses
    dtpThingName,
    dtpPrincipal,

    -- * Destructuring the response
    DetachThingPrincipalResponse (..),
    mkDetachThingPrincipalResponse,

    -- ** Response lenses
    dtprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DetachThingPrincipal operation.
--
-- /See:/ 'mkDetachThingPrincipal' smart constructor.
data DetachThingPrincipal = DetachThingPrincipal'
  { -- | The name of the thing.
    thingName :: Types.ThingName,
    -- | If the principal is a certificate, this value must be ARN of the certificate. If the principal is an Amazon Cognito identity, this value must be the ID of the Amazon Cognito identity.
    principal :: Types.Principal
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachThingPrincipal' value with any optional fields omitted.
mkDetachThingPrincipal ::
  -- | 'thingName'
  Types.ThingName ->
  -- | 'principal'
  Types.Principal ->
  DetachThingPrincipal
mkDetachThingPrincipal thingName principal =
  DetachThingPrincipal' {thingName, principal}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpThingName :: Lens.Lens' DetachThingPrincipal Types.ThingName
dtpThingName = Lens.field @"thingName"
{-# DEPRECATED dtpThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | If the principal is a certificate, this value must be ARN of the certificate. If the principal is an Amazon Cognito identity, this value must be the ID of the Amazon Cognito identity.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpPrincipal :: Lens.Lens' DetachThingPrincipal Types.Principal
dtpPrincipal = Lens.field @"principal"
{-# DEPRECATED dtpPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

instance Core.AWSRequest DetachThingPrincipal where
  type Rs DetachThingPrincipal = DetachThingPrincipalResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/things/" Core.<> (Core.toText thingName)
                Core.<> ("/principals")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amzn-principal" principal,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DetachThingPrincipalResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The output from the DetachThingPrincipal operation.
--
-- /See:/ 'mkDetachThingPrincipalResponse' smart constructor.
newtype DetachThingPrincipalResponse = DetachThingPrincipalResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachThingPrincipalResponse' value with any optional fields omitted.
mkDetachThingPrincipalResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachThingPrincipalResponse
mkDetachThingPrincipalResponse responseStatus =
  DetachThingPrincipalResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprrsResponseStatus :: Lens.Lens' DetachThingPrincipalResponse Core.Int
dtprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
