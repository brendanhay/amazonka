{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AttachThingPrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified principal to the specified thing. A principal can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito identities or federated identities.
module Network.AWS.IoT.AttachThingPrincipal
  ( -- * Creating a request
    AttachThingPrincipal (..),
    mkAttachThingPrincipal,

    -- ** Request lenses
    atpThingName,
    atpPrincipal,

    -- * Destructuring the response
    AttachThingPrincipalResponse (..),
    mkAttachThingPrincipalResponse,

    -- ** Response lenses
    atprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the AttachThingPrincipal operation.
--
-- /See:/ 'mkAttachThingPrincipal' smart constructor.
data AttachThingPrincipal = AttachThingPrincipal'
  { -- | The name of the thing.
    thingName :: Types.ThingName,
    -- | The principal, which can be a certificate ARN (as returned from the CreateCertificate operation) or an Amazon Cognito ID.
    principal :: Types.Principal
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachThingPrincipal' value with any optional fields omitted.
mkAttachThingPrincipal ::
  -- | 'thingName'
  Types.ThingName ->
  -- | 'principal'
  Types.Principal ->
  AttachThingPrincipal
mkAttachThingPrincipal thingName principal =
  AttachThingPrincipal' {thingName, principal}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpThingName :: Lens.Lens' AttachThingPrincipal Types.ThingName
atpThingName = Lens.field @"thingName"
{-# DEPRECATED atpThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The principal, which can be a certificate ARN (as returned from the CreateCertificate operation) or an Amazon Cognito ID.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpPrincipal :: Lens.Lens' AttachThingPrincipal Types.Principal
atpPrincipal = Lens.field @"principal"
{-# DEPRECATED atpPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

instance Core.FromJSON AttachThingPrincipal where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest AttachThingPrincipal where
  type Rs AttachThingPrincipal = AttachThingPrincipalResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/things/" Core.<> (Core.toText thingName)
                Core.<> ("/principals")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amzn-principal" principal,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachThingPrincipalResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The output from the AttachThingPrincipal operation.
--
-- /See:/ 'mkAttachThingPrincipalResponse' smart constructor.
newtype AttachThingPrincipalResponse = AttachThingPrincipalResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttachThingPrincipalResponse' value with any optional fields omitted.
mkAttachThingPrincipalResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AttachThingPrincipalResponse
mkAttachThingPrincipalResponse responseStatus =
  AttachThingPrincipalResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atprrsResponseStatus :: Lens.Lens' AttachThingPrincipalResponse Core.Int
atprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
