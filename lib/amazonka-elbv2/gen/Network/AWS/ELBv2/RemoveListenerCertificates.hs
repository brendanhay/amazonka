{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.RemoveListenerCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified certificate from the certificate list for the specified HTTPS or TLS listener.
module Network.AWS.ELBv2.RemoveListenerCertificates
  ( -- * Creating a request
    RemoveListenerCertificates (..),
    mkRemoveListenerCertificates,

    -- ** Request lenses
    rlcListenerArn,
    rlcCertificates,

    -- * Destructuring the response
    RemoveListenerCertificatesResponse (..),
    mkRemoveListenerCertificatesResponse,

    -- ** Response lenses
    rlcrrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveListenerCertificates' smart constructor.
data RemoveListenerCertificates = RemoveListenerCertificates'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Types.ListenerArn,
    -- | The certificate to remove. You can specify one certificate per call. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
    certificates :: [Types.Certificate]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveListenerCertificates' value with any optional fields omitted.
mkRemoveListenerCertificates ::
  -- | 'listenerArn'
  Types.ListenerArn ->
  RemoveListenerCertificates
mkRemoveListenerCertificates listenerArn =
  RemoveListenerCertificates'
    { listenerArn,
      certificates = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcListenerArn :: Lens.Lens' RemoveListenerCertificates Types.ListenerArn
rlcListenerArn = Lens.field @"listenerArn"
{-# DEPRECATED rlcListenerArn "Use generic-lens or generic-optics with 'listenerArn' instead." #-}

-- | The certificate to remove. You can specify one certificate per call. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcCertificates :: Lens.Lens' RemoveListenerCertificates [Types.Certificate]
rlcCertificates = Lens.field @"certificates"
{-# DEPRECATED rlcCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

instance Core.AWSRequest RemoveListenerCertificates where
  type
    Rs RemoveListenerCertificates =
      RemoveListenerCertificatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RemoveListenerCertificates")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "ListenerArn" listenerArn)
                Core.<> ( Core.toQueryValue
                            "Certificates"
                            (Core.toQueryList "member" certificates)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "RemoveListenerCertificatesResult"
      ( \s h x ->
          RemoveListenerCertificatesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveListenerCertificatesResponse' smart constructor.
newtype RemoveListenerCertificatesResponse = RemoveListenerCertificatesResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveListenerCertificatesResponse' value with any optional fields omitted.
mkRemoveListenerCertificatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveListenerCertificatesResponse
mkRemoveListenerCertificatesResponse responseStatus =
  RemoveListenerCertificatesResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcrrsResponseStatus :: Lens.Lens' RemoveListenerCertificatesResponse Core.Int
rlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
