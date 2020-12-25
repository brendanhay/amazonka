{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetIdentityDkimEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables Easy DKIM signing of email sent from an identity. If Easy DKIM signing is enabled for a domain, then Amazon SES uses DKIM to sign all email that it sends from addresses on that domain. If Easy DKIM signing is enabled for an email address, then Amazon SES uses DKIM to sign all email it sends from that address.
--
-- You can enable DKIM signing for an identity at any time after you start the verification process for the identity, even if the verification process isn't complete.
-- You can execute this operation no more than once per second.
-- For more information about Easy DKIM signing, go to the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
module Network.AWS.SES.SetIdentityDkimEnabled
  ( -- * Creating a request
    SetIdentityDkimEnabled (..),
    mkSetIdentityDkimEnabled,

    -- ** Request lenses
    sideIdentity,
    sideDkimEnabled,

    -- * Destructuring the response
    SetIdentityDkimEnabledResponse (..),
    mkSetIdentityDkimEnabledResponse,

    -- ** Response lenses
    siderrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to enable or disable Amazon SES Easy DKIM signing for an identity. For more information about setting up Easy DKIM, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetIdentityDkimEnabled' smart constructor.
data SetIdentityDkimEnabled = SetIdentityDkimEnabled'
  { -- | The identity for which DKIM signing should be enabled or disabled.
    identity :: Types.Identity,
    -- | Sets whether DKIM signing is enabled for an identity. Set to @true@ to enable DKIM signing for this identity; @false@ to disable it.
    dkimEnabled :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityDkimEnabled' value with any optional fields omitted.
mkSetIdentityDkimEnabled ::
  -- | 'identity'
  Types.Identity ->
  -- | 'dkimEnabled'
  Core.Bool ->
  SetIdentityDkimEnabled
mkSetIdentityDkimEnabled identity dkimEnabled =
  SetIdentityDkimEnabled' {identity, dkimEnabled}

-- | The identity for which DKIM signing should be enabled or disabled.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sideIdentity :: Lens.Lens' SetIdentityDkimEnabled Types.Identity
sideIdentity = Lens.field @"identity"
{-# DEPRECATED sideIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | Sets whether DKIM signing is enabled for an identity. Set to @true@ to enable DKIM signing for this identity; @false@ to disable it.
--
-- /Note:/ Consider using 'dkimEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sideDkimEnabled :: Lens.Lens' SetIdentityDkimEnabled Core.Bool
sideDkimEnabled = Lens.field @"dkimEnabled"
{-# DEPRECATED sideDkimEnabled "Use generic-lens or generic-optics with 'dkimEnabled' instead." #-}

instance Core.AWSRequest SetIdentityDkimEnabled where
  type Rs SetIdentityDkimEnabled = SetIdentityDkimEnabledResponse
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
            ( Core.pure ("Action", "SetIdentityDkimEnabled")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Identity" identity)
                Core.<> (Core.toQueryValue "DkimEnabled" dkimEnabled)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetIdentityDkimEnabledResult"
      ( \s h x ->
          SetIdentityDkimEnabledResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetIdentityDkimEnabledResponse' smart constructor.
newtype SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityDkimEnabledResponse' value with any optional fields omitted.
mkSetIdentityDkimEnabledResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetIdentityDkimEnabledResponse
mkSetIdentityDkimEnabledResponse responseStatus =
  SetIdentityDkimEnabledResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siderrsResponseStatus :: Lens.Lens' SetIdentityDkimEnabledResponse Core.Int
siderrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED siderrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
