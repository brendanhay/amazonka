{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified identity (an email address or a domain) from the list of verified identities.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteIdentity
  ( -- * Creating a request
    DeleteIdentity (..),
    mkDeleteIdentity,

    -- ** Request lenses
    diIdentity,

    -- * Destructuring the response
    DeleteIdentityResponse (..),
    mkDeleteIdentityResponse,

    -- ** Response lenses
    dirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete one of your Amazon SES identities (an email address or domain).
--
-- /See:/ 'mkDeleteIdentity' smart constructor.
newtype DeleteIdentity = DeleteIdentity'
  { -- | The identity to be removed from the list of identities for the AWS Account.
    identity :: Types.Identity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentity' value with any optional fields omitted.
mkDeleteIdentity ::
  -- | 'identity'
  Types.Identity ->
  DeleteIdentity
mkDeleteIdentity identity = DeleteIdentity' {identity}

-- | The identity to be removed from the list of identities for the AWS Account.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIdentity :: Lens.Lens' DeleteIdentity Types.Identity
diIdentity = Lens.field @"identity"
{-# DEPRECATED diIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Core.AWSRequest DeleteIdentity where
  type Rs DeleteIdentity = DeleteIdentityResponse
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
            ( Core.pure ("Action", "DeleteIdentity")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Identity" identity)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteIdentityResult"
      ( \s h x ->
          DeleteIdentityResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteIdentityResponse' smart constructor.
newtype DeleteIdentityResponse = DeleteIdentityResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityResponse' value with any optional fields omitted.
mkDeleteIdentityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteIdentityResponse
mkDeleteIdentityResponse responseStatus =
  DeleteIdentityResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteIdentityResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
