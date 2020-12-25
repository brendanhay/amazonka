{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.VerifyEmailAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @VerifyEmailIdentity@ operation to verify a new email address.
module Network.AWS.SES.VerifyEmailAddress
  ( -- * Creating a request
    VerifyEmailAddress (..),
    mkVerifyEmailAddress,

    -- ** Request lenses
    veaEmailAddress,

    -- * Destructuring the response
    VerifyEmailAddressResponse (..),
    mkVerifyEmailAddressResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkVerifyEmailAddress' smart constructor.
newtype VerifyEmailAddress = VerifyEmailAddress'
  { -- | The email address to be verified.
    emailAddress :: Types.Address
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyEmailAddress' value with any optional fields omitted.
mkVerifyEmailAddress ::
  -- | 'emailAddress'
  Types.Address ->
  VerifyEmailAddress
mkVerifyEmailAddress emailAddress =
  VerifyEmailAddress' {emailAddress}

-- | The email address to be verified.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veaEmailAddress :: Lens.Lens' VerifyEmailAddress Types.Address
veaEmailAddress = Lens.field @"emailAddress"
{-# DEPRECATED veaEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

instance Core.AWSRequest VerifyEmailAddress where
  type Rs VerifyEmailAddress = VerifyEmailAddressResponse
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
            ( Core.pure ("Action", "VerifyEmailAddress")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "EmailAddress" emailAddress)
            )
      }
  response = Response.receiveNull VerifyEmailAddressResponse'

-- | /See:/ 'mkVerifyEmailAddressResponse' smart constructor.
data VerifyEmailAddressResponse = VerifyEmailAddressResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyEmailAddressResponse' value with any optional fields omitted.
mkVerifyEmailAddressResponse ::
  VerifyEmailAddressResponse
mkVerifyEmailAddressResponse = VerifyEmailAddressResponse'
