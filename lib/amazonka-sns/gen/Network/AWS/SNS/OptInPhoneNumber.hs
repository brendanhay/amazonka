{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.OptInPhoneNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this request to opt in a phone number that is opted out, which enables you to resume sending SMS messages to the number.
--
-- You can opt in a phone number only once every 30 days.
module Network.AWS.SNS.OptInPhoneNumber
  ( -- * Creating a request
    OptInPhoneNumber (..),
    mkOptInPhoneNumber,

    -- ** Request lenses
    oipnPhoneNumber,

    -- * Destructuring the response
    OptInPhoneNumberResponse (..),
    mkOptInPhoneNumberResponse,

    -- ** Response lenses
    oipnrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for the OptInPhoneNumber action.
--
-- /See:/ 'mkOptInPhoneNumber' smart constructor.
newtype OptInPhoneNumber = OptInPhoneNumber'
  { -- | The phone number to opt in.
    phoneNumber :: Types.PhoneNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OptInPhoneNumber' value with any optional fields omitted.
mkOptInPhoneNumber ::
  -- | 'phoneNumber'
  Types.PhoneNumber ->
  OptInPhoneNumber
mkOptInPhoneNumber phoneNumber = OptInPhoneNumber' {phoneNumber}

-- | The phone number to opt in.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oipnPhoneNumber :: Lens.Lens' OptInPhoneNumber Types.PhoneNumber
oipnPhoneNumber = Lens.field @"phoneNumber"
{-# DEPRECATED oipnPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

instance Core.AWSRequest OptInPhoneNumber where
  type Rs OptInPhoneNumber = OptInPhoneNumberResponse
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
            ( Core.pure ("Action", "OptInPhoneNumber")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> (Core.toQueryValue "phoneNumber" phoneNumber)
            )
      }
  response =
    Response.receiveXMLWrapper
      "OptInPhoneNumberResult"
      ( \s h x ->
          OptInPhoneNumberResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The response for the OptInPhoneNumber action.
--
-- /See:/ 'mkOptInPhoneNumberResponse' smart constructor.
newtype OptInPhoneNumberResponse = OptInPhoneNumberResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OptInPhoneNumberResponse' value with any optional fields omitted.
mkOptInPhoneNumberResponse ::
  -- | 'responseStatus'
  Core.Int ->
  OptInPhoneNumberResponse
mkOptInPhoneNumberResponse responseStatus =
  OptInPhoneNumberResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oipnrrsResponseStatus :: Lens.Lens' OptInPhoneNumberResponse Core.Int
oipnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED oipnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
