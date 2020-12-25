{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CheckIfPhoneNumberIsOptedOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a phone number and indicates whether the phone holder has opted out of receiving SMS messages from your account. You cannot send SMS messages to a number that is opted out.
--
-- To resume sending messages, you can opt in the number by using the @OptInPhoneNumber@ action.
module Network.AWS.SNS.CheckIfPhoneNumberIsOptedOut
  ( -- * Creating a request
    CheckIfPhoneNumberIsOptedOut (..),
    mkCheckIfPhoneNumberIsOptedOut,

    -- ** Request lenses
    cipniooPhoneNumber,

    -- * Destructuring the response
    CheckIfPhoneNumberIsOptedOutResponse (..),
    mkCheckIfPhoneNumberIsOptedOutResponse,

    -- ** Response lenses
    cipnioorrsIsOptedOut,
    cipnioorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | The input for the @CheckIfPhoneNumberIsOptedOut@ action.
--
-- /See:/ 'mkCheckIfPhoneNumberIsOptedOut' smart constructor.
newtype CheckIfPhoneNumberIsOptedOut = CheckIfPhoneNumberIsOptedOut'
  { -- | The phone number for which you want to check the opt out status.
    phoneNumber :: Types.PhoneNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CheckIfPhoneNumberIsOptedOut' value with any optional fields omitted.
mkCheckIfPhoneNumberIsOptedOut ::
  -- | 'phoneNumber'
  Types.PhoneNumber ->
  CheckIfPhoneNumberIsOptedOut
mkCheckIfPhoneNumberIsOptedOut phoneNumber =
  CheckIfPhoneNumberIsOptedOut' {phoneNumber}

-- | The phone number for which you want to check the opt out status.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipniooPhoneNumber :: Lens.Lens' CheckIfPhoneNumberIsOptedOut Types.PhoneNumber
cipniooPhoneNumber = Lens.field @"phoneNumber"
{-# DEPRECATED cipniooPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

instance Core.AWSRequest CheckIfPhoneNumberIsOptedOut where
  type
    Rs CheckIfPhoneNumberIsOptedOut =
      CheckIfPhoneNumberIsOptedOutResponse
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
            ( Core.pure ("Action", "CheckIfPhoneNumberIsOptedOut")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> (Core.toQueryValue "phoneNumber" phoneNumber)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CheckIfPhoneNumberIsOptedOutResult"
      ( \s h x ->
          CheckIfPhoneNumberIsOptedOutResponse'
            Core.<$> (x Core..@? "isOptedOut") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response from the @CheckIfPhoneNumberIsOptedOut@ action.
--
-- /See:/ 'mkCheckIfPhoneNumberIsOptedOutResponse' smart constructor.
data CheckIfPhoneNumberIsOptedOutResponse = CheckIfPhoneNumberIsOptedOutResponse'
  { -- | Indicates whether the phone number is opted out:
    --
    --
    --     * @true@ – The phone number is opted out, meaning you cannot publish SMS messages to it.
    --
    --
    --     * @false@ – The phone number is opted in, meaning you can publish SMS messages to it.
    isOptedOut :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckIfPhoneNumberIsOptedOutResponse' value with any optional fields omitted.
mkCheckIfPhoneNumberIsOptedOutResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CheckIfPhoneNumberIsOptedOutResponse
mkCheckIfPhoneNumberIsOptedOutResponse responseStatus =
  CheckIfPhoneNumberIsOptedOutResponse'
    { isOptedOut = Core.Nothing,
      responseStatus
    }

-- | Indicates whether the phone number is opted out:
--
--
--     * @true@ – The phone number is opted out, meaning you cannot publish SMS messages to it.
--
--
--     * @false@ – The phone number is opted in, meaning you can publish SMS messages to it.
--
--
--
-- /Note:/ Consider using 'isOptedOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipnioorrsIsOptedOut :: Lens.Lens' CheckIfPhoneNumberIsOptedOutResponse (Core.Maybe Core.Bool)
cipnioorrsIsOptedOut = Lens.field @"isOptedOut"
{-# DEPRECATED cipnioorrsIsOptedOut "Use generic-lens or generic-optics with 'isOptedOut' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipnioorrsResponseStatus :: Lens.Lens' CheckIfPhoneNumberIsOptedOutResponse Core.Int
cipnioorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cipnioorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
