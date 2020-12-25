{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.SendContactMethodVerification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a verification request to an email contact method to ensure it's owned by the requester. SMS contact methods don't need to be verified.
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
-- A verification request is sent to the contact method when you initially create it. Use this action to send another verification request if a previous verification request was deleted, or has expired.
-- /Important:/ Notifications are not sent to an email contact method until after it is verified, and confirmed as valid.
module Network.AWS.Lightsail.SendContactMethodVerification
  ( -- * Creating a request
    SendContactMethodVerification (..),
    mkSendContactMethodVerification,

    -- ** Request lenses
    scmvProtocol,

    -- * Destructuring the response
    SendContactMethodVerificationResponse (..),
    mkSendContactMethodVerificationResponse,

    -- ** Response lenses
    scmvrrsOperations,
    scmvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendContactMethodVerification' smart constructor.
newtype SendContactMethodVerification = SendContactMethodVerification'
  { -- | The protocol to verify, such as @Email@ or @SMS@ (text messaging).
    protocol :: Types.ContactMethodVerificationProtocol
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SendContactMethodVerification' value with any optional fields omitted.
mkSendContactMethodVerification ::
  -- | 'protocol'
  Types.ContactMethodVerificationProtocol ->
  SendContactMethodVerification
mkSendContactMethodVerification protocol =
  SendContactMethodVerification' {protocol}

-- | The protocol to verify, such as @Email@ or @SMS@ (text messaging).
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmvProtocol :: Lens.Lens' SendContactMethodVerification Types.ContactMethodVerificationProtocol
scmvProtocol = Lens.field @"protocol"
{-# DEPRECATED scmvProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

instance Core.FromJSON SendContactMethodVerification where
  toJSON SendContactMethodVerification {..} =
    Core.object
      (Core.catMaybes [Core.Just ("protocol" Core..= protocol)])

instance Core.AWSRequest SendContactMethodVerification where
  type
    Rs SendContactMethodVerification =
      SendContactMethodVerificationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.SendContactMethodVerification"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SendContactMethodVerificationResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSendContactMethodVerificationResponse' smart constructor.
data SendContactMethodVerificationResponse = SendContactMethodVerificationResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SendContactMethodVerificationResponse' value with any optional fields omitted.
mkSendContactMethodVerificationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SendContactMethodVerificationResponse
mkSendContactMethodVerificationResponse responseStatus =
  SendContactMethodVerificationResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmvrrsOperations :: Lens.Lens' SendContactMethodVerificationResponse (Core.Maybe [Types.Operation])
scmvrrsOperations = Lens.field @"operations"
{-# DEPRECATED scmvrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmvrrsResponseStatus :: Lens.Lens' SendContactMethodVerificationResponse Core.Int
scmvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scmvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
