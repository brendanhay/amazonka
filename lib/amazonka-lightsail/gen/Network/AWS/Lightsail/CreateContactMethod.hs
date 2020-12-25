{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateContactMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an email or SMS text message contact method.
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
module Network.AWS.Lightsail.CreateContactMethod
  ( -- * Creating a request
    CreateContactMethod (..),
    mkCreateContactMethod,

    -- ** Request lenses
    ccmProtocol,
    ccmContactEndpoint,

    -- * Destructuring the response
    CreateContactMethodResponse (..),
    mkCreateContactMethodResponse,

    -- ** Response lenses
    ccmrrsOperations,
    ccmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateContactMethod' smart constructor.
data CreateContactMethod = CreateContactMethod'
  { -- | The protocol of the contact method, such as @Email@ or @SMS@ (text messaging).
    --
    -- The @SMS@ protocol is supported only in the following AWS Regions.
    --
    --     * US East (N. Virginia) (@us-east-1@ )
    --
    --
    --     * US West (Oregon) (@us-west-2@ )
    --
    --
    --     * Europe (Ireland) (@eu-west-1@ )
    --
    --
    --     * Asia Pacific (Tokyo) (@ap-northeast-1@ )
    --
    --
    --     * Asia Pacific (Singapore) (@ap-southeast-1@ )
    --
    --
    --     * Asia Pacific (Sydney) (@ap-southeast-2@ )
    --
    --
    -- For a list of countries/regions where SMS text messages can be sent, and the latest AWS Regions where SMS text messaging is supported, see <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries> in the /Amazon SNS Developer Guide/ .
    -- For more information about notifications in Amazon Lightsail, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
    protocol :: Types.ContactProtocol,
    -- | The destination of the contact method, such as an email address or a mobile phone number.
    --
    -- Use the E.164 format when specifying a mobile phone number. E.164 is a standard for the phone number structure used for international telecommunication. Phone numbers that follow this format can have a maximum of 15 digits, and they are prefixed with the plus character (+) and the country code. For example, a U.S. phone number in E.164 format would be specified as +1XXX5550100. For more information, see <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/ .
    contactEndpoint :: Types.ContactEndpoint
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContactMethod' value with any optional fields omitted.
mkCreateContactMethod ::
  -- | 'protocol'
  Types.ContactProtocol ->
  -- | 'contactEndpoint'
  Types.ContactEndpoint ->
  CreateContactMethod
mkCreateContactMethod protocol contactEndpoint =
  CreateContactMethod' {protocol, contactEndpoint}

-- | The protocol of the contact method, such as @Email@ or @SMS@ (text messaging).
--
-- The @SMS@ protocol is supported only in the following AWS Regions.
--
--     * US East (N. Virginia) (@us-east-1@ )
--
--
--     * US West (Oregon) (@us-west-2@ )
--
--
--     * Europe (Ireland) (@eu-west-1@ )
--
--
--     * Asia Pacific (Tokyo) (@ap-northeast-1@ )
--
--
--     * Asia Pacific (Singapore) (@ap-southeast-1@ )
--
--
--     * Asia Pacific (Sydney) (@ap-southeast-2@ )
--
--
-- For a list of countries/regions where SMS text messages can be sent, and the latest AWS Regions where SMS text messaging is supported, see <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries> in the /Amazon SNS Developer Guide/ .
-- For more information about notifications in Amazon Lightsail, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmProtocol :: Lens.Lens' CreateContactMethod Types.ContactProtocol
ccmProtocol = Lens.field @"protocol"
{-# DEPRECATED ccmProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The destination of the contact method, such as an email address or a mobile phone number.
--
-- Use the E.164 format when specifying a mobile phone number. E.164 is a standard for the phone number structure used for international telecommunication. Phone numbers that follow this format can have a maximum of 15 digits, and they are prefixed with the plus character (+) and the country code. For example, a U.S. phone number in E.164 format would be specified as +1XXX5550100. For more information, see <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/ .
--
-- /Note:/ Consider using 'contactEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmContactEndpoint :: Lens.Lens' CreateContactMethod Types.ContactEndpoint
ccmContactEndpoint = Lens.field @"contactEndpoint"
{-# DEPRECATED ccmContactEndpoint "Use generic-lens or generic-optics with 'contactEndpoint' instead." #-}

instance Core.FromJSON CreateContactMethod where
  toJSON CreateContactMethod {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("protocol" Core..= protocol),
            Core.Just ("contactEndpoint" Core..= contactEndpoint)
          ]
      )

instance Core.AWSRequest CreateContactMethod where
  type Rs CreateContactMethod = CreateContactMethodResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.CreateContactMethod")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContactMethodResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateContactMethodResponse' smart constructor.
data CreateContactMethodResponse = CreateContactMethodResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateContactMethodResponse' value with any optional fields omitted.
mkCreateContactMethodResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateContactMethodResponse
mkCreateContactMethodResponse responseStatus =
  CreateContactMethodResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmrrsOperations :: Lens.Lens' CreateContactMethodResponse (Core.Maybe [Types.Operation])
ccmrrsOperations = Lens.field @"operations"
{-# DEPRECATED ccmrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmrrsResponseStatus :: Lens.Lens' CreateContactMethodResponse Core.Int
ccmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
