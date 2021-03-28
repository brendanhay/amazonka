{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateContactMethod (..)
    , mkCreateContactMethod
    -- ** Request lenses
    , ccmProtocol
    , ccmContactEndpoint

    -- * Destructuring the response
    , CreateContactMethodResponse (..)
    , mkCreateContactMethodResponse
    -- ** Response lenses
    , ccmrrsOperations
    , ccmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateContactMethod' smart constructor.
data CreateContactMethod = CreateContactMethod'
  { protocol :: Types.ContactProtocol
    -- ^ The protocol of the contact method, such as @Email@ or @SMS@ (text messaging).
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
  , contactEndpoint :: Types.ContactEndpoint
    -- ^ The destination of the contact method, such as an email address or a mobile phone number.
--
-- Use the E.164 format when specifying a mobile phone number. E.164 is a standard for the phone number structure used for international telecommunication. Phone numbers that follow this format can have a maximum of 15 digits, and they are prefixed with the plus character (+) and the country code. For example, a U.S. phone number in E.164 format would be specified as +1XXX5550100. For more information, see <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContactMethod' value with any optional fields omitted.
mkCreateContactMethod
    :: Types.ContactProtocol -- ^ 'protocol'
    -> Types.ContactEndpoint -- ^ 'contactEndpoint'
    -> CreateContactMethod
mkCreateContactMethod protocol contactEndpoint
  = CreateContactMethod'{protocol, contactEndpoint}

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
{-# INLINEABLE ccmProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The destination of the contact method, such as an email address or a mobile phone number.
--
-- Use the E.164 format when specifying a mobile phone number. E.164 is a standard for the phone number structure used for international telecommunication. Phone numbers that follow this format can have a maximum of 15 digits, and they are prefixed with the plus character (+) and the country code. For example, a U.S. phone number in E.164 format would be specified as +1XXX5550100. For more information, see <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/ .
--
-- /Note:/ Consider using 'contactEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmContactEndpoint :: Lens.Lens' CreateContactMethod Types.ContactEndpoint
ccmContactEndpoint = Lens.field @"contactEndpoint"
{-# INLINEABLE ccmContactEndpoint #-}
{-# DEPRECATED contactEndpoint "Use generic-lens or generic-optics with 'contactEndpoint' instead"  #-}

instance Core.ToQuery CreateContactMethod where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateContactMethod where
        toHeaders CreateContactMethod{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateContactMethod")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateContactMethod where
        toJSON CreateContactMethod{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("protocol" Core..= protocol),
                  Core.Just ("contactEndpoint" Core..= contactEndpoint)])

instance Core.AWSRequest CreateContactMethod where
        type Rs CreateContactMethod = CreateContactMethodResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateContactMethodResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateContactMethodResponse' smart constructor.
data CreateContactMethodResponse = CreateContactMethodResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateContactMethodResponse' value with any optional fields omitted.
mkCreateContactMethodResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateContactMethodResponse
mkCreateContactMethodResponse responseStatus
  = CreateContactMethodResponse'{operations = Core.Nothing,
                                 responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmrrsOperations :: Lens.Lens' CreateContactMethodResponse (Core.Maybe [Types.Operation])
ccmrrsOperations = Lens.field @"operations"
{-# INLINEABLE ccmrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmrrsResponseStatus :: Lens.Lens' CreateContactMethodResponse Core.Int
ccmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
