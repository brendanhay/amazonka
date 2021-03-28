{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact with the specified details.
module Network.AWS.AlexaBusiness.CreateContact
    (
    -- * Creating a request
      CreateContact (..)
    , mkCreateContact
    -- ** Request lenses
    , ccFirstName
    , ccClientRequestToken
    , ccDisplayName
    , ccLastName
    , ccPhoneNumber
    , ccPhoneNumbers
    , ccSipAddresses

    -- * Destructuring the response
    , CreateContactResponse (..)
    , mkCreateContactResponse
    -- ** Response lenses
    , ccrrsContactArn
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateContact' smart constructor.
data CreateContact = CreateContact'
  { firstName :: Types.FirstName
    -- ^ The first name of the contact that is used to call the contact on the device.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique, user-specified identifier for this request that ensures idempotency.
  , displayName :: Core.Maybe Types.DisplayName
    -- ^ The name of the contact to display on the console.
  , lastName :: Core.Maybe Types.LastName
    -- ^ The last name of the contact that is used to call the contact on the device.
  , phoneNumber :: Core.Maybe Types.RawPhoneNumber
    -- ^ The phone number of the contact in E.164 format. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
  , phoneNumbers :: Core.Maybe [Types.PhoneNumber]
    -- ^ The list of phone numbers for the contact.
  , sipAddresses :: Core.Maybe [Types.SipAddress]
    -- ^ The list of SIP addresses for the contact.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContact' value with any optional fields omitted.
mkCreateContact
    :: Types.FirstName -- ^ 'firstName'
    -> CreateContact
mkCreateContact firstName
  = CreateContact'{firstName, clientRequestToken = Core.Nothing,
                   displayName = Core.Nothing, lastName = Core.Nothing,
                   phoneNumber = Core.Nothing, phoneNumbers = Core.Nothing,
                   sipAddresses = Core.Nothing}

-- | The first name of the contact that is used to call the contact on the device.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccFirstName :: Lens.Lens' CreateContact Types.FirstName
ccFirstName = Lens.field @"firstName"
{-# INLINEABLE ccFirstName #-}
{-# DEPRECATED firstName "Use generic-lens or generic-optics with 'firstName' instead"  #-}

-- | A unique, user-specified identifier for this request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClientRequestToken :: Lens.Lens' CreateContact (Core.Maybe Types.ClientRequestToken)
ccClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE ccClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The name of the contact to display on the console.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDisplayName :: Lens.Lens' CreateContact (Core.Maybe Types.DisplayName)
ccDisplayName = Lens.field @"displayName"
{-# INLINEABLE ccDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The last name of the contact that is used to call the contact on the device.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLastName :: Lens.Lens' CreateContact (Core.Maybe Types.LastName)
ccLastName = Lens.field @"lastName"
{-# INLINEABLE ccLastName #-}
{-# DEPRECATED lastName "Use generic-lens or generic-optics with 'lastName' instead"  #-}

-- | The phone number of the contact in E.164 format. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPhoneNumber :: Lens.Lens' CreateContact (Core.Maybe Types.RawPhoneNumber)
ccPhoneNumber = Lens.field @"phoneNumber"
{-# INLINEABLE ccPhoneNumber #-}
{-# DEPRECATED phoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead"  #-}

-- | The list of phone numbers for the contact.
--
-- /Note:/ Consider using 'phoneNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPhoneNumbers :: Lens.Lens' CreateContact (Core.Maybe [Types.PhoneNumber])
ccPhoneNumbers = Lens.field @"phoneNumbers"
{-# INLINEABLE ccPhoneNumbers #-}
{-# DEPRECATED phoneNumbers "Use generic-lens or generic-optics with 'phoneNumbers' instead"  #-}

-- | The list of SIP addresses for the contact.
--
-- /Note:/ Consider using 'sipAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSipAddresses :: Lens.Lens' CreateContact (Core.Maybe [Types.SipAddress])
ccSipAddresses = Lens.field @"sipAddresses"
{-# INLINEABLE ccSipAddresses #-}
{-# DEPRECATED sipAddresses "Use generic-lens or generic-optics with 'sipAddresses' instead"  #-}

instance Core.ToQuery CreateContact where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateContact where
        toHeaders CreateContact{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateContact")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateContact where
        toJSON CreateContact{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FirstName" Core..= firstName),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("DisplayName" Core..=) Core.<$> displayName,
                  ("LastName" Core..=) Core.<$> lastName,
                  ("PhoneNumber" Core..=) Core.<$> phoneNumber,
                  ("PhoneNumbers" Core..=) Core.<$> phoneNumbers,
                  ("SipAddresses" Core..=) Core.<$> sipAddresses])

instance Core.AWSRequest CreateContact where
        type Rs CreateContact = CreateContactResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateContactResponse' Core.<$>
                   (x Core..:? "ContactArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateContactResponse' smart constructor.
data CreateContactResponse = CreateContactResponse'
  { contactArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the newly created address book.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContactResponse' value with any optional fields omitted.
mkCreateContactResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateContactResponse
mkCreateContactResponse responseStatus
  = CreateContactResponse'{contactArn = Core.Nothing, responseStatus}

-- | The ARN of the newly created address book.
--
-- /Note:/ Consider using 'contactArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsContactArn :: Lens.Lens' CreateContactResponse (Core.Maybe Types.Arn)
ccrrsContactArn = Lens.field @"contactArn"
{-# INLINEABLE ccrrsContactArn #-}
{-# DEPRECATED contactArn "Use generic-lens or generic-optics with 'contactArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateContactResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
