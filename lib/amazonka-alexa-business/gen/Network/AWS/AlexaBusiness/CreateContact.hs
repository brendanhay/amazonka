{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateContact (..),
    mkCreateContact,

    -- ** Request lenses
    ccLastName,
    ccPhoneNumbers,
    ccPhoneNumber,
    ccSipAddresses,
    ccDisplayName,
    ccClientRequestToken,
    ccFirstName,

    -- * Destructuring the response
    CreateContactResponse (..),
    mkCreateContactResponse,

    -- ** Response lenses
    ccrsContactARN,
    ccrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateContact' smart constructor.
data CreateContact = CreateContact'
  { lastName ::
      Lude.Maybe Lude.Text,
    phoneNumbers :: Lude.Maybe [PhoneNumber],
    phoneNumber :: Lude.Maybe (Lude.Sensitive Lude.Text),
    sipAddresses :: Lude.Maybe [SipAddress],
    displayName :: Lude.Maybe Lude.Text,
    clientRequestToken :: Lude.Maybe Lude.Text,
    firstName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContact' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique, user-specified identifier for this request that ensures idempotency.
-- * 'displayName' - The name of the contact to display on the console.
-- * 'firstName' - The first name of the contact that is used to call the contact on the device.
-- * 'lastName' - The last name of the contact that is used to call the contact on the device.
-- * 'phoneNumber' - The phone number of the contact in E.164 format. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
-- * 'phoneNumbers' - The list of phone numbers for the contact.
-- * 'sipAddresses' - The list of SIP addresses for the contact.
mkCreateContact ::
  -- | 'firstName'
  Lude.Text ->
  CreateContact
mkCreateContact pFirstName_ =
  CreateContact'
    { lastName = Lude.Nothing,
      phoneNumbers = Lude.Nothing,
      phoneNumber = Lude.Nothing,
      sipAddresses = Lude.Nothing,
      displayName = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      firstName = pFirstName_
    }

-- | The last name of the contact that is used to call the contact on the device.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLastName :: Lens.Lens' CreateContact (Lude.Maybe Lude.Text)
ccLastName = Lens.lens (lastName :: CreateContact -> Lude.Maybe Lude.Text) (\s a -> s {lastName = a} :: CreateContact)
{-# DEPRECATED ccLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The list of phone numbers for the contact.
--
-- /Note:/ Consider using 'phoneNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPhoneNumbers :: Lens.Lens' CreateContact (Lude.Maybe [PhoneNumber])
ccPhoneNumbers = Lens.lens (phoneNumbers :: CreateContact -> Lude.Maybe [PhoneNumber]) (\s a -> s {phoneNumbers = a} :: CreateContact)
{-# DEPRECATED ccPhoneNumbers "Use generic-lens or generic-optics with 'phoneNumbers' instead." #-}

-- | The phone number of the contact in E.164 format. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPhoneNumber :: Lens.Lens' CreateContact (Lude.Maybe (Lude.Sensitive Lude.Text))
ccPhoneNumber = Lens.lens (phoneNumber :: CreateContact -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {phoneNumber = a} :: CreateContact)
{-# DEPRECATED ccPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The list of SIP addresses for the contact.
--
-- /Note:/ Consider using 'sipAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSipAddresses :: Lens.Lens' CreateContact (Lude.Maybe [SipAddress])
ccSipAddresses = Lens.lens (sipAddresses :: CreateContact -> Lude.Maybe [SipAddress]) (\s a -> s {sipAddresses = a} :: CreateContact)
{-# DEPRECATED ccSipAddresses "Use generic-lens or generic-optics with 'sipAddresses' instead." #-}

-- | The name of the contact to display on the console.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDisplayName :: Lens.Lens' CreateContact (Lude.Maybe Lude.Text)
ccDisplayName = Lens.lens (displayName :: CreateContact -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: CreateContact)
{-# DEPRECATED ccDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | A unique, user-specified identifier for this request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClientRequestToken :: Lens.Lens' CreateContact (Lude.Maybe Lude.Text)
ccClientRequestToken = Lens.lens (clientRequestToken :: CreateContact -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateContact)
{-# DEPRECATED ccClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The first name of the contact that is used to call the contact on the device.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccFirstName :: Lens.Lens' CreateContact Lude.Text
ccFirstName = Lens.lens (firstName :: CreateContact -> Lude.Text) (\s a -> s {firstName = a} :: CreateContact)
{-# DEPRECATED ccFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

instance Lude.AWSRequest CreateContact where
  type Rs CreateContact = CreateContactResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateContactResponse'
            Lude.<$> (x Lude..?> "ContactArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateContact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.CreateContact" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateContact where
  toJSON CreateContact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LastName" Lude..=) Lude.<$> lastName,
            ("PhoneNumbers" Lude..=) Lude.<$> phoneNumbers,
            ("PhoneNumber" Lude..=) Lude.<$> phoneNumber,
            ("SipAddresses" Lude..=) Lude.<$> sipAddresses,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("FirstName" Lude..= firstName)
          ]
      )

instance Lude.ToPath CreateContact where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateContact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateContactResponse' smart constructor.
data CreateContactResponse = CreateContactResponse'
  { contactARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContactResponse' with the minimum fields required to make a request.
--
-- * 'contactARN' - The ARN of the newly created address book.
-- * 'responseStatus' - The response status code.
mkCreateContactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateContactResponse
mkCreateContactResponse pResponseStatus_ =
  CreateContactResponse'
    { contactARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the newly created address book.
--
-- /Note:/ Consider using 'contactARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsContactARN :: Lens.Lens' CreateContactResponse (Lude.Maybe Lude.Text)
ccrsContactARN = Lens.lens (contactARN :: CreateContactResponse -> Lude.Maybe Lude.Text) (\s a -> s {contactARN = a} :: CreateContactResponse)
{-# DEPRECATED ccrsContactARN "Use generic-lens or generic-optics with 'contactARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateContactResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateContactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateContactResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
