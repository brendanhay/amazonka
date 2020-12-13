{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the contact details by the contact ARN.
module Network.AWS.AlexaBusiness.UpdateContact
  ( -- * Creating a request
    UpdateContact (..),
    mkUpdateContact,

    -- ** Request lenses
    ucLastName,
    ucContactARN,
    ucPhoneNumbers,
    ucPhoneNumber,
    ucSipAddresses,
    ucFirstName,
    ucDisplayName,

    -- * Destructuring the response
    UpdateContactResponse (..),
    mkUpdateContactResponse,

    -- ** Response lenses
    ucrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateContact' smart constructor.
data UpdateContact = UpdateContact'
  { -- | The updated last name of the contact.
    lastName :: Lude.Maybe Lude.Text,
    -- | The ARN of the contact to update.
    contactARN :: Lude.Text,
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Lude.Maybe [PhoneNumber],
    -- | The updated phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
    phoneNumber :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Lude.Maybe [SipAddress],
    -- | The updated first name of the contact.
    firstName :: Lude.Maybe Lude.Text,
    -- | The updated display name of the contact.
    displayName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContact' with the minimum fields required to make a request.
--
-- * 'lastName' - The updated last name of the contact.
-- * 'contactARN' - The ARN of the contact to update.
-- * 'phoneNumbers' - The list of phone numbers for the contact.
-- * 'phoneNumber' - The updated phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
-- * 'sipAddresses' - The list of SIP addresses for the contact.
-- * 'firstName' - The updated first name of the contact.
-- * 'displayName' - The updated display name of the contact.
mkUpdateContact ::
  -- | 'contactARN'
  Lude.Text ->
  UpdateContact
mkUpdateContact pContactARN_ =
  UpdateContact'
    { lastName = Lude.Nothing,
      contactARN = pContactARN_,
      phoneNumbers = Lude.Nothing,
      phoneNumber = Lude.Nothing,
      sipAddresses = Lude.Nothing,
      firstName = Lude.Nothing,
      displayName = Lude.Nothing
    }

-- | The updated last name of the contact.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucLastName :: Lens.Lens' UpdateContact (Lude.Maybe Lude.Text)
ucLastName = Lens.lens (lastName :: UpdateContact -> Lude.Maybe Lude.Text) (\s a -> s {lastName = a} :: UpdateContact)
{-# DEPRECATED ucLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The ARN of the contact to update.
--
-- /Note:/ Consider using 'contactARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucContactARN :: Lens.Lens' UpdateContact Lude.Text
ucContactARN = Lens.lens (contactARN :: UpdateContact -> Lude.Text) (\s a -> s {contactARN = a} :: UpdateContact)
{-# DEPRECATED ucContactARN "Use generic-lens or generic-optics with 'contactARN' instead." #-}

-- | The list of phone numbers for the contact.
--
-- /Note:/ Consider using 'phoneNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucPhoneNumbers :: Lens.Lens' UpdateContact (Lude.Maybe [PhoneNumber])
ucPhoneNumbers = Lens.lens (phoneNumbers :: UpdateContact -> Lude.Maybe [PhoneNumber]) (\s a -> s {phoneNumbers = a} :: UpdateContact)
{-# DEPRECATED ucPhoneNumbers "Use generic-lens or generic-optics with 'phoneNumbers' instead." #-}

-- | The updated phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucPhoneNumber :: Lens.Lens' UpdateContact (Lude.Maybe (Lude.Sensitive Lude.Text))
ucPhoneNumber = Lens.lens (phoneNumber :: UpdateContact -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {phoneNumber = a} :: UpdateContact)
{-# DEPRECATED ucPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The list of SIP addresses for the contact.
--
-- /Note:/ Consider using 'sipAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucSipAddresses :: Lens.Lens' UpdateContact (Lude.Maybe [SipAddress])
ucSipAddresses = Lens.lens (sipAddresses :: UpdateContact -> Lude.Maybe [SipAddress]) (\s a -> s {sipAddresses = a} :: UpdateContact)
{-# DEPRECATED ucSipAddresses "Use generic-lens or generic-optics with 'sipAddresses' instead." #-}

-- | The updated first name of the contact.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucFirstName :: Lens.Lens' UpdateContact (Lude.Maybe Lude.Text)
ucFirstName = Lens.lens (firstName :: UpdateContact -> Lude.Maybe Lude.Text) (\s a -> s {firstName = a} :: UpdateContact)
{-# DEPRECATED ucFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The updated display name of the contact.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDisplayName :: Lens.Lens' UpdateContact (Lude.Maybe Lude.Text)
ucDisplayName = Lens.lens (displayName :: UpdateContact -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: UpdateContact)
{-# DEPRECATED ucDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Lude.AWSRequest UpdateContact where
  type Rs UpdateContact = UpdateContactResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateContactResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateContact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateContact" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateContact where
  toJSON UpdateContact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LastName" Lude..=) Lude.<$> lastName,
            Lude.Just ("ContactArn" Lude..= contactARN),
            ("PhoneNumbers" Lude..=) Lude.<$> phoneNumbers,
            ("PhoneNumber" Lude..=) Lude.<$> phoneNumber,
            ("SipAddresses" Lude..=) Lude.<$> sipAddresses,
            ("FirstName" Lude..=) Lude.<$> firstName,
            ("DisplayName" Lude..=) Lude.<$> displayName
          ]
      )

instance Lude.ToPath UpdateContact where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateContact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateContactResponse' smart constructor.
newtype UpdateContactResponse = UpdateContactResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContactResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateContactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateContactResponse
mkUpdateContactResponse pResponseStatus_ =
  UpdateContactResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateContactResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateContactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateContactResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
