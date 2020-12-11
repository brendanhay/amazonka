{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a contact with a given address book.
module Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
  ( -- * Creating a request
    AssociateContactWithAddressBook (..),
    mkAssociateContactWithAddressBook,

    -- ** Request lenses
    acwabContactARN,
    acwabAddressBookARN,

    -- * Destructuring the response
    AssociateContactWithAddressBookResponse (..),
    mkAssociateContactWithAddressBookResponse,

    -- ** Response lenses
    acwabrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateContactWithAddressBook' smart constructor.
data AssociateContactWithAddressBook = AssociateContactWithAddressBook'
  { contactARN ::
      Lude.Text,
    addressBookARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateContactWithAddressBook' with the minimum fields required to make a request.
--
-- * 'addressBookARN' - The ARN of the address book with which to associate the contact.
-- * 'contactARN' - The ARN of the contact to associate with an address book.
mkAssociateContactWithAddressBook ::
  -- | 'contactARN'
  Lude.Text ->
  -- | 'addressBookARN'
  Lude.Text ->
  AssociateContactWithAddressBook
mkAssociateContactWithAddressBook pContactARN_ pAddressBookARN_ =
  AssociateContactWithAddressBook'
    { contactARN = pContactARN_,
      addressBookARN = pAddressBookARN_
    }

-- | The ARN of the contact to associate with an address book.
--
-- /Note:/ Consider using 'contactARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwabContactARN :: Lens.Lens' AssociateContactWithAddressBook Lude.Text
acwabContactARN = Lens.lens (contactARN :: AssociateContactWithAddressBook -> Lude.Text) (\s a -> s {contactARN = a} :: AssociateContactWithAddressBook)
{-# DEPRECATED acwabContactARN "Use generic-lens or generic-optics with 'contactARN' instead." #-}

-- | The ARN of the address book with which to associate the contact.
--
-- /Note:/ Consider using 'addressBookARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwabAddressBookARN :: Lens.Lens' AssociateContactWithAddressBook Lude.Text
acwabAddressBookARN = Lens.lens (addressBookARN :: AssociateContactWithAddressBook -> Lude.Text) (\s a -> s {addressBookARN = a} :: AssociateContactWithAddressBook)
{-# DEPRECATED acwabAddressBookARN "Use generic-lens or generic-optics with 'addressBookARN' instead." #-}

instance Lude.AWSRequest AssociateContactWithAddressBook where
  type
    Rs AssociateContactWithAddressBook =
      AssociateContactWithAddressBookResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateContactWithAddressBookResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateContactWithAddressBook where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.AssociateContactWithAddressBook" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateContactWithAddressBook where
  toJSON AssociateContactWithAddressBook' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ContactArn" Lude..= contactARN),
            Lude.Just ("AddressBookArn" Lude..= addressBookARN)
          ]
      )

instance Lude.ToPath AssociateContactWithAddressBook where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateContactWithAddressBook where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateContactWithAddressBookResponse' smart constructor.
newtype AssociateContactWithAddressBookResponse = AssociateContactWithAddressBookResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateContactWithAddressBookResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateContactWithAddressBookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateContactWithAddressBookResponse
mkAssociateContactWithAddressBookResponse pResponseStatus_ =
  AssociateContactWithAddressBookResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwabrsResponseStatus :: Lens.Lens' AssociateContactWithAddressBookResponse Lude.Int
acwabrsResponseStatus = Lens.lens (responseStatus :: AssociateContactWithAddressBookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateContactWithAddressBookResponse)
{-# DEPRECATED acwabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
