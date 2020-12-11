{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a contact from a given address book.
module Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
  ( -- * Creating a request
    DisassociateContactFromAddressBook (..),
    mkDisassociateContactFromAddressBook,

    -- ** Request lenses
    dcfabContactARN,
    dcfabAddressBookARN,

    -- * Destructuring the response
    DisassociateContactFromAddressBookResponse (..),
    mkDisassociateContactFromAddressBookResponse,

    -- ** Response lenses
    dcfabrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateContactFromAddressBook' smart constructor.
data DisassociateContactFromAddressBook = DisassociateContactFromAddressBook'
  { contactARN ::
      Lude.Text,
    addressBookARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateContactFromAddressBook' with the minimum fields required to make a request.
--
-- * 'addressBookARN' - The ARN of the address from which to disassociate the contact.
-- * 'contactARN' - The ARN of the contact to disassociate from an address book.
mkDisassociateContactFromAddressBook ::
  -- | 'contactARN'
  Lude.Text ->
  -- | 'addressBookARN'
  Lude.Text ->
  DisassociateContactFromAddressBook
mkDisassociateContactFromAddressBook pContactARN_ pAddressBookARN_ =
  DisassociateContactFromAddressBook'
    { contactARN = pContactARN_,
      addressBookARN = pAddressBookARN_
    }

-- | The ARN of the contact to disassociate from an address book.
--
-- /Note:/ Consider using 'contactARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfabContactARN :: Lens.Lens' DisassociateContactFromAddressBook Lude.Text
dcfabContactARN = Lens.lens (contactARN :: DisassociateContactFromAddressBook -> Lude.Text) (\s a -> s {contactARN = a} :: DisassociateContactFromAddressBook)
{-# DEPRECATED dcfabContactARN "Use generic-lens or generic-optics with 'contactARN' instead." #-}

-- | The ARN of the address from which to disassociate the contact.
--
-- /Note:/ Consider using 'addressBookARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfabAddressBookARN :: Lens.Lens' DisassociateContactFromAddressBook Lude.Text
dcfabAddressBookARN = Lens.lens (addressBookARN :: DisassociateContactFromAddressBook -> Lude.Text) (\s a -> s {addressBookARN = a} :: DisassociateContactFromAddressBook)
{-# DEPRECATED dcfabAddressBookARN "Use generic-lens or generic-optics with 'addressBookARN' instead." #-}

instance Lude.AWSRequest DisassociateContactFromAddressBook where
  type
    Rs DisassociateContactFromAddressBook =
      DisassociateContactFromAddressBookResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateContactFromAddressBookResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateContactFromAddressBook where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.DisassociateContactFromAddressBook" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateContactFromAddressBook where
  toJSON DisassociateContactFromAddressBook' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ContactArn" Lude..= contactARN),
            Lude.Just ("AddressBookArn" Lude..= addressBookARN)
          ]
      )

instance Lude.ToPath DisassociateContactFromAddressBook where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateContactFromAddressBook where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateContactFromAddressBookResponse' smart constructor.
newtype DisassociateContactFromAddressBookResponse = DisassociateContactFromAddressBookResponse'
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

-- | Creates a value of 'DisassociateContactFromAddressBookResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateContactFromAddressBookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateContactFromAddressBookResponse
mkDisassociateContactFromAddressBookResponse pResponseStatus_ =
  DisassociateContactFromAddressBookResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfabrsResponseStatus :: Lens.Lens' DisassociateContactFromAddressBookResponse Lude.Int
dcfabrsResponseStatus = Lens.lens (responseStatus :: DisassociateContactFromAddressBookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateContactFromAddressBookResponse)
{-# DEPRECATED dcfabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
