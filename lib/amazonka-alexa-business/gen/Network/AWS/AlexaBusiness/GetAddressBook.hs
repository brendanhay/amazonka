{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets address the book details by the address book ARN.
module Network.AWS.AlexaBusiness.GetAddressBook
  ( -- * Creating a request
    GetAddressBook (..),
    mkGetAddressBook,

    -- ** Request lenses
    gabAddressBookARN,

    -- * Destructuring the response
    GetAddressBookResponse (..),
    mkGetAddressBookResponse,

    -- ** Response lenses
    gabrsAddressBook,
    gabrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAddressBook' smart constructor.
newtype GetAddressBook = GetAddressBook'
  { addressBookARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAddressBook' with the minimum fields required to make a request.
--
-- * 'addressBookARN' - The ARN of the address book for which to request details.
mkGetAddressBook ::
  -- | 'addressBookARN'
  Lude.Text ->
  GetAddressBook
mkGetAddressBook pAddressBookARN_ =
  GetAddressBook' {addressBookARN = pAddressBookARN_}

-- | The ARN of the address book for which to request details.
--
-- /Note:/ Consider using 'addressBookARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabAddressBookARN :: Lens.Lens' GetAddressBook Lude.Text
gabAddressBookARN = Lens.lens (addressBookARN :: GetAddressBook -> Lude.Text) (\s a -> s {addressBookARN = a} :: GetAddressBook)
{-# DEPRECATED gabAddressBookARN "Use generic-lens or generic-optics with 'addressBookARN' instead." #-}

instance Lude.AWSRequest GetAddressBook where
  type Rs GetAddressBook = GetAddressBookResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAddressBookResponse'
            Lude.<$> (x Lude..?> "AddressBook") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAddressBook where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetAddressBook" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAddressBook where
  toJSON GetAddressBook' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AddressBookArn" Lude..= addressBookARN)]
      )

instance Lude.ToPath GetAddressBook where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAddressBook where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAddressBookResponse' smart constructor.
data GetAddressBookResponse = GetAddressBookResponse'
  { addressBook ::
      Lude.Maybe AddressBook,
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

-- | Creates a value of 'GetAddressBookResponse' with the minimum fields required to make a request.
--
-- * 'addressBook' - The details of the requested address book.
-- * 'responseStatus' - The response status code.
mkGetAddressBookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAddressBookResponse
mkGetAddressBookResponse pResponseStatus_ =
  GetAddressBookResponse'
    { addressBook = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the requested address book.
--
-- /Note:/ Consider using 'addressBook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabrsAddressBook :: Lens.Lens' GetAddressBookResponse (Lude.Maybe AddressBook)
gabrsAddressBook = Lens.lens (addressBook :: GetAddressBookResponse -> Lude.Maybe AddressBook) (\s a -> s {addressBook = a} :: GetAddressBookResponse)
{-# DEPRECATED gabrsAddressBook "Use generic-lens or generic-optics with 'addressBook' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabrsResponseStatus :: Lens.Lens' GetAddressBookResponse Lude.Int
gabrsResponseStatus = Lens.lens (responseStatus :: GetAddressBookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAddressBookResponse)
{-# DEPRECATED gabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
