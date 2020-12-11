{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an address book by the address book ARN.
module Network.AWS.AlexaBusiness.DeleteAddressBook
  ( -- * Creating a request
    DeleteAddressBook (..),
    mkDeleteAddressBook,

    -- ** Request lenses
    dabAddressBookARN,

    -- * Destructuring the response
    DeleteAddressBookResponse (..),
    mkDeleteAddressBookResponse,

    -- ** Response lenses
    dabrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAddressBook' smart constructor.
newtype DeleteAddressBook = DeleteAddressBook'
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

-- | Creates a value of 'DeleteAddressBook' with the minimum fields required to make a request.
--
-- * 'addressBookARN' - The ARN of the address book to delete.
mkDeleteAddressBook ::
  -- | 'addressBookARN'
  Lude.Text ->
  DeleteAddressBook
mkDeleteAddressBook pAddressBookARN_ =
  DeleteAddressBook' {addressBookARN = pAddressBookARN_}

-- | The ARN of the address book to delete.
--
-- /Note:/ Consider using 'addressBookARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dabAddressBookARN :: Lens.Lens' DeleteAddressBook Lude.Text
dabAddressBookARN = Lens.lens (addressBookARN :: DeleteAddressBook -> Lude.Text) (\s a -> s {addressBookARN = a} :: DeleteAddressBook)
{-# DEPRECATED dabAddressBookARN "Use generic-lens or generic-optics with 'addressBookARN' instead." #-}

instance Lude.AWSRequest DeleteAddressBook where
  type Rs DeleteAddressBook = DeleteAddressBookResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAddressBookResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAddressBook where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteAddressBook" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAddressBook where
  toJSON DeleteAddressBook' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AddressBookArn" Lude..= addressBookARN)]
      )

instance Lude.ToPath DeleteAddressBook where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAddressBook where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAddressBookResponse' smart constructor.
newtype DeleteAddressBookResponse = DeleteAddressBookResponse'
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

-- | Creates a value of 'DeleteAddressBookResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAddressBookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAddressBookResponse
mkDeleteAddressBookResponse pResponseStatus_ =
  DeleteAddressBookResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dabrsResponseStatus :: Lens.Lens' DeleteAddressBookResponse Lude.Int
dabrsResponseStatus = Lens.lens (responseStatus :: DeleteAddressBookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAddressBookResponse)
{-# DEPRECATED dabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
