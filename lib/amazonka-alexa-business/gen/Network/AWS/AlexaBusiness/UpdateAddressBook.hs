{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates address book details by the address book ARN.
module Network.AWS.AlexaBusiness.UpdateAddressBook
  ( -- * Creating a request
    UpdateAddressBook (..),
    mkUpdateAddressBook,

    -- ** Request lenses
    uabName,
    uabDescription,
    uabAddressBookARN,

    -- * Destructuring the response
    UpdateAddressBookResponse (..),
    mkUpdateAddressBookResponse,

    -- ** Response lenses
    uabrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAddressBook' smart constructor.
data UpdateAddressBook = UpdateAddressBook'
  { name ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateAddressBook' with the minimum fields required to make a request.
--
-- * 'addressBookARN' - The ARN of the room to update.
-- * 'description' - The updated description of the room.
-- * 'name' - The updated name of the room.
mkUpdateAddressBook ::
  -- | 'addressBookARN'
  Lude.Text ->
  UpdateAddressBook
mkUpdateAddressBook pAddressBookARN_ =
  UpdateAddressBook'
    { name = Lude.Nothing,
      description = Lude.Nothing,
      addressBookARN = pAddressBookARN_
    }

-- | The updated name of the room.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabName :: Lens.Lens' UpdateAddressBook (Lude.Maybe Lude.Text)
uabName = Lens.lens (name :: UpdateAddressBook -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateAddressBook)
{-# DEPRECATED uabName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The updated description of the room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabDescription :: Lens.Lens' UpdateAddressBook (Lude.Maybe Lude.Text)
uabDescription = Lens.lens (description :: UpdateAddressBook -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateAddressBook)
{-# DEPRECATED uabDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the room to update.
--
-- /Note:/ Consider using 'addressBookARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabAddressBookARN :: Lens.Lens' UpdateAddressBook Lude.Text
uabAddressBookARN = Lens.lens (addressBookARN :: UpdateAddressBook -> Lude.Text) (\s a -> s {addressBookARN = a} :: UpdateAddressBook)
{-# DEPRECATED uabAddressBookARN "Use generic-lens or generic-optics with 'addressBookARN' instead." #-}

instance Lude.AWSRequest UpdateAddressBook where
  type Rs UpdateAddressBook = UpdateAddressBookResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateAddressBookResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAddressBook where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateAddressBook" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAddressBook where
  toJSON UpdateAddressBook' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("AddressBookArn" Lude..= addressBookARN)
          ]
      )

instance Lude.ToPath UpdateAddressBook where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAddressBook where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAddressBookResponse' smart constructor.
newtype UpdateAddressBookResponse = UpdateAddressBookResponse'
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

-- | Creates a value of 'UpdateAddressBookResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateAddressBookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAddressBookResponse
mkUpdateAddressBookResponse pResponseStatus_ =
  UpdateAddressBookResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabrsResponseStatus :: Lens.Lens' UpdateAddressBookResponse Lude.Int
uabrsResponseStatus = Lens.lens (responseStatus :: UpdateAddressBookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAddressBookResponse)
{-# DEPRECATED uabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
