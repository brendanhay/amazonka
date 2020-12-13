{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an address book with the specified details.
module Network.AWS.AlexaBusiness.CreateAddressBook
  ( -- * Creating a request
    CreateAddressBook (..),
    mkCreateAddressBook,

    -- ** Request lenses
    cabName,
    cabClientRequestToken,
    cabDescription,

    -- * Destructuring the response
    CreateAddressBookResponse (..),
    mkCreateAddressBookResponse,

    -- ** Response lenses
    cabrsAddressBookARN,
    cabrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAddressBook' smart constructor.
data CreateAddressBook = CreateAddressBook'
  { -- | The name of the address book.
    name :: Lude.Text,
    -- | A unique, user-specified identifier for the request that ensures idempotency.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | The description of the address book.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAddressBook' with the minimum fields required to make a request.
--
-- * 'name' - The name of the address book.
-- * 'clientRequestToken' - A unique, user-specified identifier for the request that ensures idempotency.
-- * 'description' - The description of the address book.
mkCreateAddressBook ::
  -- | 'name'
  Lude.Text ->
  CreateAddressBook
mkCreateAddressBook pName_ =
  CreateAddressBook'
    { name = pName_,
      clientRequestToken = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name of the address book.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabName :: Lens.Lens' CreateAddressBook Lude.Text
cabName = Lens.lens (name :: CreateAddressBook -> Lude.Text) (\s a -> s {name = a} :: CreateAddressBook)
{-# DEPRECATED cabName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique, user-specified identifier for the request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabClientRequestToken :: Lens.Lens' CreateAddressBook (Lude.Maybe Lude.Text)
cabClientRequestToken = Lens.lens (clientRequestToken :: CreateAddressBook -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateAddressBook)
{-# DEPRECATED cabClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The description of the address book.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabDescription :: Lens.Lens' CreateAddressBook (Lude.Maybe Lude.Text)
cabDescription = Lens.lens (description :: CreateAddressBook -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateAddressBook)
{-# DEPRECATED cabDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateAddressBook where
  type Rs CreateAddressBook = CreateAddressBookResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAddressBookResponse'
            Lude.<$> (x Lude..?> "AddressBookArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAddressBook where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.CreateAddressBook" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAddressBook where
  toJSON CreateAddressBook' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateAddressBook where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAddressBook where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAddressBookResponse' smart constructor.
data CreateAddressBookResponse = CreateAddressBookResponse'
  { -- | The ARN of the newly created address book.
    addressBookARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAddressBookResponse' with the minimum fields required to make a request.
--
-- * 'addressBookARN' - The ARN of the newly created address book.
-- * 'responseStatus' - The response status code.
mkCreateAddressBookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAddressBookResponse
mkCreateAddressBookResponse pResponseStatus_ =
  CreateAddressBookResponse'
    { addressBookARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the newly created address book.
--
-- /Note:/ Consider using 'addressBookARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrsAddressBookARN :: Lens.Lens' CreateAddressBookResponse (Lude.Maybe Lude.Text)
cabrsAddressBookARN = Lens.lens (addressBookARN :: CreateAddressBookResponse -> Lude.Maybe Lude.Text) (\s a -> s {addressBookARN = a} :: CreateAddressBookResponse)
{-# DEPRECATED cabrsAddressBookARN "Use generic-lens or generic-optics with 'addressBookARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrsResponseStatus :: Lens.Lens' CreateAddressBookResponse Lude.Int
cabrsResponseStatus = Lens.lens (responseStatus :: CreateAddressBookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAddressBookResponse)
{-# DEPRECATED cabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
