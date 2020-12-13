{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an address for a Snow device to be shipped to. In most regions, addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. If the address is invalid or unsupported, then an exception is thrown.
module Network.AWS.Snowball.CreateAddress
  ( -- * Creating a request
    CreateAddress (..),
    mkCreateAddress,

    -- ** Request lenses
    caAddress,

    -- * Destructuring the response
    CreateAddressResponse (..),
    mkCreateAddressResponse,

    -- ** Response lenses
    carsAddressId,
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkCreateAddress' smart constructor.
newtype CreateAddress = CreateAddress'
  { -- | The address that you want the Snow device shipped to.
    address :: Address
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAddress' with the minimum fields required to make a request.
--
-- * 'address' - The address that you want the Snow device shipped to.
mkCreateAddress ::
  -- | 'address'
  Address ->
  CreateAddress
mkCreateAddress pAddress_ = CreateAddress' {address = pAddress_}

-- | The address that you want the Snow device shipped to.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAddress :: Lens.Lens' CreateAddress Address
caAddress = Lens.lens (address :: CreateAddress -> Address) (\s a -> s {address = a} :: CreateAddress)
{-# DEPRECATED caAddress "Use generic-lens or generic-optics with 'address' instead." #-}

instance Lude.AWSRequest CreateAddress where
  type Rs CreateAddress = CreateAddressResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAddressResponse'
            Lude.<$> (x Lude..?> "AddressId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAddress where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.CreateAddress" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAddress where
  toJSON CreateAddress' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Address" Lude..= address)])

instance Lude.ToPath CreateAddress where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAddress where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAddressResponse' smart constructor.
data CreateAddressResponse = CreateAddressResponse'
  { -- | The automatically generated ID for a specific address. You'll use this ID when you create a job to specify which address you want the Snow device for that job shipped to.
    addressId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAddressResponse' with the minimum fields required to make a request.
--
-- * 'addressId' - The automatically generated ID for a specific address. You'll use this ID when you create a job to specify which address you want the Snow device for that job shipped to.
-- * 'responseStatus' - The response status code.
mkCreateAddressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAddressResponse
mkCreateAddressResponse pResponseStatus_ =
  CreateAddressResponse'
    { addressId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The automatically generated ID for a specific address. You'll use this ID when you create a job to specify which address you want the Snow device for that job shipped to.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAddressId :: Lens.Lens' CreateAddressResponse (Lude.Maybe Lude.Text)
carsAddressId = Lens.lens (addressId :: CreateAddressResponse -> Lude.Maybe Lude.Text) (\s a -> s {addressId = a} :: CreateAddressResponse)
{-# DEPRECATED carsAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAddressResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAddressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAddressResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
