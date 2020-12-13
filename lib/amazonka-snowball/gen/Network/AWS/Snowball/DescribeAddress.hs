{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Takes an @AddressId@ and returns specific details about that address in the form of an @Address@ object.
module Network.AWS.Snowball.DescribeAddress
  ( -- * Creating a request
    DescribeAddress (..),
    mkDescribeAddress,

    -- ** Request lenses
    daAddressId,

    -- * Destructuring the response
    DescribeAddressResponse (..),
    mkDescribeAddressResponse,

    -- ** Response lenses
    drsAddress,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkDescribeAddress' smart constructor.
newtype DescribeAddress = DescribeAddress'
  { -- | The automatically generated ID for a specific address.
    addressId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAddress' with the minimum fields required to make a request.
--
-- * 'addressId' - The automatically generated ID for a specific address.
mkDescribeAddress ::
  -- | 'addressId'
  Lude.Text ->
  DescribeAddress
mkDescribeAddress pAddressId_ =
  DescribeAddress' {addressId = pAddressId_}

-- | The automatically generated ID for a specific address.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAddressId :: Lens.Lens' DescribeAddress Lude.Text
daAddressId = Lens.lens (addressId :: DescribeAddress -> Lude.Text) (\s a -> s {addressId = a} :: DescribeAddress)
{-# DEPRECATED daAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

instance Lude.AWSRequest DescribeAddress where
  type Rs DescribeAddress = DescribeAddressResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAddressResponse'
            Lude.<$> (x Lude..?> "Address") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAddress where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.DescribeAddress" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAddress where
  toJSON DescribeAddress' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AddressId" Lude..= addressId)])

instance Lude.ToPath DescribeAddress where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAddress where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAddressResponse' smart constructor.
data DescribeAddressResponse = DescribeAddressResponse'
  { -- | The address that you want the Snow device(s) associated with a specific job to be shipped to.
    address :: Lude.Maybe Address,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAddressResponse' with the minimum fields required to make a request.
--
-- * 'address' - The address that you want the Snow device(s) associated with a specific job to be shipped to.
-- * 'responseStatus' - The response status code.
mkDescribeAddressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAddressResponse
mkDescribeAddressResponse pResponseStatus_ =
  DescribeAddressResponse'
    { address = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The address that you want the Snow device(s) associated with a specific job to be shipped to.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAddress :: Lens.Lens' DescribeAddressResponse (Lude.Maybe Address)
drsAddress = Lens.lens (address :: DescribeAddressResponse -> Lude.Maybe Address) (\s a -> s {address = a} :: DescribeAddressResponse)
{-# DEPRECATED drsAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeAddressResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeAddressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAddressResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
