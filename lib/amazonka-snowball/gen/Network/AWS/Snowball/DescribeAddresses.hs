{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a specified number of @ADDRESS@ objects. Calling this API in one of the US regions will return addresses from the list of all addresses associated with this account in all US regions.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.DescribeAddresses
  ( -- * Creating a request
    DescribeAddresses (..),
    mkDescribeAddresses,

    -- ** Request lenses
    daNextToken,
    daMaxResults,

    -- * Destructuring the response
    DescribeAddressesResponse (..),
    mkDescribeAddressesResponse,

    -- ** Response lenses
    darsAddresses,
    darsNextToken,
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkDescribeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { -- | HTTP requests are stateless. To identify what object comes "next" in the list of @ADDRESS@ objects, you have the option of specifying a value for @NextToken@ as the starting point for your list of returned addresses.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of @ADDRESS@ objects to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAddresses' with the minimum fields required to make a request.
--
-- * 'nextToken' - HTTP requests are stateless. To identify what object comes "next" in the list of @ADDRESS@ objects, you have the option of specifying a value for @NextToken@ as the starting point for your list of returned addresses.
-- * 'maxResults' - The number of @ADDRESS@ objects to return.
mkDescribeAddresses ::
  DescribeAddresses
mkDescribeAddresses =
  DescribeAddresses'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes "next" in the list of @ADDRESS@ objects, you have the option of specifying a value for @NextToken@ as the starting point for your list of returned addresses.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daNextToken :: Lens.Lens' DescribeAddresses (Lude.Maybe Lude.Text)
daNextToken = Lens.lens (nextToken :: DescribeAddresses -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAddresses)
{-# DEPRECATED daNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of @ADDRESS@ objects to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMaxResults :: Lens.Lens' DescribeAddresses (Lude.Maybe Lude.Natural)
daMaxResults = Lens.lens (maxResults :: DescribeAddresses -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAddresses)
{-# DEPRECATED daMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeAddresses where
  page rq rs
    | Page.stop (rs Lens.^. darsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. darsAddresses) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daNextToken Lens..~ rs Lens.^. darsNextToken

instance Lude.AWSRequest DescribeAddresses where
  type Rs DescribeAddresses = DescribeAddressesResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAddressesResponse'
            Lude.<$> (x Lude..?> "Addresses" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAddresses where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.DescribeAddresses" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAddresses where
  toJSON DescribeAddresses' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeAddresses where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAddresses where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { -- | The Snow device shipping addresses that were created for this account.
    addresses :: Lude.Maybe [Address],
    -- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @DescribeAddresses@ call, your list of returned addresses will start from this point in the array.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAddressesResponse' with the minimum fields required to make a request.
--
-- * 'addresses' - The Snow device shipping addresses that were created for this account.
-- * 'nextToken' - HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @DescribeAddresses@ call, your list of returned addresses will start from this point in the array.
-- * 'responseStatus' - The response status code.
mkDescribeAddressesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAddressesResponse
mkDescribeAddressesResponse pResponseStatus_ =
  DescribeAddressesResponse'
    { addresses = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Snow device shipping addresses that were created for this account.
--
-- /Note:/ Consider using 'addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAddresses :: Lens.Lens' DescribeAddressesResponse (Lude.Maybe [Address])
darsAddresses = Lens.lens (addresses :: DescribeAddressesResponse -> Lude.Maybe [Address]) (\s a -> s {addresses = a} :: DescribeAddressesResponse)
{-# DEPRECATED darsAddresses "Use generic-lens or generic-optics with 'addresses' instead." #-}

-- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @DescribeAddresses@ call, your list of returned addresses will start from this point in the array.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsNextToken :: Lens.Lens' DescribeAddressesResponse (Lude.Maybe Lude.Text)
darsNextToken = Lens.lens (nextToken :: DescribeAddressesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAddressesResponse)
{-# DEPRECATED darsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAddressesResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAddressesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAddressesResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
