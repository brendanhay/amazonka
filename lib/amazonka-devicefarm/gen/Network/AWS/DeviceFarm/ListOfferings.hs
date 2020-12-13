{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of products or offerings that the user can manage through the API. Each offering record indicates the recurring price per unit and the frequency for that offering. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListOfferings
  ( -- * Creating a request
    ListOfferings (..),
    mkListOfferings,

    -- ** Request lenses
    loNextToken,

    -- * Destructuring the response
    ListOfferingsResponse (..),
    mkListOfferingsResponse,

    -- ** Response lenses
    lorsNextToken,
    lorsOfferings,
    lorsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to list all offerings.
--
-- /See:/ 'mkListOfferings' smart constructor.
newtype ListOfferings = ListOfferings'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOfferings' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListOfferings ::
  ListOfferings
mkListOfferings = ListOfferings' {nextToken = Lude.Nothing}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loNextToken :: Lens.Lens' ListOfferings (Lude.Maybe Lude.Text)
loNextToken = Lens.lens (nextToken :: ListOfferings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOfferings)
{-# DEPRECATED loNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListOfferings where
  page rq rs
    | Page.stop (rs Lens.^. lorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lorsOfferings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loNextToken Lens..~ rs Lens.^. lorsNextToken

instance Lude.AWSRequest ListOfferings where
  type Rs ListOfferings = ListOfferingsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOfferingsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "offerings" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOfferings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListOfferings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOfferings where
  toJSON ListOfferings' {..} =
    Lude.object
      (Lude.catMaybes [("nextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListOfferings where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOfferings where
  toQuery = Lude.const Lude.mempty

-- | Represents the return values of the list of offerings.
--
-- /See:/ 'mkListOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A value that represents the list offering results.
    offerings :: Lude.Maybe [Offering],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'offerings' - A value that represents the list offering results.
-- * 'responseStatus' - The response status code.
mkListOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOfferingsResponse
mkListOfferingsResponse pResponseStatus_ =
  ListOfferingsResponse'
    { nextToken = Lude.Nothing,
      offerings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsNextToken :: Lens.Lens' ListOfferingsResponse (Lude.Maybe Lude.Text)
lorsNextToken = Lens.lens (nextToken :: ListOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOfferingsResponse)
{-# DEPRECATED lorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A value that represents the list offering results.
--
-- /Note:/ Consider using 'offerings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsOfferings :: Lens.Lens' ListOfferingsResponse (Lude.Maybe [Offering])
lorsOfferings = Lens.lens (offerings :: ListOfferingsResponse -> Lude.Maybe [Offering]) (\s a -> s {offerings = a} :: ListOfferingsResponse)
{-# DEPRECATED lorsOfferings "Use generic-lens or generic-optics with 'offerings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsResponseStatus :: Lens.Lens' ListOfferingsResponse Lude.Int
lorsResponseStatus = Lens.lens (responseStatus :: ListOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOfferingsResponse)
{-# DEPRECATED lorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
