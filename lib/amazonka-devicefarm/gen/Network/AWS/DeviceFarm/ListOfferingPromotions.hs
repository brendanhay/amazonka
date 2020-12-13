{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListOfferingPromotions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of offering promotions. Each offering promotion record contains the ID and description of the promotion. The API returns a @NotEligible@ error if the caller is not permitted to invoke the operation. Contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> if you must be able to invoke this operation.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListOfferingPromotions
  ( -- * Creating a request
    ListOfferingPromotions (..),
    mkListOfferingPromotions,

    -- ** Request lenses
    lopNextToken,

    -- * Destructuring the response
    ListOfferingPromotionsResponse (..),
    mkListOfferingPromotionsResponse,

    -- ** Response lenses
    loprsNextToken,
    loprsOfferingPromotions,
    loprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListOfferingPromotions' smart constructor.
newtype ListOfferingPromotions = ListOfferingPromotions'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOfferingPromotions' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListOfferingPromotions ::
  ListOfferingPromotions
mkListOfferingPromotions =
  ListOfferingPromotions' {nextToken = Lude.Nothing}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopNextToken :: Lens.Lens' ListOfferingPromotions (Lude.Maybe Lude.Text)
lopNextToken = Lens.lens (nextToken :: ListOfferingPromotions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOfferingPromotions)
{-# DEPRECATED lopNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListOfferingPromotions where
  page rq rs
    | Page.stop (rs Lens.^. loprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. loprsOfferingPromotions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lopNextToken Lens..~ rs Lens.^. loprsNextToken

instance Lude.AWSRequest ListOfferingPromotions where
  type Rs ListOfferingPromotions = ListOfferingPromotionsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOfferingPromotionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "offeringPromotions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOfferingPromotions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListOfferingPromotions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOfferingPromotions where
  toJSON ListOfferingPromotions' {..} =
    Lude.object
      (Lude.catMaybes [("nextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListOfferingPromotions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOfferingPromotions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListOfferingPromotionsResponse' smart constructor.
data ListOfferingPromotionsResponse = ListOfferingPromotionsResponse'
  { -- | An identifier to be used in the next call to this operation, to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the offering promotions.
    offeringPromotions :: Lude.Maybe [OfferingPromotion],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOfferingPromotionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier to be used in the next call to this operation, to return the next set of items in the list.
-- * 'offeringPromotions' - Information about the offering promotions.
-- * 'responseStatus' - The response status code.
mkListOfferingPromotionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOfferingPromotionsResponse
mkListOfferingPromotionsResponse pResponseStatus_ =
  ListOfferingPromotionsResponse'
    { nextToken = Lude.Nothing,
      offeringPromotions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier to be used in the next call to this operation, to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loprsNextToken :: Lens.Lens' ListOfferingPromotionsResponse (Lude.Maybe Lude.Text)
loprsNextToken = Lens.lens (nextToken :: ListOfferingPromotionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOfferingPromotionsResponse)
{-# DEPRECATED loprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the offering promotions.
--
-- /Note:/ Consider using 'offeringPromotions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loprsOfferingPromotions :: Lens.Lens' ListOfferingPromotionsResponse (Lude.Maybe [OfferingPromotion])
loprsOfferingPromotions = Lens.lens (offeringPromotions :: ListOfferingPromotionsResponse -> Lude.Maybe [OfferingPromotion]) (\s a -> s {offeringPromotions = a} :: ListOfferingPromotionsResponse)
{-# DEPRECATED loprsOfferingPromotions "Use generic-lens or generic-optics with 'offeringPromotions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loprsResponseStatus :: Lens.Lens' ListOfferingPromotionsResponse Lude.Int
loprsResponseStatus = Lens.lens (responseStatus :: ListOfferingPromotionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOfferingPromotionsResponse)
{-# DEPRECATED loprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
