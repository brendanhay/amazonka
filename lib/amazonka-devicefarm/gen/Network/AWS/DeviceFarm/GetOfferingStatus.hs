{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetOfferingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current status and future status of all offerings purchased by an AWS account. The response indicates how many offerings are currently available and the offerings that will be available in the next period. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.GetOfferingStatus
  ( -- * Creating a request
    GetOfferingStatus (..),
    mkGetOfferingStatus,

    -- ** Request lenses
    gosNextToken,

    -- * Destructuring the response
    GetOfferingStatusResponse (..),
    mkGetOfferingStatusResponse,

    -- ** Response lenses
    gosrsNextPeriod,
    gosrsCurrent,
    gosrsNextToken,
    gosrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to retrieve the offering status for the specified customer or account.
--
-- /See:/ 'mkGetOfferingStatus' smart constructor.
newtype GetOfferingStatus = GetOfferingStatus'
  { nextToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOfferingStatus' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkGetOfferingStatus ::
  GetOfferingStatus
mkGetOfferingStatus = GetOfferingStatus' {nextToken = Lude.Nothing}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosNextToken :: Lens.Lens' GetOfferingStatus (Lude.Maybe Lude.Text)
gosNextToken = Lens.lens (nextToken :: GetOfferingStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetOfferingStatus)
{-# DEPRECATED gosNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager GetOfferingStatus where
  page rq rs
    | Page.stop (rs Lens.^. gosrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gosrsCurrent) = Lude.Nothing
    | Page.stop (rs Lens.^. gosrsNextPeriod) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gosNextToken Lens..~ rs Lens.^. gosrsNextToken

instance Lude.AWSRequest GetOfferingStatus where
  type Rs GetOfferingStatus = GetOfferingStatusResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOfferingStatusResponse'
            Lude.<$> (x Lude..?> "nextPeriod" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "current" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOfferingStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetOfferingStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOfferingStatus where
  toJSON GetOfferingStatus' {..} =
    Lude.object
      (Lude.catMaybes [("nextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath GetOfferingStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOfferingStatus where
  toQuery = Lude.const Lude.mempty

-- | Returns the status result for a device offering.
--
-- /See:/ 'mkGetOfferingStatusResponse' smart constructor.
data GetOfferingStatusResponse = GetOfferingStatusResponse'
  { nextPeriod ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (OfferingStatus)
        ),
    current ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (OfferingStatus)
        ),
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetOfferingStatusResponse' with the minimum fields required to make a request.
--
-- * 'current' - When specified, gets the offering status for the current period.
-- * 'nextPeriod' - When specified, gets the offering status for the next period.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkGetOfferingStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOfferingStatusResponse
mkGetOfferingStatusResponse pResponseStatus_ =
  GetOfferingStatusResponse'
    { nextPeriod = Lude.Nothing,
      current = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When specified, gets the offering status for the next period.
--
-- /Note:/ Consider using 'nextPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsNextPeriod :: Lens.Lens' GetOfferingStatusResponse (Lude.Maybe (Lude.HashMap Lude.Text (OfferingStatus)))
gosrsNextPeriod = Lens.lens (nextPeriod :: GetOfferingStatusResponse -> Lude.Maybe (Lude.HashMap Lude.Text (OfferingStatus))) (\s a -> s {nextPeriod = a} :: GetOfferingStatusResponse)
{-# DEPRECATED gosrsNextPeriod "Use generic-lens or generic-optics with 'nextPeriod' instead." #-}

-- | When specified, gets the offering status for the current period.
--
-- /Note:/ Consider using 'current' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsCurrent :: Lens.Lens' GetOfferingStatusResponse (Lude.Maybe (Lude.HashMap Lude.Text (OfferingStatus)))
gosrsCurrent = Lens.lens (current :: GetOfferingStatusResponse -> Lude.Maybe (Lude.HashMap Lude.Text (OfferingStatus))) (\s a -> s {current = a} :: GetOfferingStatusResponse)
{-# DEPRECATED gosrsCurrent "Use generic-lens or generic-optics with 'current' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsNextToken :: Lens.Lens' GetOfferingStatusResponse (Lude.Maybe Lude.Text)
gosrsNextToken = Lens.lens (nextToken :: GetOfferingStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetOfferingStatusResponse)
{-# DEPRECATED gosrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrsResponseStatus :: Lens.Lens' GetOfferingStatusResponse Lude.Int
gosrsResponseStatus = Lens.lens (responseStatus :: GetOfferingStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOfferingStatusResponse)
{-# DEPRECATED gosrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
