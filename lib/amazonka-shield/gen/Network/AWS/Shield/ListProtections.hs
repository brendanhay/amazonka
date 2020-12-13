{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.ListProtections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all 'Protection' objects for the account.
--
-- This operation returns paginated results.
module Network.AWS.Shield.ListProtections
  ( -- * Creating a request
    ListProtections (..),
    mkListProtections,

    -- ** Request lenses
    lpNextToken,
    lpMaxResults,

    -- * Destructuring the response
    ListProtectionsResponse (..),
    mkListProtectionsResponse,

    -- ** Response lenses
    lprsProtections,
    lprsNextToken,
    lprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkListProtections' smart constructor.
data ListProtections = ListProtections'
  { -- | The @ListProtectionsRequest.NextToken@ value from a previous call to @ListProtections@ . Pass null if this is the first call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of 'Protection' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProtections' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @ListProtectionsRequest.NextToken@ value from a previous call to @ListProtections@ . Pass null if this is the first call.
-- * 'maxResults' - The maximum number of 'Protection' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
mkListProtections ::
  ListProtections
mkListProtections =
  ListProtections'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The @ListProtectionsRequest.NextToken@ value from a previous call to @ListProtections@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListProtections (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListProtections -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProtections)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of 'Protection' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListProtections (Lude.Maybe Lude.Natural)
lpMaxResults = Lens.lens (maxResults :: ListProtections -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListProtections)
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListProtections where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsProtections) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest ListProtections where
  type Rs ListProtections = ListProtectionsResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProtectionsResponse'
            Lude.<$> (x Lude..?> "Protections" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProtections where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.ListProtections" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProtections where
  toJSON ListProtections' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListProtections where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProtections where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProtectionsResponse' smart constructor.
data ListProtectionsResponse = ListProtectionsResponse'
  { -- | The array of enabled 'Protection' objects.
    protections :: Lude.Maybe [Protection],
    -- | If you specify a value for @MaxResults@ and you have more Protections than the value of MaxResults, AWS Shield Advanced returns a NextToken value in the response that allows you to list another group of Protections. For the second and subsequent ListProtections requests, specify the value of NextToken from the previous response to get information about another batch of Protections.
    --
    -- Shield Advanced might return the list of 'Protection' objects in batches smaller than the number specified by MaxResults. If there are more 'Protection' objects to return, Shield Advanced will always also return a @NextToken@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProtectionsResponse' with the minimum fields required to make a request.
--
-- * 'protections' - The array of enabled 'Protection' objects.
-- * 'nextToken' - If you specify a value for @MaxResults@ and you have more Protections than the value of MaxResults, AWS Shield Advanced returns a NextToken value in the response that allows you to list another group of Protections. For the second and subsequent ListProtections requests, specify the value of NextToken from the previous response to get information about another batch of Protections.
--
-- Shield Advanced might return the list of 'Protection' objects in batches smaller than the number specified by MaxResults. If there are more 'Protection' objects to return, Shield Advanced will always also return a @NextToken@ .
-- * 'responseStatus' - The response status code.
mkListProtectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProtectionsResponse
mkListProtectionsResponse pResponseStatus_ =
  ListProtectionsResponse'
    { protections = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The array of enabled 'Protection' objects.
--
-- /Note:/ Consider using 'protections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsProtections :: Lens.Lens' ListProtectionsResponse (Lude.Maybe [Protection])
lprsProtections = Lens.lens (protections :: ListProtectionsResponse -> Lude.Maybe [Protection]) (\s a -> s {protections = a} :: ListProtectionsResponse)
{-# DEPRECATED lprsProtections "Use generic-lens or generic-optics with 'protections' instead." #-}

-- | If you specify a value for @MaxResults@ and you have more Protections than the value of MaxResults, AWS Shield Advanced returns a NextToken value in the response that allows you to list another group of Protections. For the second and subsequent ListProtections requests, specify the value of NextToken from the previous response to get information about another batch of Protections.
--
-- Shield Advanced might return the list of 'Protection' objects in batches smaller than the number specified by MaxResults. If there are more 'Protection' objects to return, Shield Advanced will always also return a @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListProtectionsResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListProtectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProtectionsResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListProtectionsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListProtectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProtectionsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
