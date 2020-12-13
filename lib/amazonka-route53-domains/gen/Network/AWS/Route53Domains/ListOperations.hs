{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ListOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of the operations that return an operation ID and that have ever been performed on domains that were registered by the current account.
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ListOperations
  ( -- * Creating a request
    ListOperations (..),
    mkListOperations,

    -- ** Request lenses
    loMarker,
    loMaxItems,
    loSubmittedSince,

    -- * Destructuring the response
    ListOperationsResponse (..),
    mkListOperationsResponse,

    -- ** Response lenses
    lorsNextPageMarker,
    lorsOperations,
    lorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The ListOperations request includes the following elements.
--
-- /See:/ 'mkListOperations' smart constructor.
data ListOperations = ListOperations'
  { -- | For an initial request for a list of operations, omit this element. If the number of operations that are not yet complete is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional operations. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
    marker :: Lude.Maybe Lude.Text,
    -- | Number of domains to be returned.
    --
    -- Default: 20
    maxItems :: Lude.Maybe Lude.Int,
    -- | An optional parameter that lets you get information about all the operations that you submitted after a specified date and time. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
    submittedSince :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOperations' with the minimum fields required to make a request.
--
-- * 'marker' - For an initial request for a list of operations, omit this element. If the number of operations that are not yet complete is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional operations. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
-- * 'maxItems' - Number of domains to be returned.
--
-- Default: 20
-- * 'submittedSince' - An optional parameter that lets you get information about all the operations that you submitted after a specified date and time. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
mkListOperations ::
  ListOperations
mkListOperations =
  ListOperations'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      submittedSince = Lude.Nothing
    }

-- | For an initial request for a list of operations, omit this element. If the number of operations that are not yet complete is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional operations. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMarker :: Lens.Lens' ListOperations (Lude.Maybe Lude.Text)
loMarker = Lens.lens (marker :: ListOperations -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListOperations)
{-# DEPRECATED loMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Number of domains to be returned.
--
-- Default: 20
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxItems :: Lens.Lens' ListOperations (Lude.Maybe Lude.Int)
loMaxItems = Lens.lens (maxItems :: ListOperations -> Lude.Maybe Lude.Int) (\s a -> s {maxItems = a} :: ListOperations)
{-# DEPRECATED loMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | An optional parameter that lets you get information about all the operations that you submitted after a specified date and time. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'submittedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSubmittedSince :: Lens.Lens' ListOperations (Lude.Maybe Lude.Timestamp)
loSubmittedSince = Lens.lens (submittedSince :: ListOperations -> Lude.Maybe Lude.Timestamp) (\s a -> s {submittedSince = a} :: ListOperations)
{-# DEPRECATED loSubmittedSince "Use generic-lens or generic-optics with 'submittedSince' instead." #-}

instance Page.AWSPager ListOperations where
  page rq rs
    | Page.stop (rs Lens.^. lorsNextPageMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lorsOperations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loMarker Lens..~ rs Lens.^. lorsNextPageMarker

instance Lude.AWSRequest ListOperations where
  type Rs ListOperations = ListOperationsResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOperationsResponse'
            Lude.<$> (x Lude..?> "NextPageMarker")
            Lude.<*> (x Lude..?> "Operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOperations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53Domains_v20140515.ListOperations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOperations where
  toJSON ListOperations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("MaxItems" Lude..=) Lude.<$> maxItems,
            ("SubmittedSince" Lude..=) Lude.<$> submittedSince
          ]
      )

instance Lude.ToPath ListOperations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOperations where
  toQuery = Lude.const Lude.mempty

-- | The ListOperations response includes the following elements.
--
-- /See:/ 'mkListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { -- | If there are more operations than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
    nextPageMarker :: Lude.Maybe Lude.Text,
    -- | Lists summaries of the operations.
    operations :: [OperationSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOperationsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageMarker' - If there are more operations than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
-- * 'operations' - Lists summaries of the operations.
-- * 'responseStatus' - The response status code.
mkListOperationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOperationsResponse
mkListOperationsResponse pResponseStatus_ =
  ListOperationsResponse'
    { nextPageMarker = Lude.Nothing,
      operations = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If there are more operations than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
--
-- /Note:/ Consider using 'nextPageMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsNextPageMarker :: Lens.Lens' ListOperationsResponse (Lude.Maybe Lude.Text)
lorsNextPageMarker = Lens.lens (nextPageMarker :: ListOperationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageMarker = a} :: ListOperationsResponse)
{-# DEPRECATED lorsNextPageMarker "Use generic-lens or generic-optics with 'nextPageMarker' instead." #-}

-- | Lists summaries of the operations.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsOperations :: Lens.Lens' ListOperationsResponse [OperationSummary]
lorsOperations = Lens.lens (operations :: ListOperationsResponse -> [OperationSummary]) (\s a -> s {operations = a} :: ListOperationsResponse)
{-# DEPRECATED lorsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsResponseStatus :: Lens.Lens' ListOperationsResponse Lude.Int
lorsResponseStatus = Lens.lens (responseStatus :: ListOperationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOperationsResponse)
{-# DEPRECATED lorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
