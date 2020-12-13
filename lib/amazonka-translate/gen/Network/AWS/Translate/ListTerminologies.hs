{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.ListTerminologies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of custom terminologies associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.Translate.ListTerminologies
  ( -- * Creating a request
    ListTerminologies (..),
    mkListTerminologies,

    -- ** Request lenses
    ltNextToken,
    ltMaxResults,

    -- * Destructuring the response
    ListTerminologiesResponse (..),
    mkListTerminologiesResponse,

    -- ** Response lenses
    ltrsTerminologyPropertiesList,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkListTerminologies' smart constructor.
data ListTerminologies = ListTerminologies'
  { -- | If the result of the request to ListTerminologies was truncated, include the NextToken to fetch the next group of custom terminologies.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of custom terminologies returned per list request.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTerminologies' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the result of the request to ListTerminologies was truncated, include the NextToken to fetch the next group of custom terminologies.
-- * 'maxResults' - The maximum number of custom terminologies returned per list request.
mkListTerminologies ::
  ListTerminologies
mkListTerminologies =
  ListTerminologies'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If the result of the request to ListTerminologies was truncated, include the NextToken to fetch the next group of custom terminologies.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTerminologies (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTerminologies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTerminologies)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of custom terminologies returned per list request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTerminologies (Lude.Maybe Lude.Natural)
ltMaxResults = Lens.lens (maxResults :: ListTerminologies -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTerminologies)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTerminologies where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTerminologyPropertiesList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListTerminologies where
  type Rs ListTerminologies = ListTerminologiesResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTerminologiesResponse'
            Lude.<$> (x Lude..?> "TerminologyPropertiesList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTerminologies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.ListTerminologies" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTerminologies where
  toJSON ListTerminologies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTerminologies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTerminologies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTerminologiesResponse' smart constructor.
data ListTerminologiesResponse = ListTerminologiesResponse'
  { -- | The properties list of the custom terminologies returned on the list request.
    terminologyPropertiesList :: Lude.Maybe [TerminologyProperties],
    -- | If the response to the ListTerminologies was truncated, the NextToken fetches the next group of custom terminologies.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTerminologiesResponse' with the minimum fields required to make a request.
--
-- * 'terminologyPropertiesList' - The properties list of the custom terminologies returned on the list request.
-- * 'nextToken' - If the response to the ListTerminologies was truncated, the NextToken fetches the next group of custom terminologies.
-- * 'responseStatus' - The response status code.
mkListTerminologiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTerminologiesResponse
mkListTerminologiesResponse pResponseStatus_ =
  ListTerminologiesResponse'
    { terminologyPropertiesList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The properties list of the custom terminologies returned on the list request.
--
-- /Note:/ Consider using 'terminologyPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTerminologyPropertiesList :: Lens.Lens' ListTerminologiesResponse (Lude.Maybe [TerminologyProperties])
ltrsTerminologyPropertiesList = Lens.lens (terminologyPropertiesList :: ListTerminologiesResponse -> Lude.Maybe [TerminologyProperties]) (\s a -> s {terminologyPropertiesList = a} :: ListTerminologiesResponse)
{-# DEPRECATED ltrsTerminologyPropertiesList "Use generic-lens or generic-optics with 'terminologyPropertiesList' instead." #-}

-- | If the response to the ListTerminologies was truncated, the NextToken fetches the next group of custom terminologies.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTerminologiesResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTerminologiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTerminologiesResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTerminologiesResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTerminologiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTerminologiesResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
