{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListDocumentClassifiers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the document classifiers that you have created.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDocumentClassifiers
  ( -- * Creating a request
    ListDocumentClassifiers (..),
    mkListDocumentClassifiers,

    -- ** Request lenses
    ldcNextToken,
    ldcFilter,
    ldcMaxResults,

    -- * Destructuring the response
    ListDocumentClassifiersResponse (..),
    mkListDocumentClassifiersResponse,

    -- ** Response lenses
    ldcrsNextToken,
    ldcrsDocumentClassifierPropertiesList,
    ldcrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDocumentClassifiers' smart constructor.
data ListDocumentClassifiers = ListDocumentClassifiers'
  { -- | Identifies the next page of results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
    filter :: Lude.Maybe DocumentClassifierFilter,
    -- | The maximum number of results to return in each page. The default is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDocumentClassifiers' with the minimum fields required to make a request.
--
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'filter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page. The default is 100.
mkListDocumentClassifiers ::
  ListDocumentClassifiers
mkListDocumentClassifiers =
  ListDocumentClassifiers'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcNextToken :: Lens.Lens' ListDocumentClassifiers (Lude.Maybe Lude.Text)
ldcNextToken = Lens.lens (nextToken :: ListDocumentClassifiers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDocumentClassifiers)
{-# DEPRECATED ldcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcFilter :: Lens.Lens' ListDocumentClassifiers (Lude.Maybe DocumentClassifierFilter)
ldcFilter = Lens.lens (filter :: ListDocumentClassifiers -> Lude.Maybe DocumentClassifierFilter) (\s a -> s {filter = a} :: ListDocumentClassifiers)
{-# DEPRECATED ldcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcMaxResults :: Lens.Lens' ListDocumentClassifiers (Lude.Maybe Lude.Natural)
ldcMaxResults = Lens.lens (maxResults :: ListDocumentClassifiers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDocumentClassifiers)
{-# DEPRECATED ldcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDocumentClassifiers where
  page rq rs
    | Page.stop (rs Lens.^. ldcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldcrsDocumentClassifierPropertiesList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldcNextToken Lens..~ rs Lens.^. ldcrsNextToken

instance Lude.AWSRequest ListDocumentClassifiers where
  type Rs ListDocumentClassifiers = ListDocumentClassifiersResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDocumentClassifiersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "DocumentClassifierPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDocumentClassifiers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.ListDocumentClassifiers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDocumentClassifiers where
  toJSON ListDocumentClassifiers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDocumentClassifiers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDocumentClassifiers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDocumentClassifiersResponse' smart constructor.
data ListDocumentClassifiersResponse = ListDocumentClassifiersResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list containing the properties of each job returned.
    documentClassifierPropertiesList :: Lude.Maybe [DocumentClassifierProperties],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDocumentClassifiersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'documentClassifierPropertiesList' - A list containing the properties of each job returned.
-- * 'responseStatus' - The response status code.
mkListDocumentClassifiersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDocumentClassifiersResponse
mkListDocumentClassifiersResponse pResponseStatus_ =
  ListDocumentClassifiersResponse'
    { nextToken = Lude.Nothing,
      documentClassifierPropertiesList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsNextToken :: Lens.Lens' ListDocumentClassifiersResponse (Lude.Maybe Lude.Text)
ldcrsNextToken = Lens.lens (nextToken :: ListDocumentClassifiersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDocumentClassifiersResponse)
{-# DEPRECATED ldcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list containing the properties of each job returned.
--
-- /Note:/ Consider using 'documentClassifierPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsDocumentClassifierPropertiesList :: Lens.Lens' ListDocumentClassifiersResponse (Lude.Maybe [DocumentClassifierProperties])
ldcrsDocumentClassifierPropertiesList = Lens.lens (documentClassifierPropertiesList :: ListDocumentClassifiersResponse -> Lude.Maybe [DocumentClassifierProperties]) (\s a -> s {documentClassifierPropertiesList = a} :: ListDocumentClassifiersResponse)
{-# DEPRECATED ldcrsDocumentClassifierPropertiesList "Use generic-lens or generic-optics with 'documentClassifierPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsResponseStatus :: Lens.Lens' ListDocumentClassifiersResponse Lude.Int
ldcrsResponseStatus = Lens.lens (responseStatus :: ListDocumentClassifiersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDocumentClassifiersResponse)
{-# DEPRECATED ldcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
