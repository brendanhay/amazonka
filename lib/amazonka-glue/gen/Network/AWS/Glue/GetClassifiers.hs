{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetClassifiers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all classifier objects in the Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetClassifiers
  ( -- * Creating a request
    GetClassifiers (..),
    mkGetClassifiers,

    -- ** Request lenses
    gcfNextToken,
    gcfMaxResults,

    -- * Destructuring the response
    GetClassifiersResponse (..),
    mkGetClassifiersResponse,

    -- ** Response lenses
    gcsrsNextToken,
    gcsrsClassifiers,
    gcsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetClassifiers' smart constructor.
data GetClassifiers = GetClassifiers'
  { -- | An optional continuation token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The size of the list to return (optional).
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClassifiers' with the minimum fields required to make a request.
--
-- * 'nextToken' - An optional continuation token.
-- * 'maxResults' - The size of the list to return (optional).
mkGetClassifiers ::
  GetClassifiers
mkGetClassifiers =
  GetClassifiers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An optional continuation token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfNextToken :: Lens.Lens' GetClassifiers (Lude.Maybe Lude.Text)
gcfNextToken = Lens.lens (nextToken :: GetClassifiers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetClassifiers)
{-# DEPRECATED gcfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The size of the list to return (optional).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfMaxResults :: Lens.Lens' GetClassifiers (Lude.Maybe Lude.Natural)
gcfMaxResults = Lens.lens (maxResults :: GetClassifiers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetClassifiers)
{-# DEPRECATED gcfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetClassifiers where
  page rq rs
    | Page.stop (rs Lens.^. gcsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gcsrsClassifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcfNextToken Lens..~ rs Lens.^. gcsrsNextToken

instance Lude.AWSRequest GetClassifiers where
  type Rs GetClassifiers = GetClassifiersResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetClassifiersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Classifiers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetClassifiers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetClassifiers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetClassifiers where
  toJSON GetClassifiers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetClassifiers where
  toPath = Lude.const "/"

instance Lude.ToQuery GetClassifiers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetClassifiersResponse' smart constructor.
data GetClassifiersResponse = GetClassifiersResponse'
  { -- | A continuation token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The requested list of classifier objects.
    classifiers :: Lude.Maybe [Classifier],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClassifiersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token.
-- * 'classifiers' - The requested list of classifier objects.
-- * 'responseStatus' - The response status code.
mkGetClassifiersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetClassifiersResponse
mkGetClassifiersResponse pResponseStatus_ =
  GetClassifiersResponse'
    { nextToken = Lude.Nothing,
      classifiers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsNextToken :: Lens.Lens' GetClassifiersResponse (Lude.Maybe Lude.Text)
gcsrsNextToken = Lens.lens (nextToken :: GetClassifiersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetClassifiersResponse)
{-# DEPRECATED gcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The requested list of classifier objects.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsClassifiers :: Lens.Lens' GetClassifiersResponse (Lude.Maybe [Classifier])
gcsrsClassifiers = Lens.lens (classifiers :: GetClassifiersResponse -> Lude.Maybe [Classifier]) (\s a -> s {classifiers = a} :: GetClassifiersResponse)
{-# DEPRECATED gcsrsClassifiers "Use generic-lens or generic-optics with 'classifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsResponseStatus :: Lens.Lens' GetClassifiersResponse Lude.Int
gcsrsResponseStatus = Lens.lens (responseStatus :: GetClassifiersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetClassifiersResponse)
{-# DEPRECATED gcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
