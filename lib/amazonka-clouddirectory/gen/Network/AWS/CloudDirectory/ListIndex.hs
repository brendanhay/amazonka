{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists objects attached to the specified index.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListIndex
  ( -- * Creating a request
    ListIndex (..),
    mkListIndex,

    -- ** Request lenses
    liDirectoryARN,
    liRangesOnIndexedValues,
    liIndexReference,
    liConsistencyLevel,
    liNextToken,
    liMaxResults,

    -- * Destructuring the response
    ListIndexResponse (..),
    mkListIndexResponse,

    -- ** Response lenses
    lirsIndexAttachments,
    lirsNextToken,
    lirsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListIndex' smart constructor.
data ListIndex = ListIndex'
  { -- | The ARN of the directory that the index exists in.
    directoryARN :: Lude.Text,
    -- | Specifies the ranges of indexed values that you want to query.
    rangesOnIndexedValues :: Lude.Maybe [ObjectAttributeRange],
    -- | The reference to the index to list.
    indexReference :: ObjectReference,
    -- | The consistency level to execute the request at.
    consistencyLevel :: Lude.Maybe ConsistencyLevel,
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of objects in a single page to retrieve from the index during a request. For more information, see <http://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits> .
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIndex' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory that the index exists in.
-- * 'rangesOnIndexedValues' - Specifies the ranges of indexed values that you want to query.
-- * 'indexReference' - The reference to the index to list.
-- * 'consistencyLevel' - The consistency level to execute the request at.
-- * 'nextToken' - The pagination token.
-- * 'maxResults' - The maximum number of objects in a single page to retrieve from the index during a request. For more information, see <http://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits> .
mkListIndex ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'indexReference'
  ObjectReference ->
  ListIndex
mkListIndex pDirectoryARN_ pIndexReference_ =
  ListIndex'
    { directoryARN = pDirectoryARN_,
      rangesOnIndexedValues = Lude.Nothing,
      indexReference = pIndexReference_,
      consistencyLevel = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ARN of the directory that the index exists in.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liDirectoryARN :: Lens.Lens' ListIndex Lude.Text
liDirectoryARN = Lens.lens (directoryARN :: ListIndex -> Lude.Text) (\s a -> s {directoryARN = a} :: ListIndex)
{-# DEPRECATED liDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Specifies the ranges of indexed values that you want to query.
--
-- /Note:/ Consider using 'rangesOnIndexedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liRangesOnIndexedValues :: Lens.Lens' ListIndex (Lude.Maybe [ObjectAttributeRange])
liRangesOnIndexedValues = Lens.lens (rangesOnIndexedValues :: ListIndex -> Lude.Maybe [ObjectAttributeRange]) (\s a -> s {rangesOnIndexedValues = a} :: ListIndex)
{-# DEPRECATED liRangesOnIndexedValues "Use generic-lens or generic-optics with 'rangesOnIndexedValues' instead." #-}

-- | The reference to the index to list.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liIndexReference :: Lens.Lens' ListIndex ObjectReference
liIndexReference = Lens.lens (indexReference :: ListIndex -> ObjectReference) (\s a -> s {indexReference = a} :: ListIndex)
{-# DEPRECATED liIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

-- | The consistency level to execute the request at.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liConsistencyLevel :: Lens.Lens' ListIndex (Lude.Maybe ConsistencyLevel)
liConsistencyLevel = Lens.lens (consistencyLevel :: ListIndex -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: ListIndex)
{-# DEPRECATED liConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListIndex (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListIndex -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIndex)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of objects in a single page to retrieve from the index during a request. For more information, see <http://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits> .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListIndex (Lude.Maybe Lude.Natural)
liMaxResults = Lens.lens (maxResults :: ListIndex -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListIndex)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListIndex where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsIndexAttachments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListIndex where
  type Rs ListIndex = ListIndexResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIndexResponse'
            Lude.<$> (x Lude..?> "IndexAttachments" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListIndex where
  toHeaders ListIndex' {..} =
    Lude.mconcat
      [ "x-amz-data-partition" Lude.=# directoryARN,
        "x-amz-consistency-level" Lude.=# consistencyLevel
      ]

instance Lude.ToJSON ListIndex where
  toJSON ListIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RangesOnIndexedValues" Lude..=) Lude.<$> rangesOnIndexedValues,
            Lude.Just ("IndexReference" Lude..= indexReference),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListIndex where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/index/targets"

instance Lude.ToQuery ListIndex where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListIndexResponse' smart constructor.
data ListIndexResponse = ListIndexResponse'
  { -- | The objects and indexed values attached to the index.
    indexAttachments :: Lude.Maybe [IndexAttachment],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIndexResponse' with the minimum fields required to make a request.
--
-- * 'indexAttachments' - The objects and indexed values attached to the index.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListIndexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIndexResponse
mkListIndexResponse pResponseStatus_ =
  ListIndexResponse'
    { indexAttachments = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The objects and indexed values attached to the index.
--
-- /Note:/ Consider using 'indexAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsIndexAttachments :: Lens.Lens' ListIndexResponse (Lude.Maybe [IndexAttachment])
lirsIndexAttachments = Lens.lens (indexAttachments :: ListIndexResponse -> Lude.Maybe [IndexAttachment]) (\s a -> s {indexAttachments = a} :: ListIndexResponse)
{-# DEPRECATED lirsIndexAttachments "Use generic-lens or generic-optics with 'indexAttachments' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListIndexResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIndexResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListIndexResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListIndexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIndexResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
