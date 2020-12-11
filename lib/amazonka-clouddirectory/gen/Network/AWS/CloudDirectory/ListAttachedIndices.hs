{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListAttachedIndices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists indices attached to the specified object.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListAttachedIndices
  ( -- * Creating a request
    ListAttachedIndices (..),
    mkListAttachedIndices,

    -- ** Request lenses
    laiConsistencyLevel,
    laiNextToken,
    laiMaxResults,
    laiDirectoryARN,
    laiTargetReference,

    -- * Destructuring the response
    ListAttachedIndicesResponse (..),
    mkListAttachedIndicesResponse,

    -- ** Response lenses
    lairsIndexAttachments,
    lairsNextToken,
    lairsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAttachedIndices' smart constructor.
data ListAttachedIndices = ListAttachedIndices'
  { consistencyLevel ::
      Lude.Maybe ConsistencyLevel,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    directoryARN :: Lude.Text,
    targetReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttachedIndices' with the minimum fields required to make a request.
--
-- * 'consistencyLevel' - The consistency level to use for this operation.
-- * 'directoryARN' - The ARN of the directory.
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'nextToken' - The pagination token.
-- * 'targetReference' - A reference to the object that has indices attached.
mkListAttachedIndices ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'targetReference'
  ObjectReference ->
  ListAttachedIndices
mkListAttachedIndices pDirectoryARN_ pTargetReference_ =
  ListAttachedIndices'
    { consistencyLevel = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      targetReference = pTargetReference_
    }

-- | The consistency level to use for this operation.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiConsistencyLevel :: Lens.Lens' ListAttachedIndices (Lude.Maybe ConsistencyLevel)
laiConsistencyLevel = Lens.lens (consistencyLevel :: ListAttachedIndices -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: ListAttachedIndices)
{-# DEPRECATED laiConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiNextToken :: Lens.Lens' ListAttachedIndices (Lude.Maybe Lude.Text)
laiNextToken = Lens.lens (nextToken :: ListAttachedIndices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAttachedIndices)
{-# DEPRECATED laiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiMaxResults :: Lens.Lens' ListAttachedIndices (Lude.Maybe Lude.Natural)
laiMaxResults = Lens.lens (maxResults :: ListAttachedIndices -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAttachedIndices)
{-# DEPRECATED laiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ARN of the directory.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiDirectoryARN :: Lens.Lens' ListAttachedIndices Lude.Text
laiDirectoryARN = Lens.lens (directoryARN :: ListAttachedIndices -> Lude.Text) (\s a -> s {directoryARN = a} :: ListAttachedIndices)
{-# DEPRECATED laiDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | A reference to the object that has indices attached.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiTargetReference :: Lens.Lens' ListAttachedIndices ObjectReference
laiTargetReference = Lens.lens (targetReference :: ListAttachedIndices -> ObjectReference) (\s a -> s {targetReference = a} :: ListAttachedIndices)
{-# DEPRECATED laiTargetReference "Use generic-lens or generic-optics with 'targetReference' instead." #-}

instance Page.AWSPager ListAttachedIndices where
  page rq rs
    | Page.stop (rs Lens.^. lairsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lairsIndexAttachments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laiNextToken Lens..~ rs Lens.^. lairsNextToken

instance Lude.AWSRequest ListAttachedIndices where
  type Rs ListAttachedIndices = ListAttachedIndicesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAttachedIndicesResponse'
            Lude.<$> (x Lude..?> "IndexAttachments" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAttachedIndices where
  toHeaders ListAttachedIndices' {..} =
    Lude.mconcat
      [ "x-amz-consistency-level" Lude.=# consistencyLevel,
        "x-amz-data-partition" Lude.=# directoryARN
      ]

instance Lude.ToJSON ListAttachedIndices where
  toJSON ListAttachedIndices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("TargetReference" Lude..= targetReference)
          ]
      )

instance Lude.ToPath ListAttachedIndices where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/indices"

instance Lude.ToQuery ListAttachedIndices where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAttachedIndicesResponse' smart constructor.
data ListAttachedIndicesResponse = ListAttachedIndicesResponse'
  { indexAttachments ::
      Lude.Maybe [IndexAttachment],
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

-- | Creates a value of 'ListAttachedIndicesResponse' with the minimum fields required to make a request.
--
-- * 'indexAttachments' - The indices attached to the specified object.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListAttachedIndicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAttachedIndicesResponse
mkListAttachedIndicesResponse pResponseStatus_ =
  ListAttachedIndicesResponse'
    { indexAttachments = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The indices attached to the specified object.
--
-- /Note:/ Consider using 'indexAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lairsIndexAttachments :: Lens.Lens' ListAttachedIndicesResponse (Lude.Maybe [IndexAttachment])
lairsIndexAttachments = Lens.lens (indexAttachments :: ListAttachedIndicesResponse -> Lude.Maybe [IndexAttachment]) (\s a -> s {indexAttachments = a} :: ListAttachedIndicesResponse)
{-# DEPRECATED lairsIndexAttachments "Use generic-lens or generic-optics with 'indexAttachments' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lairsNextToken :: Lens.Lens' ListAttachedIndicesResponse (Lude.Maybe Lude.Text)
lairsNextToken = Lens.lens (nextToken :: ListAttachedIndicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAttachedIndicesResponse)
{-# DEPRECATED lairsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lairsResponseStatus :: Lens.Lens' ListAttachedIndicesResponse Lude.Int
lairsResponseStatus = Lens.lens (responseStatus :: ListAttachedIndicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAttachedIndicesResponse)
{-# DEPRECATED lairsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
