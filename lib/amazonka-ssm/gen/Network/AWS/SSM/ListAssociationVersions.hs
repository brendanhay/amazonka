{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListAssociationVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all versions of an association for a specific association ID.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListAssociationVersions
  ( -- * Creating a request
    ListAssociationVersions (..),
    mkListAssociationVersions,

    -- ** Request lenses
    lavNextToken,
    lavMaxResults,
    lavAssociationId,

    -- * Destructuring the response
    ListAssociationVersionsResponse (..),
    mkListAssociationVersionsResponse,

    -- ** Response lenses
    lavrsNextToken,
    lavrsAssociationVersions,
    lavrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListAssociationVersions' smart constructor.
data ListAssociationVersions = ListAssociationVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    associationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssociationVersions' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID for which you want to view all versions.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
mkListAssociationVersions ::
  -- | 'associationId'
  Lude.Text ->
  ListAssociationVersions
mkListAssociationVersions pAssociationId_ =
  ListAssociationVersions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      associationId = pAssociationId_
    }

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavNextToken :: Lens.Lens' ListAssociationVersions (Lude.Maybe Lude.Text)
lavNextToken = Lens.lens (nextToken :: ListAssociationVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociationVersions)
{-# DEPRECATED lavNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavMaxResults :: Lens.Lens' ListAssociationVersions (Lude.Maybe Lude.Natural)
lavMaxResults = Lens.lens (maxResults :: ListAssociationVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAssociationVersions)
{-# DEPRECATED lavMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The association ID for which you want to view all versions.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavAssociationId :: Lens.Lens' ListAssociationVersions Lude.Text
lavAssociationId = Lens.lens (associationId :: ListAssociationVersions -> Lude.Text) (\s a -> s {associationId = a} :: ListAssociationVersions)
{-# DEPRECATED lavAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Page.AWSPager ListAssociationVersions where
  page rq rs
    | Page.stop (rs Lens.^. lavrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lavrsAssociationVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lavNextToken Lens..~ rs Lens.^. lavrsNextToken

instance Lude.AWSRequest ListAssociationVersions where
  type Rs ListAssociationVersions = ListAssociationVersionsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssociationVersionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AssociationVersions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAssociationVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListAssociationVersions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAssociationVersions where
  toJSON ListAssociationVersions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AssociationId" Lude..= associationId)
          ]
      )

instance Lude.ToPath ListAssociationVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAssociationVersions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssociationVersionsResponse' smart constructor.
data ListAssociationVersionsResponse = ListAssociationVersionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    associationVersions ::
      Lude.Maybe
        ( Lude.NonEmpty
            AssociationVersionInfo
        ),
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

-- | Creates a value of 'ListAssociationVersionsResponse' with the minimum fields required to make a request.
--
-- * 'associationVersions' - Information about all versions of the association for the specified association ID.
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'responseStatus' - The response status code.
mkListAssociationVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssociationVersionsResponse
mkListAssociationVersionsResponse pResponseStatus_ =
  ListAssociationVersionsResponse'
    { nextToken = Lude.Nothing,
      associationVersions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrsNextToken :: Lens.Lens' ListAssociationVersionsResponse (Lude.Maybe Lude.Text)
lavrsNextToken = Lens.lens (nextToken :: ListAssociationVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociationVersionsResponse)
{-# DEPRECATED lavrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about all versions of the association for the specified association ID.
--
-- /Note:/ Consider using 'associationVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrsAssociationVersions :: Lens.Lens' ListAssociationVersionsResponse (Lude.Maybe (Lude.NonEmpty AssociationVersionInfo))
lavrsAssociationVersions = Lens.lens (associationVersions :: ListAssociationVersionsResponse -> Lude.Maybe (Lude.NonEmpty AssociationVersionInfo)) (\s a -> s {associationVersions = a} :: ListAssociationVersionsResponse)
{-# DEPRECATED lavrsAssociationVersions "Use generic-lens or generic-optics with 'associationVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrsResponseStatus :: Lens.Lens' ListAssociationVersionsResponse Lude.Int
lavrsResponseStatus = Lens.lens (responseStatus :: ListAssociationVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssociationVersionsResponse)
{-# DEPRECATED lavrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
