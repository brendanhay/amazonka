{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListDocumentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all versions for a document.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListDocumentVersions
  ( -- * Creating a request
    ListDocumentVersions (..),
    mkListDocumentVersions,

    -- ** Request lenses
    ldvNextToken,
    ldvName,
    ldvMaxResults,

    -- * Destructuring the response
    ListDocumentVersionsResponse (..),
    mkListDocumentVersionsResponse,

    -- ** Response lenses
    ldvrsDocumentVersions,
    ldvrsNextToken,
    ldvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListDocumentVersions' smart constructor.
data ListDocumentVersions = ListDocumentVersions'
  { -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the document. You can specify an Amazon Resource Name (ARN).
    name :: Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDocumentVersions' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'name' - The name of the document. You can specify an Amazon Resource Name (ARN).
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkListDocumentVersions ::
  -- | 'name'
  Lude.Text ->
  ListDocumentVersions
mkListDocumentVersions pName_ =
  ListDocumentVersions'
    { nextToken = Lude.Nothing,
      name = pName_,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvNextToken :: Lens.Lens' ListDocumentVersions (Lude.Maybe Lude.Text)
ldvNextToken = Lens.lens (nextToken :: ListDocumentVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDocumentVersions)
{-# DEPRECATED ldvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the document. You can specify an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvName :: Lens.Lens' ListDocumentVersions Lude.Text
ldvName = Lens.lens (name :: ListDocumentVersions -> Lude.Text) (\s a -> s {name = a} :: ListDocumentVersions)
{-# DEPRECATED ldvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvMaxResults :: Lens.Lens' ListDocumentVersions (Lude.Maybe Lude.Natural)
ldvMaxResults = Lens.lens (maxResults :: ListDocumentVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDocumentVersions)
{-# DEPRECATED ldvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDocumentVersions where
  page rq rs
    | Page.stop (rs Lens.^. ldvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldvrsDocumentVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldvNextToken Lens..~ rs Lens.^. ldvrsNextToken

instance Lude.AWSRequest ListDocumentVersions where
  type Rs ListDocumentVersions = ListDocumentVersionsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDocumentVersionsResponse'
            Lude.<$> (x Lude..?> "DocumentVersions")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDocumentVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListDocumentVersions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDocumentVersions where
  toJSON ListDocumentVersions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("Name" Lude..= name),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDocumentVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDocumentVersions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDocumentVersionsResponse' smart constructor.
data ListDocumentVersionsResponse = ListDocumentVersionsResponse'
  { -- | The document versions.
    documentVersions :: Lude.Maybe (Lude.NonEmpty DocumentVersionInfo),
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDocumentVersionsResponse' with the minimum fields required to make a request.
--
-- * 'documentVersions' - The document versions.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkListDocumentVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDocumentVersionsResponse
mkListDocumentVersionsResponse pResponseStatus_ =
  ListDocumentVersionsResponse'
    { documentVersions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The document versions.
--
-- /Note:/ Consider using 'documentVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvrsDocumentVersions :: Lens.Lens' ListDocumentVersionsResponse (Lude.Maybe (Lude.NonEmpty DocumentVersionInfo))
ldvrsDocumentVersions = Lens.lens (documentVersions :: ListDocumentVersionsResponse -> Lude.Maybe (Lude.NonEmpty DocumentVersionInfo)) (\s a -> s {documentVersions = a} :: ListDocumentVersionsResponse)
{-# DEPRECATED ldvrsDocumentVersions "Use generic-lens or generic-optics with 'documentVersions' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvrsNextToken :: Lens.Lens' ListDocumentVersionsResponse (Lude.Maybe Lude.Text)
ldvrsNextToken = Lens.lens (nextToken :: ListDocumentVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDocumentVersionsResponse)
{-# DEPRECATED ldvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvrsResponseStatus :: Lens.Lens' ListDocumentVersionsResponse Lude.Int
ldvrsResponseStatus = Lens.lens (responseStatus :: ListDocumentVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDocumentVersionsResponse)
{-# DEPRECATED ldvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
