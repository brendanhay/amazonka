{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all Systems Manager (SSM) documents in the current AWS account and Region. You can limit the results of this request by using a filter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListDocuments
  ( -- * Creating a request
    ListDocuments (..),
    mkListDocuments,

    -- ** Request lenses
    ldDocumentFilterList,
    ldFilters,
    ldNextToken,
    ldMaxResults,

    -- * Destructuring the response
    ListDocumentsResponse (..),
    mkListDocumentsResponse,

    -- ** Response lenses
    ldrsDocumentIdentifiers,
    ldrsNextToken,
    ldrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListDocuments' smart constructor.
data ListDocuments = ListDocuments'
  { -- | This data type is deprecated. Instead, use @Filters@ .
    documentFilterList :: Lude.Maybe (Lude.NonEmpty DocumentFilter),
    -- | One or more DocumentKeyValuesFilter objects. Use a filter to return a more specific list of results. For keys, you can specify one or more key-value pair tags that have been applied to a document. Other valid keys include @Owner@ , @Name@ , @PlatformTypes@ , @DocumentType@ , and @TargetType@ . For example, to return documents you own use @Key=Owner,Values=Self@ . To specify a custom key-value pair, use the format @Key=tag:tagName,Values=valueName@ .
    filters :: Lude.Maybe [DocumentKeyValuesFilter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDocuments' with the minimum fields required to make a request.
--
-- * 'documentFilterList' - This data type is deprecated. Instead, use @Filters@ .
-- * 'filters' - One or more DocumentKeyValuesFilter objects. Use a filter to return a more specific list of results. For keys, you can specify one or more key-value pair tags that have been applied to a document. Other valid keys include @Owner@ , @Name@ , @PlatformTypes@ , @DocumentType@ , and @TargetType@ . For example, to return documents you own use @Key=Owner,Values=Self@ . To specify a custom key-value pair, use the format @Key=tag:tagName,Values=valueName@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkListDocuments ::
  ListDocuments
mkListDocuments =
  ListDocuments'
    { documentFilterList = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | This data type is deprecated. Instead, use @Filters@ .
--
-- /Note:/ Consider using 'documentFilterList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDocumentFilterList :: Lens.Lens' ListDocuments (Lude.Maybe (Lude.NonEmpty DocumentFilter))
ldDocumentFilterList = Lens.lens (documentFilterList :: ListDocuments -> Lude.Maybe (Lude.NonEmpty DocumentFilter)) (\s a -> s {documentFilterList = a} :: ListDocuments)
{-# DEPRECATED ldDocumentFilterList "Use generic-lens or generic-optics with 'documentFilterList' instead." #-}

-- | One or more DocumentKeyValuesFilter objects. Use a filter to return a more specific list of results. For keys, you can specify one or more key-value pair tags that have been applied to a document. Other valid keys include @Owner@ , @Name@ , @PlatformTypes@ , @DocumentType@ , and @TargetType@ . For example, to return documents you own use @Key=Owner,Values=Self@ . To specify a custom key-value pair, use the format @Key=tag:tagName,Values=valueName@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldFilters :: Lens.Lens' ListDocuments (Lude.Maybe [DocumentKeyValuesFilter])
ldFilters = Lens.lens (filters :: ListDocuments -> Lude.Maybe [DocumentKeyValuesFilter]) (\s a -> s {filters = a} :: ListDocuments)
{-# DEPRECATED ldFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDocuments (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDocuments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDocuments)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDocuments (Lude.Maybe Lude.Natural)
ldMaxResults = Lens.lens (maxResults :: ListDocuments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDocuments)
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDocuments where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDocumentIdentifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDocuments where
  type Rs ListDocuments = ListDocumentsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDocumentsResponse'
            Lude.<$> (x Lude..?> "DocumentIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDocuments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListDocuments" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDocuments where
  toJSON ListDocuments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DocumentFilterList" Lude..=) Lude.<$> documentFilterList,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDocuments where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDocuments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDocumentsResponse' smart constructor.
data ListDocumentsResponse = ListDocumentsResponse'
  { -- | The names of the Systems Manager documents.
    documentIdentifiers :: Lude.Maybe [DocumentIdentifier],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDocumentsResponse' with the minimum fields required to make a request.
--
-- * 'documentIdentifiers' - The names of the Systems Manager documents.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkListDocumentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDocumentsResponse
mkListDocumentsResponse pResponseStatus_ =
  ListDocumentsResponse'
    { documentIdentifiers = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of the Systems Manager documents.
--
-- /Note:/ Consider using 'documentIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDocumentIdentifiers :: Lens.Lens' ListDocumentsResponse (Lude.Maybe [DocumentIdentifier])
ldrsDocumentIdentifiers = Lens.lens (documentIdentifiers :: ListDocumentsResponse -> Lude.Maybe [DocumentIdentifier]) (\s a -> s {documentIdentifiers = a} :: ListDocumentsResponse)
{-# DEPRECATED ldrsDocumentIdentifiers "Use generic-lens or generic-optics with 'documentIdentifiers' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDocumentsResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDocumentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDocumentsResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDocumentsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDocumentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDocumentsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
