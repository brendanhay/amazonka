{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists completed exports within the past 90 days.
module Network.AWS.DynamoDB.ListExports
  ( -- * Creating a request
    ListExports (..),
    mkListExports,

    -- ** Request lenses
    leTableARN,
    leNextToken,
    leMaxResults,

    -- * Destructuring the response
    ListExportsResponse (..),
    mkListExportsResponse,

    -- ** Response lenses
    lersExportSummaries,
    lersNextToken,
    lersResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListExports' smart constructor.
data ListExports = ListExports'
  { -- | The Amazon Resource Name (ARN) associated with the exported table.
    tableARN :: Lude.Maybe Lude.Text,
    -- | An optional string that, if supplied, must be copied from the output of a previous call to @ListExports@ . When provided in this manner, the API fetches the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of results to return per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListExports' with the minimum fields required to make a request.
--
-- * 'tableARN' - The Amazon Resource Name (ARN) associated with the exported table.
-- * 'nextToken' - An optional string that, if supplied, must be copied from the output of a previous call to @ListExports@ . When provided in this manner, the API fetches the next page of results.
-- * 'maxResults' - Maximum number of results to return per page.
mkListExports ::
  ListExports
mkListExports =
  ListExports'
    { tableARN = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) associated with the exported table.
--
-- /Note:/ Consider using 'tableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leTableARN :: Lens.Lens' ListExports (Lude.Maybe Lude.Text)
leTableARN = Lens.lens (tableARN :: ListExports -> Lude.Maybe Lude.Text) (\s a -> s {tableARN = a} :: ListExports)
{-# DEPRECATED leTableARN "Use generic-lens or generic-optics with 'tableARN' instead." #-}

-- | An optional string that, if supplied, must be copied from the output of a previous call to @ListExports@ . When provided in this manner, the API fetches the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListExports (Lude.Maybe Lude.Text)
leNextToken = Lens.lens (nextToken :: ListExports -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExports)
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListExports (Lude.Maybe Lude.Natural)
leMaxResults = Lens.lens (maxResults :: ListExports -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListExports)
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListExports where
  type Rs ListExports = ListExportsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListExportsResponse'
            Lude.<$> (x Lude..?> "ExportSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListExports where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.ListExports" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListExports where
  toJSON ListExports' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TableArn" Lude..=) Lude.<$> tableARN,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListExports where
  toPath = Lude.const "/"

instance Lude.ToQuery ListExports where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { -- | A list of @ExportSummary@ objects.
    exportSummaries :: Lude.Maybe [ExportSummary],
    -- | If this value is returned, there are additional results to be displayed. To retrieve them, call @ListExports@ again, with @NextToken@ set to this value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListExportsResponse' with the minimum fields required to make a request.
--
-- * 'exportSummaries' - A list of @ExportSummary@ objects.
-- * 'nextToken' - If this value is returned, there are additional results to be displayed. To retrieve them, call @ListExports@ again, with @NextToken@ set to this value.
-- * 'responseStatus' - The response status code.
mkListExportsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListExportsResponse
mkListExportsResponse pResponseStatus_ =
  ListExportsResponse'
    { exportSummaries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @ExportSummary@ objects.
--
-- /Note:/ Consider using 'exportSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersExportSummaries :: Lens.Lens' ListExportsResponse (Lude.Maybe [ExportSummary])
lersExportSummaries = Lens.lens (exportSummaries :: ListExportsResponse -> Lude.Maybe [ExportSummary]) (\s a -> s {exportSummaries = a} :: ListExportsResponse)
{-# DEPRECATED lersExportSummaries "Use generic-lens or generic-optics with 'exportSummaries' instead." #-}

-- | If this value is returned, there are additional results to be displayed. To retrieve them, call @ListExports@ again, with @NextToken@ set to this value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersNextToken :: Lens.Lens' ListExportsResponse (Lude.Maybe Lude.Text)
lersNextToken = Lens.lens (nextToken :: ListExportsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExportsResponse)
{-# DEPRECATED lersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersResponseStatus :: Lens.Lens' ListExportsResponse Lude.Int
lersResponseStatus = Lens.lens (responseStatus :: ListExportsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListExportsResponse)
{-# DEPRECATED lersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
