{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetUserDefinedFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves multiple function definitions from the Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetUserDefinedFunctions
  ( -- * Creating a request
    GetUserDefinedFunctions (..),
    mkGetUserDefinedFunctions,

    -- ** Request lenses
    gudfCatalogId,
    gudfNextToken,
    gudfDatabaseName,
    gudfMaxResults,
    gudfPattern,

    -- * Destructuring the response
    GetUserDefinedFunctionsResponse (..),
    mkGetUserDefinedFunctionsResponse,

    -- ** Response lenses
    gudfrsNextToken,
    gudfrsUserDefinedFunctions,
    gudfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUserDefinedFunctions' smart constructor.
data GetUserDefinedFunctions = GetUserDefinedFunctions'
  { catalogId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    databaseName :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    pattern' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserDefinedFunctions' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog where the functions to be retrieved are located. If none is provided, the AWS account ID is used by default.
-- * 'databaseName' - The name of the catalog database where the functions are located. If none is provided, functions from all the databases across the catalog will be returned.
-- * 'maxResults' - The maximum number of functions to return in one response.
-- * 'nextToken' - A continuation token, if this is a continuation call.
-- * 'pattern'' - An optional function-name pattern string that filters the function definitions returned.
mkGetUserDefinedFunctions ::
  -- | 'pattern''
  Lude.Text ->
  GetUserDefinedFunctions
mkGetUserDefinedFunctions pPattern_ =
  GetUserDefinedFunctions'
    { catalogId = Lude.Nothing,
      nextToken = Lude.Nothing,
      databaseName = Lude.Nothing,
      maxResults = Lude.Nothing,
      pattern' = pPattern_
    }

-- | The ID of the Data Catalog where the functions to be retrieved are located. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfCatalogId :: Lens.Lens' GetUserDefinedFunctions (Lude.Maybe Lude.Text)
gudfCatalogId = Lens.lens (catalogId :: GetUserDefinedFunctions -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfNextToken :: Lens.Lens' GetUserDefinedFunctions (Lude.Maybe Lude.Text)
gudfNextToken = Lens.lens (nextToken :: GetUserDefinedFunctions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the catalog database where the functions are located. If none is provided, functions from all the databases across the catalog will be returned.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfDatabaseName :: Lens.Lens' GetUserDefinedFunctions (Lude.Maybe Lude.Text)
gudfDatabaseName = Lens.lens (databaseName :: GetUserDefinedFunctions -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The maximum number of functions to return in one response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfMaxResults :: Lens.Lens' GetUserDefinedFunctions (Lude.Maybe Lude.Natural)
gudfMaxResults = Lens.lens (maxResults :: GetUserDefinedFunctions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional function-name pattern string that filters the function definitions returned.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfPattern :: Lens.Lens' GetUserDefinedFunctions Lude.Text
gudfPattern = Lens.lens (pattern' :: GetUserDefinedFunctions -> Lude.Text) (\s a -> s {pattern' = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

instance Page.AWSPager GetUserDefinedFunctions where
  page rq rs
    | Page.stop (rs Lens.^. gudfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gudfrsUserDefinedFunctions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gudfNextToken Lens..~ rs Lens.^. gudfrsNextToken

instance Lude.AWSRequest GetUserDefinedFunctions where
  type Rs GetUserDefinedFunctions = GetUserDefinedFunctionsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUserDefinedFunctionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "UserDefinedFunctions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUserDefinedFunctions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetUserDefinedFunctions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetUserDefinedFunctions where
  toJSON GetUserDefinedFunctions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("Pattern" Lude..= pattern')
          ]
      )

instance Lude.ToPath GetUserDefinedFunctions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUserDefinedFunctions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetUserDefinedFunctionsResponse' smart constructor.
data GetUserDefinedFunctionsResponse = GetUserDefinedFunctionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    userDefinedFunctions ::
      Lude.Maybe
        [UserDefinedFunction],
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

-- | Creates a value of 'GetUserDefinedFunctionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if the list of functions returned does not include the last requested function.
-- * 'responseStatus' - The response status code.
-- * 'userDefinedFunctions' - A list of requested function definitions.
mkGetUserDefinedFunctionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUserDefinedFunctionsResponse
mkGetUserDefinedFunctionsResponse pResponseStatus_ =
  GetUserDefinedFunctionsResponse'
    { nextToken = Lude.Nothing,
      userDefinedFunctions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if the list of functions returned does not include the last requested function.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrsNextToken :: Lens.Lens' GetUserDefinedFunctionsResponse (Lude.Maybe Lude.Text)
gudfrsNextToken = Lens.lens (nextToken :: GetUserDefinedFunctionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetUserDefinedFunctionsResponse)
{-# DEPRECATED gudfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of requested function definitions.
--
-- /Note:/ Consider using 'userDefinedFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrsUserDefinedFunctions :: Lens.Lens' GetUserDefinedFunctionsResponse (Lude.Maybe [UserDefinedFunction])
gudfrsUserDefinedFunctions = Lens.lens (userDefinedFunctions :: GetUserDefinedFunctionsResponse -> Lude.Maybe [UserDefinedFunction]) (\s a -> s {userDefinedFunctions = a} :: GetUserDefinedFunctionsResponse)
{-# DEPRECATED gudfrsUserDefinedFunctions "Use generic-lens or generic-optics with 'userDefinedFunctions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrsResponseStatus :: Lens.Lens' GetUserDefinedFunctionsResponse Lude.Int
gudfrsResponseStatus = Lens.lens (responseStatus :: GetUserDefinedFunctionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUserDefinedFunctionsResponse)
{-# DEPRECATED gudfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
