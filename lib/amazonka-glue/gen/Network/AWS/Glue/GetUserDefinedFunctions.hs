{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gudfsPattern,
    gudfsCatalogId,
    gudfsNextToken,
    gudfsDatabaseName,
    gudfsMaxResults,

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
  { -- | An optional function-name pattern string that filters the function definitions returned.
    pattern' :: Lude.Text,
    -- | The ID of the Data Catalog where the functions to be retrieved are located. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the catalog database where the functions are located. If none is provided, functions from all the databases across the catalog will be returned.
    databaseName :: Lude.Maybe Lude.Text,
    -- | The maximum number of functions to return in one response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserDefinedFunctions' with the minimum fields required to make a request.
--
-- * 'pattern'' - An optional function-name pattern string that filters the function definitions returned.
-- * 'catalogId' - The ID of the Data Catalog where the functions to be retrieved are located. If none is provided, the AWS account ID is used by default.
-- * 'nextToken' - A continuation token, if this is a continuation call.
-- * 'databaseName' - The name of the catalog database where the functions are located. If none is provided, functions from all the databases across the catalog will be returned.
-- * 'maxResults' - The maximum number of functions to return in one response.
mkGetUserDefinedFunctions ::
  -- | 'pattern''
  Lude.Text ->
  GetUserDefinedFunctions
mkGetUserDefinedFunctions pPattern_ =
  GetUserDefinedFunctions'
    { pattern' = pPattern_,
      catalogId = Lude.Nothing,
      nextToken = Lude.Nothing,
      databaseName = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An optional function-name pattern string that filters the function definitions returned.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfsPattern :: Lens.Lens' GetUserDefinedFunctions Lude.Text
gudfsPattern = Lens.lens (pattern' :: GetUserDefinedFunctions -> Lude.Text) (\s a -> s {pattern' = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfsPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

-- | The ID of the Data Catalog where the functions to be retrieved are located. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfsCatalogId :: Lens.Lens' GetUserDefinedFunctions (Lude.Maybe Lude.Text)
gudfsCatalogId = Lens.lens (catalogId :: GetUserDefinedFunctions -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfsCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfsNextToken :: Lens.Lens' GetUserDefinedFunctions (Lude.Maybe Lude.Text)
gudfsNextToken = Lens.lens (nextToken :: GetUserDefinedFunctions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the catalog database where the functions are located. If none is provided, functions from all the databases across the catalog will be returned.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfsDatabaseName :: Lens.Lens' GetUserDefinedFunctions (Lude.Maybe Lude.Text)
gudfsDatabaseName = Lens.lens (databaseName :: GetUserDefinedFunctions -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The maximum number of functions to return in one response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfsMaxResults :: Lens.Lens' GetUserDefinedFunctions (Lude.Maybe Lude.Natural)
gudfsMaxResults = Lens.lens (maxResults :: GetUserDefinedFunctions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetUserDefinedFunctions)
{-# DEPRECATED gudfsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetUserDefinedFunctions where
  page rq rs
    | Page.stop (rs Lens.^. gudfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gudfrsUserDefinedFunctions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gudfsNextToken Lens..~ rs Lens.^. gudfrsNextToken

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
          [ Lude.Just ("Pattern" Lude..= pattern'),
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetUserDefinedFunctions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUserDefinedFunctions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetUserDefinedFunctionsResponse' smart constructor.
data GetUserDefinedFunctionsResponse = GetUserDefinedFunctionsResponse'
  { -- | A continuation token, if the list of functions returned does not include the last requested function.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of requested function definitions.
    userDefinedFunctions :: Lude.Maybe [UserDefinedFunction],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserDefinedFunctionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if the list of functions returned does not include the last requested function.
-- * 'userDefinedFunctions' - A list of requested function definitions.
-- * 'responseStatus' - The response status code.
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
