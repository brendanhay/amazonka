{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the runtime parameters offered by the underlying database software, or engine, for a specific database in Amazon Lightsail.
--
-- In addition to the parameter names and values, this operation returns other information about each parameter. This information includes whether changes require a reboot, whether the parameter is modifiable, the allowed values, and the data types.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseParameters
  ( -- * Creating a request
    GetRelationalDatabaseParameters (..),
    mkGetRelationalDatabaseParameters,

    -- ** Request lenses
    grdpPageToken,
    grdpRelationalDatabaseName,

    -- * Destructuring the response
    GetRelationalDatabaseParametersResponse (..),
    mkGetRelationalDatabaseParametersResponse,

    -- ** Response lenses
    grdprsNextPageToken,
    grdprsParameters,
    grdprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabaseParameters' smart constructor.
data GetRelationalDatabaseParameters = GetRelationalDatabaseParameters'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseParameters@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The name of your database for which to get parameters.
    relationalDatabaseName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseParameters' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseParameters@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
-- * 'relationalDatabaseName' - The name of your database for which to get parameters.
mkGetRelationalDatabaseParameters ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  GetRelationalDatabaseParameters
mkGetRelationalDatabaseParameters pRelationalDatabaseName_ =
  GetRelationalDatabaseParameters'
    { pageToken = Lude.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseParameters@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdpPageToken :: Lens.Lens' GetRelationalDatabaseParameters (Lude.Maybe Lude.Text)
grdpPageToken = Lens.lens (pageToken :: GetRelationalDatabaseParameters -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetRelationalDatabaseParameters)
{-# DEPRECATED grdpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The name of your database for which to get parameters.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdpRelationalDatabaseName :: Lens.Lens' GetRelationalDatabaseParameters Lude.Text
grdpRelationalDatabaseName = Lens.lens (relationalDatabaseName :: GetRelationalDatabaseParameters -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseParameters)
{-# DEPRECATED grdpRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Page.AWSPager GetRelationalDatabaseParameters where
  page rq rs
    | Page.stop (rs Lens.^. grdprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grdprsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grdpPageToken Lens..~ rs Lens.^. grdprsNextPageToken

instance Lude.AWSRequest GetRelationalDatabaseParameters where
  type
    Rs GetRelationalDatabaseParameters =
      GetRelationalDatabaseParametersResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseParametersResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "parameters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabaseParameters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetRelationalDatabaseParameters" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabaseParameters where
  toJSON GetRelationalDatabaseParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("pageToken" Lude..=) Lude.<$> pageToken,
            Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath GetRelationalDatabaseParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabaseParameters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseParametersResponse' smart constructor.
data GetRelationalDatabaseParametersResponse = GetRelationalDatabaseParametersResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetRelationalDatabaseParameters@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An object describing the result of your get relational database parameters request.
    parameters :: Lude.Maybe [RelationalDatabaseParameter],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseParametersResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseParameters@ request and specify the next page token using the @pageToken@ parameter.
-- * 'parameters' - An object describing the result of your get relational database parameters request.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseParametersResponse
mkGetRelationalDatabaseParametersResponse pResponseStatus_ =
  GetRelationalDatabaseParametersResponse'
    { nextPageToken =
        Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseParameters@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdprsNextPageToken :: Lens.Lens' GetRelationalDatabaseParametersResponse (Lude.Maybe Lude.Text)
grdprsNextPageToken = Lens.lens (nextPageToken :: GetRelationalDatabaseParametersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetRelationalDatabaseParametersResponse)
{-# DEPRECATED grdprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An object describing the result of your get relational database parameters request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdprsParameters :: Lens.Lens' GetRelationalDatabaseParametersResponse (Lude.Maybe [RelationalDatabaseParameter])
grdprsParameters = Lens.lens (parameters :: GetRelationalDatabaseParametersResponse -> Lude.Maybe [RelationalDatabaseParameter]) (\s a -> s {parameters = a} :: GetRelationalDatabaseParametersResponse)
{-# DEPRECATED grdprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdprsResponseStatus :: Lens.Lens' GetRelationalDatabaseParametersResponse Lude.Int
grdprsResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseParametersResponse)
{-# DEPRECATED grdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
