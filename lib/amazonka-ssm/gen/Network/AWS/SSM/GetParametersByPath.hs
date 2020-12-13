{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetParametersByPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about one or more parameters in a specific hierarchy.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetParametersByPath
  ( -- * Creating a request
    GetParametersByPath (..),
    mkGetParametersByPath,

    -- ** Request lenses
    gpbpWithDecryption,
    gpbpParameterFilters,
    gpbpPath,
    gpbpNextToken,
    gpbpRecursive,
    gpbpMaxResults,

    -- * Destructuring the response
    GetParametersByPathResponse (..),
    mkGetParametersByPathResponse,

    -- ** Response lenses
    gpbprsNextToken,
    gpbprsParameters,
    gpbprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetParametersByPath' smart constructor.
data GetParametersByPath = GetParametersByPath'
  { -- | Retrieve all parameters in a hierarchy with their value decrypted.
    withDecryption :: Lude.Maybe Lude.Bool,
    -- | Filters to limit the request results.
    parameterFilters :: Lude.Maybe [ParameterStringFilter],
    -- | The hierarchy for the parameter. Hierarchies start with a forward slash (/) and end with the parameter name. A parameter name hierarchy can have a maximum of 15 levels. Here is an example of a hierarchy: @/Finance/Prod/IAD/WinServ2016/license33@
    path :: Lude.Text,
    -- | A token to start the list. Use this token to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Retrieve all parameters within a hierarchy.
    --
    -- /Important:/ If a user has access to a path, then the user can access all levels of that path. For example, if a user has permission to access path @/a@ , then the user can also access @/a/b@ . Even if a user has explicitly been denied access in IAM for parameter @/a/b@ , they can still call the GetParametersByPath API action recursively for @/a@ and view @/a/b@ .
    recursive :: Lude.Maybe Lude.Bool,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParametersByPath' with the minimum fields required to make a request.
--
-- * 'withDecryption' - Retrieve all parameters in a hierarchy with their value decrypted.
-- * 'parameterFilters' - Filters to limit the request results.
-- * 'path' - The hierarchy for the parameter. Hierarchies start with a forward slash (/) and end with the parameter name. A parameter name hierarchy can have a maximum of 15 levels. Here is an example of a hierarchy: @/Finance/Prod/IAD/WinServ2016/license33@
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
-- * 'recursive' - Retrieve all parameters within a hierarchy.
--
-- /Important:/ If a user has access to a path, then the user can access all levels of that path. For example, if a user has permission to access path @/a@ , then the user can also access @/a/b@ . Even if a user has explicitly been denied access in IAM for parameter @/a/b@ , they can still call the GetParametersByPath API action recursively for @/a@ and view @/a/b@ .
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkGetParametersByPath ::
  -- | 'path'
  Lude.Text ->
  GetParametersByPath
mkGetParametersByPath pPath_ =
  GetParametersByPath'
    { withDecryption = Lude.Nothing,
      parameterFilters = Lude.Nothing,
      path = pPath_,
      nextToken = Lude.Nothing,
      recursive = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Retrieve all parameters in a hierarchy with their value decrypted.
--
-- /Note:/ Consider using 'withDecryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpWithDecryption :: Lens.Lens' GetParametersByPath (Lude.Maybe Lude.Bool)
gpbpWithDecryption = Lens.lens (withDecryption :: GetParametersByPath -> Lude.Maybe Lude.Bool) (\s a -> s {withDecryption = a} :: GetParametersByPath)
{-# DEPRECATED gpbpWithDecryption "Use generic-lens or generic-optics with 'withDecryption' instead." #-}

-- | Filters to limit the request results.
--
-- /Note:/ Consider using 'parameterFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpParameterFilters :: Lens.Lens' GetParametersByPath (Lude.Maybe [ParameterStringFilter])
gpbpParameterFilters = Lens.lens (parameterFilters :: GetParametersByPath -> Lude.Maybe [ParameterStringFilter]) (\s a -> s {parameterFilters = a} :: GetParametersByPath)
{-# DEPRECATED gpbpParameterFilters "Use generic-lens or generic-optics with 'parameterFilters' instead." #-}

-- | The hierarchy for the parameter. Hierarchies start with a forward slash (/) and end with the parameter name. A parameter name hierarchy can have a maximum of 15 levels. Here is an example of a hierarchy: @/Finance/Prod/IAD/WinServ2016/license33@
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpPath :: Lens.Lens' GetParametersByPath Lude.Text
gpbpPath = Lens.lens (path :: GetParametersByPath -> Lude.Text) (\s a -> s {path = a} :: GetParametersByPath)
{-# DEPRECATED gpbpPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpNextToken :: Lens.Lens' GetParametersByPath (Lude.Maybe Lude.Text)
gpbpNextToken = Lens.lens (nextToken :: GetParametersByPath -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetParametersByPath)
{-# DEPRECATED gpbpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Retrieve all parameters within a hierarchy.
--
-- /Important:/ If a user has access to a path, then the user can access all levels of that path. For example, if a user has permission to access path @/a@ , then the user can also access @/a/b@ . Even if a user has explicitly been denied access in IAM for parameter @/a/b@ , they can still call the GetParametersByPath API action recursively for @/a@ and view @/a/b@ .
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpRecursive :: Lens.Lens' GetParametersByPath (Lude.Maybe Lude.Bool)
gpbpRecursive = Lens.lens (recursive :: GetParametersByPath -> Lude.Maybe Lude.Bool) (\s a -> s {recursive = a} :: GetParametersByPath)
{-# DEPRECATED gpbpRecursive "Use generic-lens or generic-optics with 'recursive' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbpMaxResults :: Lens.Lens' GetParametersByPath (Lude.Maybe Lude.Natural)
gpbpMaxResults = Lens.lens (maxResults :: GetParametersByPath -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetParametersByPath)
{-# DEPRECATED gpbpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetParametersByPath where
  page rq rs
    | Page.stop (rs Lens.^. gpbprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gpbprsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gpbpNextToken Lens..~ rs Lens.^. gpbprsNextToken

instance Lude.AWSRequest GetParametersByPath where
  type Rs GetParametersByPath = GetParametersByPathResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetParametersByPathResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Parameters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetParametersByPath where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetParametersByPath" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetParametersByPath where
  toJSON GetParametersByPath' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("WithDecryption" Lude..=) Lude.<$> withDecryption,
            ("ParameterFilters" Lude..=) Lude.<$> parameterFilters,
            Lude.Just ("Path" Lude..= path),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Recursive" Lude..=) Lude.<$> recursive,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetParametersByPath where
  toPath = Lude.const "/"

instance Lude.ToQuery GetParametersByPath where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetParametersByPathResponse' smart constructor.
data GetParametersByPathResponse = GetParametersByPathResponse'
  { -- | The token for the next set of items to return. Use this token to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of parameters found in the specified hierarchy.
    parameters :: Lude.Maybe [Parameter],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParametersByPathResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'parameters' - A list of parameters found in the specified hierarchy.
-- * 'responseStatus' - The response status code.
mkGetParametersByPathResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetParametersByPathResponse
mkGetParametersByPathResponse pResponseStatus_ =
  GetParametersByPathResponse'
    { nextToken = Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbprsNextToken :: Lens.Lens' GetParametersByPathResponse (Lude.Maybe Lude.Text)
gpbprsNextToken = Lens.lens (nextToken :: GetParametersByPathResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetParametersByPathResponse)
{-# DEPRECATED gpbprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of parameters found in the specified hierarchy.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbprsParameters :: Lens.Lens' GetParametersByPathResponse (Lude.Maybe [Parameter])
gpbprsParameters = Lens.lens (parameters :: GetParametersByPathResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: GetParametersByPathResponse)
{-# DEPRECATED gpbprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbprsResponseStatus :: Lens.Lens' GetParametersByPathResponse Lude.Int
gpbprsResponseStatus = Lens.lens (responseStatus :: GetParametersByPathResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetParametersByPathResponse)
{-# DEPRECATED gpbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
