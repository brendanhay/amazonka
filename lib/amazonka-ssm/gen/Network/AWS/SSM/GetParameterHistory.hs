{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetParameterHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the history of all changes to a parameter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetParameterHistory
  ( -- * Creating a request
    GetParameterHistory (..),
    mkGetParameterHistory,

    -- ** Request lenses
    gphWithDecryption,
    gphNextToken,
    gphMaxResults,
    gphName,

    -- * Destructuring the response
    GetParameterHistoryResponse (..),
    mkGetParameterHistoryResponse,

    -- ** Response lenses
    gphrsNextToken,
    gphrsParameters,
    gphrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetParameterHistory' smart constructor.
data GetParameterHistory = GetParameterHistory'
  { withDecryption ::
      Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParameterHistory' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'name' - The name of the parameter for which you want to review history.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'withDecryption' - Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
mkGetParameterHistory ::
  -- | 'name'
  Lude.Text ->
  GetParameterHistory
mkGetParameterHistory pName_ =
  GetParameterHistory'
    { withDecryption = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      name = pName_
    }

-- | Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
--
-- /Note:/ Consider using 'withDecryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphWithDecryption :: Lens.Lens' GetParameterHistory (Lude.Maybe Lude.Bool)
gphWithDecryption = Lens.lens (withDecryption :: GetParameterHistory -> Lude.Maybe Lude.Bool) (\s a -> s {withDecryption = a} :: GetParameterHistory)
{-# DEPRECATED gphWithDecryption "Use generic-lens or generic-optics with 'withDecryption' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphNextToken :: Lens.Lens' GetParameterHistory (Lude.Maybe Lude.Text)
gphNextToken = Lens.lens (nextToken :: GetParameterHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetParameterHistory)
{-# DEPRECATED gphNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphMaxResults :: Lens.Lens' GetParameterHistory (Lude.Maybe Lude.Natural)
gphMaxResults = Lens.lens (maxResults :: GetParameterHistory -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetParameterHistory)
{-# DEPRECATED gphMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the parameter for which you want to review history.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphName :: Lens.Lens' GetParameterHistory Lude.Text
gphName = Lens.lens (name :: GetParameterHistory -> Lude.Text) (\s a -> s {name = a} :: GetParameterHistory)
{-# DEPRECATED gphName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Page.AWSPager GetParameterHistory where
  page rq rs
    | Page.stop (rs Lens.^. gphrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gphrsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gphNextToken Lens..~ rs Lens.^. gphrsNextToken

instance Lude.AWSRequest GetParameterHistory where
  type Rs GetParameterHistory = GetParameterHistoryResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetParameterHistoryResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Parameters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetParameterHistory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetParameterHistory" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetParameterHistory where
  toJSON GetParameterHistory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("WithDecryption" Lude..=) Lude.<$> withDecryption,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath GetParameterHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery GetParameterHistory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetParameterHistoryResponse' smart constructor.
data GetParameterHistoryResponse = GetParameterHistoryResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    parameters ::
      Lude.Maybe [ParameterHistory],
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

-- | Creates a value of 'GetParameterHistoryResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'parameters' - A list of parameters returned by the request.
-- * 'responseStatus' - The response status code.
mkGetParameterHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetParameterHistoryResponse
mkGetParameterHistoryResponse pResponseStatus_ =
  GetParameterHistoryResponse'
    { nextToken = Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphrsNextToken :: Lens.Lens' GetParameterHistoryResponse (Lude.Maybe Lude.Text)
gphrsNextToken = Lens.lens (nextToken :: GetParameterHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetParameterHistoryResponse)
{-# DEPRECATED gphrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of parameters returned by the request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphrsParameters :: Lens.Lens' GetParameterHistoryResponse (Lude.Maybe [ParameterHistory])
gphrsParameters = Lens.lens (parameters :: GetParameterHistoryResponse -> Lude.Maybe [ParameterHistory]) (\s a -> s {parameters = a} :: GetParameterHistoryResponse)
{-# DEPRECATED gphrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphrsResponseStatus :: Lens.Lens' GetParameterHistoryResponse Lude.Int
gphrsResponseStatus = Lens.lens (responseStatus :: GetParameterHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetParameterHistoryResponse)
{-# DEPRECATED gphrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
