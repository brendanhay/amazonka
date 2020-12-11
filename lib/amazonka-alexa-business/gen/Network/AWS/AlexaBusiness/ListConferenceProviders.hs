{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListConferenceProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists conference providers under a specific AWS account.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListConferenceProviders
  ( -- * Creating a request
    ListConferenceProviders (..),
    mkListConferenceProviders,

    -- ** Request lenses
    lcpNextToken,
    lcpMaxResults,

    -- * Destructuring the response
    ListConferenceProvidersResponse (..),
    mkListConferenceProvidersResponse,

    -- ** Response lenses
    lcprsConferenceProviders,
    lcprsNextToken,
    lcprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListConferenceProviders' smart constructor.
data ListConferenceProviders = ListConferenceProviders'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConferenceProviders' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of conference providers to be returned, per paginated calls.
-- * 'nextToken' - The tokens used for pagination.
mkListConferenceProviders ::
  ListConferenceProviders
mkListConferenceProviders =
  ListConferenceProviders'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpNextToken :: Lens.Lens' ListConferenceProviders (Lude.Maybe Lude.Text)
lcpNextToken = Lens.lens (nextToken :: ListConferenceProviders -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConferenceProviders)
{-# DEPRECATED lcpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of conference providers to be returned, per paginated calls.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpMaxResults :: Lens.Lens' ListConferenceProviders (Lude.Maybe Lude.Natural)
lcpMaxResults = Lens.lens (maxResults :: ListConferenceProviders -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListConferenceProviders)
{-# DEPRECATED lcpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListConferenceProviders where
  page rq rs
    | Page.stop (rs Lens.^. lcprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcprsConferenceProviders) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcpNextToken Lens..~ rs Lens.^. lcprsNextToken

instance Lude.AWSRequest ListConferenceProviders where
  type Rs ListConferenceProviders = ListConferenceProvidersResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListConferenceProvidersResponse'
            Lude.<$> (x Lude..?> "ConferenceProviders" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListConferenceProviders where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ListConferenceProviders" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListConferenceProviders where
  toJSON ListConferenceProviders' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListConferenceProviders where
  toPath = Lude.const "/"

instance Lude.ToQuery ListConferenceProviders where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListConferenceProvidersResponse' smart constructor.
data ListConferenceProvidersResponse = ListConferenceProvidersResponse'
  { conferenceProviders ::
      Lude.Maybe
        [ConferenceProvider],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListConferenceProvidersResponse' with the minimum fields required to make a request.
--
-- * 'conferenceProviders' - The conference providers.
-- * 'nextToken' - The tokens used for pagination.
-- * 'responseStatus' - The response status code.
mkListConferenceProvidersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListConferenceProvidersResponse
mkListConferenceProvidersResponse pResponseStatus_ =
  ListConferenceProvidersResponse'
    { conferenceProviders =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The conference providers.
--
-- /Note:/ Consider using 'conferenceProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprsConferenceProviders :: Lens.Lens' ListConferenceProvidersResponse (Lude.Maybe [ConferenceProvider])
lcprsConferenceProviders = Lens.lens (conferenceProviders :: ListConferenceProvidersResponse -> Lude.Maybe [ConferenceProvider]) (\s a -> s {conferenceProviders = a} :: ListConferenceProvidersResponse)
{-# DEPRECATED lcprsConferenceProviders "Use generic-lens or generic-optics with 'conferenceProviders' instead." #-}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprsNextToken :: Lens.Lens' ListConferenceProvidersResponse (Lude.Maybe Lude.Text)
lcprsNextToken = Lens.lens (nextToken :: ListConferenceProvidersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConferenceProvidersResponse)
{-# DEPRECATED lcprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprsResponseStatus :: Lens.Lens' ListConferenceProvidersResponse Lude.Int
lcprsResponseStatus = Lens.lens (responseStatus :: ListConferenceProvidersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListConferenceProvidersResponse)
{-# DEPRECATED lcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
