{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListRegistries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registries that you have created, with minimal registry information. Registries in the @Deleting@ status will not be included in the results. Empty results will be returned if there are no registries available.
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListRegistries
  ( -- * Creating a request
    ListRegistries (..),
    mkListRegistries,

    -- ** Request lenses
    lrNextToken,
    lrMaxResults,

    -- * Destructuring the response
    ListRegistriesResponse (..),
    mkListRegistriesResponse,

    -- ** Response lenses
    lrrsRegistries,
    lrrsNextToken,
    lrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRegistries' smart constructor.
data ListRegistries = ListRegistries'
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

-- | Creates a value of 'ListRegistries' with the minimum fields required to make a request.
--
-- * 'maxResults' - Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
-- * 'nextToken' - A continuation token, if this is a continuation call.
mkListRegistries ::
  ListRegistries
mkListRegistries =
  ListRegistries'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRegistries (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListRegistries -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRegistries)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListRegistries (Lude.Maybe Lude.Natural)
lrMaxResults = Lens.lens (maxResults :: ListRegistries -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListRegistries)
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListRegistries where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsRegistries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListRegistries where
  type Rs ListRegistries = ListRegistriesResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRegistriesResponse'
            Lude.<$> (x Lude..?> "Registries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRegistries where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ListRegistries" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRegistries where
  toJSON ListRegistries' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListRegistries where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRegistries where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRegistriesResponse' smart constructor.
data ListRegistriesResponse = ListRegistriesResponse'
  { registries ::
      Lude.Maybe [RegistryListItem],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListRegistriesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
-- * 'registries' - An array of @RegistryDetailedListItem@ objects containing minimal details of each registry.
-- * 'responseStatus' - The response status code.
mkListRegistriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRegistriesResponse
mkListRegistriesResponse pResponseStatus_ =
  ListRegistriesResponse'
    { registries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @RegistryDetailedListItem@ objects containing minimal details of each registry.
--
-- /Note:/ Consider using 'registries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsRegistries :: Lens.Lens' ListRegistriesResponse (Lude.Maybe [RegistryListItem])
lrrsRegistries = Lens.lens (registries :: ListRegistriesResponse -> Lude.Maybe [RegistryListItem]) (\s a -> s {registries = a} :: ListRegistriesResponse)
{-# DEPRECATED lrrsRegistries "Use generic-lens or generic-optics with 'registries' instead." #-}

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListRegistriesResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListRegistriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRegistriesResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListRegistriesResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListRegistriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRegistriesResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
