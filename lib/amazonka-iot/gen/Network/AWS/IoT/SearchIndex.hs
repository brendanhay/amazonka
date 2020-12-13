{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SearchIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The query search index.
module Network.AWS.IoT.SearchIndex
  ( -- * Creating a request
    SearchIndex (..),
    mkSearchIndex,

    -- ** Request lenses
    siQueryVersion,
    siNextToken,
    siQueryString,
    siMaxResults,
    siIndexName,

    -- * Destructuring the response
    SearchIndexResponse (..),
    mkSearchIndexResponse,

    -- ** Response lenses
    sirsThingGroups,
    sirsNextToken,
    sirsThings,
    sirsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchIndex' smart constructor.
data SearchIndex = SearchIndex'
  { -- | The query version.
    queryVersion :: Lude.Maybe Lude.Text,
    -- | The token used to get the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The search query string.
    queryString :: Lude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The search index name.
    indexName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchIndex' with the minimum fields required to make a request.
--
-- * 'queryVersion' - The query version.
-- * 'nextToken' - The token used to get the next set of results, or @null@ if there are no additional results.
-- * 'queryString' - The search query string.
-- * 'maxResults' - The maximum number of results to return at one time.
-- * 'indexName' - The search index name.
mkSearchIndex ::
  -- | 'queryString'
  Lude.Text ->
  SearchIndex
mkSearchIndex pQueryString_ =
  SearchIndex'
    { queryVersion = Lude.Nothing,
      nextToken = Lude.Nothing,
      queryString = pQueryString_,
      maxResults = Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | The query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siQueryVersion :: Lens.Lens' SearchIndex (Lude.Maybe Lude.Text)
siQueryVersion = Lens.lens (queryVersion :: SearchIndex -> Lude.Maybe Lude.Text) (\s a -> s {queryVersion = a} :: SearchIndex)
{-# DEPRECATED siQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The token used to get the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNextToken :: Lens.Lens' SearchIndex (Lude.Maybe Lude.Text)
siNextToken = Lens.lens (nextToken :: SearchIndex -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchIndex)
{-# DEPRECATED siNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The search query string.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siQueryString :: Lens.Lens' SearchIndex Lude.Text
siQueryString = Lens.lens (queryString :: SearchIndex -> Lude.Text) (\s a -> s {queryString = a} :: SearchIndex)
{-# DEPRECATED siQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siMaxResults :: Lens.Lens' SearchIndex (Lude.Maybe Lude.Natural)
siMaxResults = Lens.lens (maxResults :: SearchIndex -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchIndex)
{-# DEPRECATED siMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The search index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siIndexName :: Lens.Lens' SearchIndex (Lude.Maybe Lude.Text)
siIndexName = Lens.lens (indexName :: SearchIndex -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: SearchIndex)
{-# DEPRECATED siIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.AWSRequest SearchIndex where
  type Rs SearchIndex = SearchIndexResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchIndexResponse'
            Lude.<$> (x Lude..?> "thingGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "things" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchIndex where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SearchIndex where
  toJSON SearchIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryVersion" Lude..=) Lude.<$> queryVersion,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("queryString" Lude..= queryString),
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("indexName" Lude..=) Lude.<$> indexName
          ]
      )

instance Lude.ToPath SearchIndex where
  toPath = Lude.const "/indices/search"

instance Lude.ToQuery SearchIndex where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchIndexResponse' smart constructor.
data SearchIndexResponse = SearchIndexResponse'
  { -- | The thing groups that match the search query.
    thingGroups :: Lude.Maybe [ThingGroupDocument],
    -- | The token used to get the next set of results, or @null@ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The things that match the search query.
    things :: Lude.Maybe [ThingDocument],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchIndexResponse' with the minimum fields required to make a request.
--
-- * 'thingGroups' - The thing groups that match the search query.
-- * 'nextToken' - The token used to get the next set of results, or @null@ if there are no additional results.
-- * 'things' - The things that match the search query.
-- * 'responseStatus' - The response status code.
mkSearchIndexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchIndexResponse
mkSearchIndexResponse pResponseStatus_ =
  SearchIndexResponse'
    { thingGroups = Lude.Nothing,
      nextToken = Lude.Nothing,
      things = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The thing groups that match the search query.
--
-- /Note:/ Consider using 'thingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsThingGroups :: Lens.Lens' SearchIndexResponse (Lude.Maybe [ThingGroupDocument])
sirsThingGroups = Lens.lens (thingGroups :: SearchIndexResponse -> Lude.Maybe [ThingGroupDocument]) (\s a -> s {thingGroups = a} :: SearchIndexResponse)
{-# DEPRECATED sirsThingGroups "Use generic-lens or generic-optics with 'thingGroups' instead." #-}

-- | The token used to get the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsNextToken :: Lens.Lens' SearchIndexResponse (Lude.Maybe Lude.Text)
sirsNextToken = Lens.lens (nextToken :: SearchIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchIndexResponse)
{-# DEPRECATED sirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The things that match the search query.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsThings :: Lens.Lens' SearchIndexResponse (Lude.Maybe [ThingDocument])
sirsThings = Lens.lens (things :: SearchIndexResponse -> Lude.Maybe [ThingDocument]) (\s a -> s {things = a} :: SearchIndexResponse)
{-# DEPRECATED sirsThings "Use generic-lens or generic-optics with 'things' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsResponseStatus :: Lens.Lens' SearchIndexResponse Lude.Int
sirsResponseStatus = Lens.lens (responseStatus :: SearchIndexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchIndexResponse)
{-# DEPRECATED sirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
