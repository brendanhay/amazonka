{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.ListConfigurationRevisions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all revisions for the specified configuration.
module Network.AWS.MQ.ListConfigurationRevisions
  ( -- * Creating a request
    ListConfigurationRevisions (..),
    mkListConfigurationRevisions,

    -- ** Request lenses
    lcrConfigurationId,
    lcrNextToken,
    lcrMaxResults,

    -- * Destructuring the response
    ListConfigurationRevisionsResponse (..),
    mkListConfigurationRevisionsResponse,

    -- ** Response lenses
    lcrrsConfigurationId,
    lcrrsNextToken,
    lcrrsRevisions,
    lcrrsMaxResults,
    lcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListConfigurationRevisions' smart constructor.
data ListConfigurationRevisions = ListConfigurationRevisions'
  { -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Lude.Text,
    -- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConfigurationRevisions' with the minimum fields required to make a request.
--
-- * 'configurationId' - The unique ID that Amazon MQ generates for the configuration.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
-- * 'maxResults' - The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
mkListConfigurationRevisions ::
  -- | 'configurationId'
  Lude.Text ->
  ListConfigurationRevisions
mkListConfigurationRevisions pConfigurationId_ =
  ListConfigurationRevisions'
    { configurationId = pConfigurationId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrConfigurationId :: Lens.Lens' ListConfigurationRevisions Lude.Text
lcrConfigurationId = Lens.lens (configurationId :: ListConfigurationRevisions -> Lude.Text) (\s a -> s {configurationId = a} :: ListConfigurationRevisions)
{-# DEPRECATED lcrConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrNextToken :: Lens.Lens' ListConfigurationRevisions (Lude.Maybe Lude.Text)
lcrNextToken = Lens.lens (nextToken :: ListConfigurationRevisions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConfigurationRevisions)
{-# DEPRECATED lcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrMaxResults :: Lens.Lens' ListConfigurationRevisions (Lude.Maybe Lude.Natural)
lcrMaxResults = Lens.lens (maxResults :: ListConfigurationRevisions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListConfigurationRevisions)
{-# DEPRECATED lcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListConfigurationRevisions where
  type
    Rs ListConfigurationRevisions =
      ListConfigurationRevisionsResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListConfigurationRevisionsResponse'
            Lude.<$> (x Lude..?> "configurationId")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "revisions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "maxResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListConfigurationRevisions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListConfigurationRevisions where
  toPath ListConfigurationRevisions' {..} =
    Lude.mconcat
      ["/v1/configurations/", Lude.toBS configurationId, "/revisions"]

instance Lude.ToQuery ListConfigurationRevisions where
  toQuery ListConfigurationRevisions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListConfigurationRevisionsResponse' smart constructor.
data ListConfigurationRevisionsResponse = ListConfigurationRevisionsResponse'
  { -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Lude.Maybe Lude.Text,
    -- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of all revisions for the specified configuration.
    revisions :: Lude.Maybe [ConfigurationRevision],
    -- | The maximum number of configuration revisions that can be returned per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConfigurationRevisionsResponse' with the minimum fields required to make a request.
--
-- * 'configurationId' - The unique ID that Amazon MQ generates for the configuration.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
-- * 'revisions' - The list of all revisions for the specified configuration.
-- * 'maxResults' - The maximum number of configuration revisions that can be returned per page (20 by default). This value must be an integer from 5 to 100.
-- * 'responseStatus' - The response status code.
mkListConfigurationRevisionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListConfigurationRevisionsResponse
mkListConfigurationRevisionsResponse pResponseStatus_ =
  ListConfigurationRevisionsResponse'
    { configurationId =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      revisions = Lude.Nothing,
      maxResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsConfigurationId :: Lens.Lens' ListConfigurationRevisionsResponse (Lude.Maybe Lude.Text)
lcrrsConfigurationId = Lens.lens (configurationId :: ListConfigurationRevisionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {configurationId = a} :: ListConfigurationRevisionsResponse)
{-# DEPRECATED lcrrsConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListConfigurationRevisionsResponse (Lude.Maybe Lude.Text)
lcrrsNextToken = Lens.lens (nextToken :: ListConfigurationRevisionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConfigurationRevisionsResponse)
{-# DEPRECATED lcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of all revisions for the specified configuration.
--
-- /Note:/ Consider using 'revisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsRevisions :: Lens.Lens' ListConfigurationRevisionsResponse (Lude.Maybe [ConfigurationRevision])
lcrrsRevisions = Lens.lens (revisions :: ListConfigurationRevisionsResponse -> Lude.Maybe [ConfigurationRevision]) (\s a -> s {revisions = a} :: ListConfigurationRevisionsResponse)
{-# DEPRECATED lcrrsRevisions "Use generic-lens or generic-optics with 'revisions' instead." #-}

-- | The maximum number of configuration revisions that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsMaxResults :: Lens.Lens' ListConfigurationRevisionsResponse (Lude.Maybe Lude.Int)
lcrrsMaxResults = Lens.lens (maxResults :: ListConfigurationRevisionsResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListConfigurationRevisionsResponse)
{-# DEPRECATED lcrrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListConfigurationRevisionsResponse Lude.Int
lcrrsResponseStatus = Lens.lens (responseStatus :: ListConfigurationRevisionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListConfigurationRevisionsResponse)
{-# DEPRECATED lcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
