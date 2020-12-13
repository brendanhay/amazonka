{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a paginated call to list the aliases associated with a given entity.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListAliases
  ( -- * Creating a request
    ListAliases (..),
    mkListAliases,

    -- ** Request lenses
    laNextToken,
    laEntityId,
    laMaxResults,
    laOrganizationId,

    -- * Destructuring the response
    ListAliasesResponse (..),
    mkListAliasesResponse,

    -- ** Response lenses
    larsAliases,
    larsNextToken,
    larsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The identifier for the entity for which to list the aliases.
    entityId :: Lude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The identifier for the organization under which the entity exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAliases' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
-- * 'entityId' - The identifier for the entity for which to list the aliases.
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'organizationId' - The identifier for the organization under which the entity exists.
mkListAliases ::
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  ListAliases
mkListAliases pEntityId_ pOrganizationId_ =
  ListAliases'
    { nextToken = Lude.Nothing,
      entityId = pEntityId_,
      maxResults = Lude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAliases (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListAliases -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAliases)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The identifier for the entity for which to list the aliases.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laEntityId :: Lens.Lens' ListAliases Lude.Text
laEntityId = Lens.lens (entityId :: ListAliases -> Lude.Text) (\s a -> s {entityId = a} :: ListAliases)
{-# DEPRECATED laEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListAliases (Lude.Maybe Lude.Natural)
laMaxResults = Lens.lens (maxResults :: ListAliases -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAliases)
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier for the organization under which the entity exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laOrganizationId :: Lens.Lens' ListAliases Lude.Text
laOrganizationId = Lens.lens (organizationId :: ListAliases -> Lude.Text) (\s a -> s {organizationId = a} :: ListAliases)
{-# DEPRECATED laOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Page.AWSPager ListAliases where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsAliases) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListAliases where
  type Rs ListAliases = ListAliasesResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Lude.<$> (x Lude..?> "Aliases" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAliases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListAliases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("EntityId" Lude..= entityId),
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath ListAliases where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAliases where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | The entity's paginated aliases.
    aliases :: Lude.Maybe [Lude.Text],
    -- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAliasesResponse' with the minimum fields required to make a request.
--
-- * 'aliases' - The entity's paginated aliases.
-- * 'nextToken' - The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListAliasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAliasesResponse
mkListAliasesResponse pResponseStatus_ =
  ListAliasesResponse'
    { aliases = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The entity's paginated aliases.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsAliases :: Lens.Lens' ListAliasesResponse (Lude.Maybe [Lude.Text])
larsAliases = Lens.lens (aliases :: ListAliasesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {aliases = a} :: ListAliasesResponse)
{-# DEPRECATED larsAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListAliasesResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListAliasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAliasesResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListAliasesResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListAliasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAliasesResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
