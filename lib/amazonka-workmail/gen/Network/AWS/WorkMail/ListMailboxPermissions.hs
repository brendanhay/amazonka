{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListMailboxPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox permissions associated with a user, group, or resource mailbox.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListMailboxPermissions
  ( -- * Creating a request
    ListMailboxPermissions (..),
    mkListMailboxPermissions,

    -- ** Request lenses
    lmpNextToken,
    lmpMaxResults,
    lmpOrganizationId,
    lmpEntityId,

    -- * Destructuring the response
    ListMailboxPermissionsResponse (..),
    mkListMailboxPermissionsResponse,

    -- ** Response lenses
    lmprsNextToken,
    lmprsPermissions,
    lmprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListMailboxPermissions' smart constructor.
data ListMailboxPermissions = ListMailboxPermissions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    organizationId :: Lude.Text,
    entityId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMailboxPermissions' with the minimum fields required to make a request.
--
-- * 'entityId' - The identifier of the user, group, or resource for which to list mailbox permissions.
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'nextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
-- * 'organizationId' - The identifier of the organization under which the user, group, or resource exists.
mkListMailboxPermissions ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  ListMailboxPermissions
mkListMailboxPermissions pOrganizationId_ pEntityId_ =
  ListMailboxPermissions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      organizationId = pOrganizationId_,
      entityId = pEntityId_
    }

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpNextToken :: Lens.Lens' ListMailboxPermissions (Lude.Maybe Lude.Text)
lmpNextToken = Lens.lens (nextToken :: ListMailboxPermissions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMailboxPermissions)
{-# DEPRECATED lmpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpMaxResults :: Lens.Lens' ListMailboxPermissions (Lude.Maybe Lude.Natural)
lmpMaxResults = Lens.lens (maxResults :: ListMailboxPermissions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMailboxPermissions)
{-# DEPRECATED lmpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the organization under which the user, group, or resource exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpOrganizationId :: Lens.Lens' ListMailboxPermissions Lude.Text
lmpOrganizationId = Lens.lens (organizationId :: ListMailboxPermissions -> Lude.Text) (\s a -> s {organizationId = a} :: ListMailboxPermissions)
{-# DEPRECATED lmpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the user, group, or resource for which to list mailbox permissions.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpEntityId :: Lens.Lens' ListMailboxPermissions Lude.Text
lmpEntityId = Lens.lens (entityId :: ListMailboxPermissions -> Lude.Text) (\s a -> s {entityId = a} :: ListMailboxPermissions)
{-# DEPRECATED lmpEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

instance Page.AWSPager ListMailboxPermissions where
  page rq rs
    | Page.stop (rs Lens.^. lmprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmprsPermissions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmpNextToken Lens..~ rs Lens.^. lmprsNextToken

instance Lude.AWSRequest ListMailboxPermissions where
  type Rs ListMailboxPermissions = ListMailboxPermissionsResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMailboxPermissionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Permissions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMailboxPermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListMailboxPermissions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListMailboxPermissions where
  toJSON ListMailboxPermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("EntityId" Lude..= entityId)
          ]
      )

instance Lude.ToPath ListMailboxPermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMailboxPermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListMailboxPermissionsResponse' smart constructor.
data ListMailboxPermissionsResponse = ListMailboxPermissionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    permissions ::
      Lude.Maybe [Permission],
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

-- | Creates a value of 'ListMailboxPermissionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
-- * 'permissions' - One page of the user, group, or resource mailbox permissions.
-- * 'responseStatus' - The response status code.
mkListMailboxPermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMailboxPermissionsResponse
mkListMailboxPermissionsResponse pResponseStatus_ =
  ListMailboxPermissionsResponse'
    { nextToken = Lude.Nothing,
      permissions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprsNextToken :: Lens.Lens' ListMailboxPermissionsResponse (Lude.Maybe Lude.Text)
lmprsNextToken = Lens.lens (nextToken :: ListMailboxPermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMailboxPermissionsResponse)
{-# DEPRECATED lmprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One page of the user, group, or resource mailbox permissions.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprsPermissions :: Lens.Lens' ListMailboxPermissionsResponse (Lude.Maybe [Permission])
lmprsPermissions = Lens.lens (permissions :: ListMailboxPermissionsResponse -> Lude.Maybe [Permission]) (\s a -> s {permissions = a} :: ListMailboxPermissionsResponse)
{-# DEPRECATED lmprsPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprsResponseStatus :: Lens.Lens' ListMailboxPermissionsResponse Lude.Int
lmprsResponseStatus = Lens.lens (responseStatus :: ListMailboxPermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMailboxPermissionsResponse)
{-# DEPRECATED lmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
