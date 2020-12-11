{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the groups associated with a user pool.
--
-- Calling this action requires developer credentials.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListGroups
  ( -- * Creating a request
    ListGroups (..),
    mkListGroups,

    -- ** Request lenses
    lgNextToken,
    lgLimit,
    lgUserPoolId,

    -- * Destructuring the response
    ListGroupsResponse (..),
    mkListGroupsResponse,

    -- ** Response lenses
    lgrsGroups,
    lgrsNextToken,
    lgrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGroups' smart constructor.
data ListGroups = ListGroups'
  { nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    userPoolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- * 'limit' - The limit of the request to list groups.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'userPoolId' - The user pool ID for the user pool.
mkListGroups ::
  -- | 'userPoolId'
  Lude.Text ->
  ListGroups
mkListGroups pUserPoolId_ =
  ListGroups'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextToken :: Lens.Lens' ListGroups (Lude.Maybe Lude.Text)
lgNextToken = Lens.lens (nextToken :: ListGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroups)
{-# DEPRECATED lgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The limit of the request to list groups.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgLimit :: Lens.Lens' ListGroups (Lude.Maybe Lude.Natural)
lgLimit = Lens.lens (limit :: ListGroups -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListGroups)
{-# DEPRECATED lgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgUserPoolId :: Lens.Lens' ListGroups Lude.Text
lgUserPoolId = Lens.lens (userPoolId :: ListGroups -> Lude.Text) (\s a -> s {userPoolId = a} :: ListGroups)
{-# DEPRECATED lgUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Page.AWSPager ListGroups where
  page rq rs
    | Page.stop (rs Lens.^. lgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgrsGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgNextToken Lens..~ rs Lens.^. lgrsNextToken

instance Lude.AWSRequest ListGroups where
  type Rs ListGroups = ListGroupsResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            Lude.<$> (x Lude..?> "Groups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ListGroups" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGroups where
  toJSON ListGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath ListGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { groups ::
      Lude.Maybe [GroupType],
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

-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- * 'groups' - The group objects for the groups.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkListGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupsResponse
mkListGroupsResponse pResponseStatus_ =
  ListGroupsResponse'
    { groups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The group objects for the groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsGroups :: Lens.Lens' ListGroupsResponse (Lude.Maybe [GroupType])
lgrsGroups = Lens.lens (groups :: ListGroupsResponse -> Lude.Maybe [GroupType]) (\s a -> s {groups = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsNextToken :: Lens.Lens' ListGroupsResponse (Lude.Maybe Lude.Text)
lgrsNextToken = Lens.lens (nextToken :: ListGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsResponseStatus :: Lens.Lens' ListGroupsResponse Lude.Int
lgrsResponseStatus = Lens.lens (responseStatus :: ListGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
