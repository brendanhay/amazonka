{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists a history of user activity and any risks detected as part of Amazon Cognito advanced security.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents
  ( -- * Creating a request
    AdminListUserAuthEvents (..),
    mkAdminListUserAuthEvents,

    -- ** Request lenses
    aluaeNextToken,
    aluaeMaxResults,
    aluaeUserPoolId,
    aluaeUsername,

    -- * Destructuring the response
    AdminListUserAuthEventsResponse (..),
    mkAdminListUserAuthEventsResponse,

    -- ** Response lenses
    aluaersNextToken,
    aluaersAuthEvents,
    aluaersResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdminListUserAuthEvents' smart constructor.
data AdminListUserAuthEvents = AdminListUserAuthEvents'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    userPoolId :: Lude.Text,
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminListUserAuthEvents' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of authentication events to return.
-- * 'nextToken' - A pagination token.
-- * 'userPoolId' - The user pool ID.
-- * 'username' - The user pool username or an alias.
mkAdminListUserAuthEvents ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminListUserAuthEvents
mkAdminListUserAuthEvents pUserPoolId_ pUsername_ =
  AdminListUserAuthEvents'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      userPoolId = pUserPoolId_,
      username = pUsername_
    }

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeNextToken :: Lens.Lens' AdminListUserAuthEvents (Lude.Maybe Lude.Text)
aluaeNextToken = Lens.lens (nextToken :: AdminListUserAuthEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: AdminListUserAuthEvents)
{-# DEPRECATED aluaeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of authentication events to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeMaxResults :: Lens.Lens' AdminListUserAuthEvents (Lude.Maybe Lude.Natural)
aluaeMaxResults = Lens.lens (maxResults :: AdminListUserAuthEvents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: AdminListUserAuthEvents)
{-# DEPRECATED aluaeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeUserPoolId :: Lens.Lens' AdminListUserAuthEvents Lude.Text
aluaeUserPoolId = Lens.lens (userPoolId :: AdminListUserAuthEvents -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminListUserAuthEvents)
{-# DEPRECATED aluaeUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user pool username or an alias.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeUsername :: Lens.Lens' AdminListUserAuthEvents (Lude.Sensitive Lude.Text)
aluaeUsername = Lens.lens (username :: AdminListUserAuthEvents -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminListUserAuthEvents)
{-# DEPRECATED aluaeUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Page.AWSPager AdminListUserAuthEvents where
  page rq rs
    | Page.stop (rs Lens.^. aluaersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. aluaersAuthEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& aluaeNextToken Lens..~ rs Lens.^. aluaersNextToken

instance Lude.AWSRequest AdminListUserAuthEvents where
  type Rs AdminListUserAuthEvents = AdminListUserAuthEventsResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          AdminListUserAuthEventsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AuthEvents" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminListUserAuthEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminListUserAuthEvents" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminListUserAuthEvents where
  toJSON AdminListUserAuthEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath AdminListUserAuthEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminListUserAuthEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminListUserAuthEventsResponse' smart constructor.
data AdminListUserAuthEventsResponse = AdminListUserAuthEventsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    authEvents ::
      Lude.Maybe [AuthEventType],
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

-- | Creates a value of 'AdminListUserAuthEventsResponse' with the minimum fields required to make a request.
--
-- * 'authEvents' - The response object. It includes the @EventID@ , @EventType@ , @CreationDate@ , @EventRisk@ , and @EventResponse@ .
-- * 'nextToken' - A pagination token.
-- * 'responseStatus' - The response status code.
mkAdminListUserAuthEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminListUserAuthEventsResponse
mkAdminListUserAuthEventsResponse pResponseStatus_ =
  AdminListUserAuthEventsResponse'
    { nextToken = Lude.Nothing,
      authEvents = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaersNextToken :: Lens.Lens' AdminListUserAuthEventsResponse (Lude.Maybe Lude.Text)
aluaersNextToken = Lens.lens (nextToken :: AdminListUserAuthEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: AdminListUserAuthEventsResponse)
{-# DEPRECATED aluaersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response object. It includes the @EventID@ , @EventType@ , @CreationDate@ , @EventRisk@ , and @EventResponse@ .
--
-- /Note:/ Consider using 'authEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaersAuthEvents :: Lens.Lens' AdminListUserAuthEventsResponse (Lude.Maybe [AuthEventType])
aluaersAuthEvents = Lens.lens (authEvents :: AdminListUserAuthEventsResponse -> Lude.Maybe [AuthEventType]) (\s a -> s {authEvents = a} :: AdminListUserAuthEventsResponse)
{-# DEPRECATED aluaersAuthEvents "Use generic-lens or generic-optics with 'authEvents' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaersResponseStatus :: Lens.Lens' AdminListUserAuthEventsResponse Lude.Int
aluaersResponseStatus = Lens.lens (responseStatus :: AdminListUserAuthEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminListUserAuthEventsResponse)
{-# DEPRECATED aluaersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
