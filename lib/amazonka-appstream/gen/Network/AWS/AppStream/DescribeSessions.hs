{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the streaming sessions for a specified stack and fleet. If a UserId is provided for the stack and fleet, only streaming sessions for that user are described. If an authentication type is not provided, the default is to authenticate users using a streaming URL.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeSessions
  ( -- * Creating a request
    DescribeSessions (..),
    mkDescribeSessions,

    -- ** Request lenses
    dsUserId,
    dsNextToken,
    dsLimit,
    dsAuthenticationType,
    dsStackName,
    dsFleetName,

    -- * Destructuring the response
    DescribeSessionsResponse (..),
    mkDescribeSessionsResponse,

    -- ** Response lenses
    dssrsNextToken,
    dssrsSessions,
    dssrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
  { userId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Int,
    authenticationType :: Lude.Maybe AuthenticationType,
    stackName :: Lude.Text,
    fleetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSessions' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication method. Specify @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user. The default is to authenticate users using a streaming URL.
-- * 'fleetName' - The name of the fleet. This value is case-sensitive.
-- * 'limit' - The size of each page of results. The default value is 20 and the maximum value is 50.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'stackName' - The name of the stack. This value is case-sensitive.
-- * 'userId' - The user identifier (ID). If you specify a user ID, you must also specify the authentication type.
mkDescribeSessions ::
  -- | 'stackName'
  Lude.Text ->
  -- | 'fleetName'
  Lude.Text ->
  DescribeSessions
mkDescribeSessions pStackName_ pFleetName_ =
  DescribeSessions'
    { userId = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      authenticationType = Lude.Nothing,
      stackName = pStackName_,
      fleetName = pFleetName_
    }

-- | The user identifier (ID). If you specify a user ID, you must also specify the authentication type.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsUserId :: Lens.Lens' DescribeSessions (Lude.Maybe Lude.Text)
dsUserId = Lens.lens (userId :: DescribeSessions -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: DescribeSessions)
{-# DEPRECATED dsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeSessions (Lude.Maybe Lude.Text)
dsNextToken = Lens.lens (nextToken :: DescribeSessions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSessions)
{-# DEPRECATED dsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The size of each page of results. The default value is 20 and the maximum value is 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimit :: Lens.Lens' DescribeSessions (Lude.Maybe Lude.Int)
dsLimit = Lens.lens (limit :: DescribeSessions -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: DescribeSessions)
{-# DEPRECATED dsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The authentication method. Specify @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user. The default is to authenticate users using a streaming URL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAuthenticationType :: Lens.Lens' DescribeSessions (Lude.Maybe AuthenticationType)
dsAuthenticationType = Lens.lens (authenticationType :: DescribeSessions -> Lude.Maybe AuthenticationType) (\s a -> s {authenticationType = a} :: DescribeSessions)
{-# DEPRECATED dsAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | The name of the stack. This value is case-sensitive.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStackName :: Lens.Lens' DescribeSessions Lude.Text
dsStackName = Lens.lens (stackName :: DescribeSessions -> Lude.Text) (\s a -> s {stackName = a} :: DescribeSessions)
{-# DEPRECATED dsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name of the fleet. This value is case-sensitive.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFleetName :: Lens.Lens' DescribeSessions Lude.Text
dsFleetName = Lens.lens (fleetName :: DescribeSessions -> Lude.Text) (\s a -> s {fleetName = a} :: DescribeSessions)
{-# DEPRECATED dsFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

instance Page.AWSPager DescribeSessions where
  page rq rs
    | Page.stop (rs Lens.^. dssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dssrsSessions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsNextToken Lens..~ rs Lens.^. dssrsNextToken

instance Lude.AWSRequest DescribeSessions where
  type Rs DescribeSessions = DescribeSessionsResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSessionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Sessions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSessions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DescribeSessions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSessions where
  toJSON DescribeSessions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserId" Lude..=) Lude.<$> userId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("AuthenticationType" Lude..=) Lude.<$> authenticationType,
            Lude.Just ("StackName" Lude..= stackName),
            Lude.Just ("FleetName" Lude..= fleetName)
          ]
      )

instance Lude.ToPath DescribeSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSessions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    sessions :: Lude.Maybe [Session],
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

-- | Creates a value of 'DescribeSessionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'responseStatus' - The response status code.
-- * 'sessions' - Information about the streaming sessions.
mkDescribeSessionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSessionsResponse
mkDescribeSessionsResponse pResponseStatus_ =
  DescribeSessionsResponse'
    { nextToken = Lude.Nothing,
      sessions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsNextToken :: Lens.Lens' DescribeSessionsResponse (Lude.Maybe Lude.Text)
dssrsNextToken = Lens.lens (nextToken :: DescribeSessionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSessionsResponse)
{-# DEPRECATED dssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the streaming sessions.
--
-- /Note:/ Consider using 'sessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsSessions :: Lens.Lens' DescribeSessionsResponse (Lude.Maybe [Session])
dssrsSessions = Lens.lens (sessions :: DescribeSessionsResponse -> Lude.Maybe [Session]) (\s a -> s {sessions = a} :: DescribeSessionsResponse)
{-# DEPRECATED dssrsSessions "Use generic-lens or generic-optics with 'sessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeSessionsResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSessionsResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
