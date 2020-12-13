{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dssUserId,
    dssNextToken,
    dssLimit,
    dssAuthenticationType,
    dssFleetName,
    dssStackName,

    -- * Destructuring the response
    DescribeSessionsResponse (..),
    mkDescribeSessionsResponse,

    -- ** Response lenses
    dsrsNextToken,
    dsrsSessions,
    dsrsResponseStatus,
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
  { -- | The user identifier (ID). If you specify a user ID, you must also specify the authentication type.
    userId :: Lude.Maybe Lude.Text,
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The size of each page of results. The default value is 20 and the maximum value is 50.
    limit :: Lude.Maybe Lude.Int,
    -- | The authentication method. Specify @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user. The default is to authenticate users using a streaming URL.
    authenticationType :: Lude.Maybe AuthenticationType,
    -- | The name of the fleet. This value is case-sensitive.
    fleetName :: Lude.Text,
    -- | The name of the stack. This value is case-sensitive.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSessions' with the minimum fields required to make a request.
--
-- * 'userId' - The user identifier (ID). If you specify a user ID, you must also specify the authentication type.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'limit' - The size of each page of results. The default value is 20 and the maximum value is 50.
-- * 'authenticationType' - The authentication method. Specify @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user. The default is to authenticate users using a streaming URL.
-- * 'fleetName' - The name of the fleet. This value is case-sensitive.
-- * 'stackName' - The name of the stack. This value is case-sensitive.
mkDescribeSessions ::
  -- | 'fleetName'
  Lude.Text ->
  -- | 'stackName'
  Lude.Text ->
  DescribeSessions
mkDescribeSessions pFleetName_ pStackName_ =
  DescribeSessions'
    { userId = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      authenticationType = Lude.Nothing,
      fleetName = pFleetName_,
      stackName = pStackName_
    }

-- | The user identifier (ID). If you specify a user ID, you must also specify the authentication type.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssUserId :: Lens.Lens' DescribeSessions (Lude.Maybe Lude.Text)
dssUserId = Lens.lens (userId :: DescribeSessions -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: DescribeSessions)
{-# DEPRECATED dssUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssNextToken :: Lens.Lens' DescribeSessions (Lude.Maybe Lude.Text)
dssNextToken = Lens.lens (nextToken :: DescribeSessions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSessions)
{-# DEPRECATED dssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The size of each page of results. The default value is 20 and the maximum value is 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssLimit :: Lens.Lens' DescribeSessions (Lude.Maybe Lude.Int)
dssLimit = Lens.lens (limit :: DescribeSessions -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: DescribeSessions)
{-# DEPRECATED dssLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The authentication method. Specify @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user. The default is to authenticate users using a streaming URL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssAuthenticationType :: Lens.Lens' DescribeSessions (Lude.Maybe AuthenticationType)
dssAuthenticationType = Lens.lens (authenticationType :: DescribeSessions -> Lude.Maybe AuthenticationType) (\s a -> s {authenticationType = a} :: DescribeSessions)
{-# DEPRECATED dssAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | The name of the fleet. This value is case-sensitive.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssFleetName :: Lens.Lens' DescribeSessions Lude.Text
dssFleetName = Lens.lens (fleetName :: DescribeSessions -> Lude.Text) (\s a -> s {fleetName = a} :: DescribeSessions)
{-# DEPRECATED dssFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The name of the stack. This value is case-sensitive.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssStackName :: Lens.Lens' DescribeSessions Lude.Text
dssStackName = Lens.lens (stackName :: DescribeSessions -> Lude.Text) (\s a -> s {stackName = a} :: DescribeSessions)
{-# DEPRECATED dssStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Page.AWSPager DescribeSessions where
  page rq rs
    | Page.stop (rs Lens.^. dsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsrsSessions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dssNextToken Lens..~ rs Lens.^. dsrsNextToken

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
            Lude.Just ("FleetName" Lude..= fleetName),
            Lude.Just ("StackName" Lude..= stackName)
          ]
      )

instance Lude.ToPath DescribeSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSessions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the streaming sessions.
    sessions :: Lude.Maybe [Session],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSessionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'sessions' - Information about the streaming sessions.
-- * 'responseStatus' - The response status code.
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
dsrsNextToken :: Lens.Lens' DescribeSessionsResponse (Lude.Maybe Lude.Text)
dsrsNextToken = Lens.lens (nextToken :: DescribeSessionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSessionsResponse)
{-# DEPRECATED dsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the streaming sessions.
--
-- /Note:/ Consider using 'sessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSessions :: Lens.Lens' DescribeSessionsResponse (Lude.Maybe [Session])
dsrsSessions = Lens.lens (sessions :: DescribeSessionsResponse -> Lude.Maybe [Session]) (\s a -> s {sessions = a} :: DescribeSessionsResponse)
{-# DEPRECATED dsrsSessions "Use generic-lens or generic-optics with 'sessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeSessionsResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSessionsResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
