{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DescribeEnvironmentMemberships
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about environment members for an AWS Cloud9 development environment.
--
-- This operation returns paginated results.
module Network.AWS.Cloud9.DescribeEnvironmentMemberships
  ( -- * Creating a request
    DescribeEnvironmentMemberships (..),
    mkDescribeEnvironmentMemberships,

    -- ** Request lenses
    dUserARN,
    dNextToken,
    dPermissions,
    dEnvironmentId,
    dMaxResults,

    -- * Destructuring the response
    DescribeEnvironmentMembershipsResponse (..),
    mkDescribeEnvironmentMembershipsResponse,

    -- ** Response lenses
    drsNextToken,
    drsMemberships,
    drsResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEnvironmentMemberships' smart constructor.
data DescribeEnvironmentMemberships = DescribeEnvironmentMemberships'
  { userARN ::
      Lude.Maybe Lude.Text,
    nextToken ::
      Lude.Maybe Lude.Text,
    permissions ::
      Lude.Maybe [Permissions],
    environmentId ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentMemberships' with the minimum fields required to make a request.
--
-- * 'environmentId' - The ID of the environment to get environment member information about.
-- * 'maxResults' - The maximum number of environment members to get information about.
-- * 'nextToken' - During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'permissions' - The type of environment member permissions to get information about. Available values include:
--
--
--     * @owner@ : Owns the environment.
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
-- If no value is specified, information about all environment members are returned.
-- * 'userARN' - The Amazon Resource Name (ARN) of an individual environment member to get information about. If no value is specified, information about all environment members are returned.
mkDescribeEnvironmentMemberships ::
  DescribeEnvironmentMemberships
mkDescribeEnvironmentMemberships =
  DescribeEnvironmentMemberships'
    { userARN = Lude.Nothing,
      nextToken = Lude.Nothing,
      permissions = Lude.Nothing,
      environmentId = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of an individual environment member to get information about. If no value is specified, information about all environment members are returned.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserARN :: Lens.Lens' DescribeEnvironmentMemberships (Lude.Maybe Lude.Text)
dUserARN = Lens.lens (userARN :: DescribeEnvironmentMemberships -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: DescribeEnvironmentMemberships)
{-# DEPRECATED dUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeEnvironmentMemberships (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeEnvironmentMemberships -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEnvironmentMemberships)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of environment member permissions to get information about. Available values include:
--
--
--     * @owner@ : Owns the environment.
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
-- If no value is specified, information about all environment members are returned.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPermissions :: Lens.Lens' DescribeEnvironmentMemberships (Lude.Maybe [Permissions])
dPermissions = Lens.lens (permissions :: DescribeEnvironmentMemberships -> Lude.Maybe [Permissions]) (\s a -> s {permissions = a} :: DescribeEnvironmentMemberships)
{-# DEPRECATED dPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The ID of the environment to get environment member information about.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEnvironmentId :: Lens.Lens' DescribeEnvironmentMemberships (Lude.Maybe Lude.Text)
dEnvironmentId = Lens.lens (environmentId :: DescribeEnvironmentMemberships -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: DescribeEnvironmentMemberships)
{-# DEPRECATED dEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The maximum number of environment members to get information about.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeEnvironmentMemberships (Lude.Maybe Lude.Natural)
dMaxResults = Lens.lens (maxResults :: DescribeEnvironmentMemberships -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeEnvironmentMemberships)
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeEnvironmentMemberships where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsMemberships) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeEnvironmentMemberships where
  type
    Rs DescribeEnvironmentMemberships =
      DescribeEnvironmentMembershipsResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEnvironmentMembershipsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "memberships" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEnvironmentMemberships where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.DescribeEnvironmentMemberships" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEnvironmentMemberships where
  toJSON DescribeEnvironmentMemberships' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("userArn" Lude..=) Lude.<$> userARN,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("permissions" Lude..=) Lude.<$> permissions,
            ("environmentId" Lude..=) Lude.<$> environmentId,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeEnvironmentMemberships where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEnvironmentMemberships where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEnvironmentMembershipsResponse' smart constructor.
data DescribeEnvironmentMembershipsResponse = DescribeEnvironmentMembershipsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    memberships ::
      Lude.Maybe
        [EnvironmentMember],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentMembershipsResponse' with the minimum fields required to make a request.
--
-- * 'memberships' - Information about the environment members for the environment.
-- * 'nextToken' - If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
-- * 'responseStatus' - The response status code.
mkDescribeEnvironmentMembershipsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEnvironmentMembershipsResponse
mkDescribeEnvironmentMembershipsResponse pResponseStatus_ =
  DescribeEnvironmentMembershipsResponse'
    { nextToken = Lude.Nothing,
      memberships = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeEnvironmentMembershipsResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeEnvironmentMembershipsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEnvironmentMembershipsResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the environment members for the environment.
--
-- /Note:/ Consider using 'memberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsMemberships :: Lens.Lens' DescribeEnvironmentMembershipsResponse (Lude.Maybe [EnvironmentMember])
drsMemberships = Lens.lens (memberships :: DescribeEnvironmentMembershipsResponse -> Lude.Maybe [EnvironmentMember]) (\s a -> s {memberships = a} :: DescribeEnvironmentMembershipsResponse)
{-# DEPRECATED drsMemberships "Use generic-lens or generic-optics with 'memberships' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeEnvironmentMembershipsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeEnvironmentMembershipsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEnvironmentMembershipsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
