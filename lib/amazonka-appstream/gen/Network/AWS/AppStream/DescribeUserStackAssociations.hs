{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeUserStackAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the UserStackAssociation objects. You must specify either or both of the following:
--
--
--     * The stack name
--
--
--     * The user name (email address of the user associated with the stack) and the authentication type for the user
--
--
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeUserStackAssociations
  ( -- * Creating a request
    DescribeUserStackAssociations (..),
    mkDescribeUserStackAssociations,

    -- ** Request lenses
    dusaUserName,
    dusaNextToken,
    dusaAuthenticationType,
    dusaMaxResults,
    dusaStackName,

    -- * Destructuring the response
    DescribeUserStackAssociationsResponse (..),
    mkDescribeUserStackAssociationsResponse,

    -- ** Response lenses
    dusarsUserStackAssociations,
    dusarsNextToken,
    dusarsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUserStackAssociations' smart constructor.
data DescribeUserStackAssociations = DescribeUserStackAssociations'
  { userName ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    nextToken ::
      Lude.Maybe Lude.Text,
    authenticationType ::
      Lude.Maybe AuthenticationType,
    maxResults ::
      Lude.Maybe Lude.Natural,
    stackName ::
      Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserStackAssociations' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication type for the user who is associated with the stack. You must specify USERPOOL.
-- * 'maxResults' - The maximum size of each page of results.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'stackName' - The name of the stack that is associated with the user.
-- * 'userName' - The email address of the user who is associated with the stack.
mkDescribeUserStackAssociations ::
  DescribeUserStackAssociations
mkDescribeUserStackAssociations =
  DescribeUserStackAssociations'
    { userName = Lude.Nothing,
      nextToken = Lude.Nothing,
      authenticationType = Lude.Nothing,
      maxResults = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | The email address of the user who is associated with the stack.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaUserName :: Lens.Lens' DescribeUserStackAssociations (Lude.Maybe (Lude.Sensitive Lude.Text))
dusaUserName = Lens.lens (userName :: DescribeUserStackAssociations -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {userName = a} :: DescribeUserStackAssociations)
{-# DEPRECATED dusaUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaNextToken :: Lens.Lens' DescribeUserStackAssociations (Lude.Maybe Lude.Text)
dusaNextToken = Lens.lens (nextToken :: DescribeUserStackAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeUserStackAssociations)
{-# DEPRECATED dusaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The authentication type for the user who is associated with the stack. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaAuthenticationType :: Lens.Lens' DescribeUserStackAssociations (Lude.Maybe AuthenticationType)
dusaAuthenticationType = Lens.lens (authenticationType :: DescribeUserStackAssociations -> Lude.Maybe AuthenticationType) (\s a -> s {authenticationType = a} :: DescribeUserStackAssociations)
{-# DEPRECATED dusaAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaMaxResults :: Lens.Lens' DescribeUserStackAssociations (Lude.Maybe Lude.Natural)
dusaMaxResults = Lens.lens (maxResults :: DescribeUserStackAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeUserStackAssociations)
{-# DEPRECATED dusaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the stack that is associated with the user.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusaStackName :: Lens.Lens' DescribeUserStackAssociations (Lude.Maybe Lude.Text)
dusaStackName = Lens.lens (stackName :: DescribeUserStackAssociations -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: DescribeUserStackAssociations)
{-# DEPRECATED dusaStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Page.AWSPager DescribeUserStackAssociations where
  page rq rs
    | Page.stop (rs Lens.^. dusarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dusarsUserStackAssociations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dusaNextToken Lens..~ rs Lens.^. dusarsNextToken

instance Lude.AWSRequest DescribeUserStackAssociations where
  type
    Rs DescribeUserStackAssociations =
      DescribeUserStackAssociationsResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserStackAssociationsResponse'
            Lude.<$> (x Lude..?> "UserStackAssociations")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserStackAssociations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.DescribeUserStackAssociations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUserStackAssociations where
  toJSON DescribeUserStackAssociations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserName" Lude..=) Lude.<$> userName,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("AuthenticationType" Lude..=) Lude.<$> authenticationType,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("StackName" Lude..=) Lude.<$> stackName
          ]
      )

instance Lude.ToPath DescribeUserStackAssociations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUserStackAssociations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUserStackAssociationsResponse' smart constructor.
data DescribeUserStackAssociationsResponse = DescribeUserStackAssociationsResponse'
  { userStackAssociations ::
      Lude.Maybe
        ( Lude.NonEmpty
            UserStackAssociation
        ),
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserStackAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'responseStatus' - The response status code.
-- * 'userStackAssociations' - The UserStackAssociation objects.
mkDescribeUserStackAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserStackAssociationsResponse
mkDescribeUserStackAssociationsResponse pResponseStatus_ =
  DescribeUserStackAssociationsResponse'
    { userStackAssociations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The UserStackAssociation objects.
--
-- /Note:/ Consider using 'userStackAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusarsUserStackAssociations :: Lens.Lens' DescribeUserStackAssociationsResponse (Lude.Maybe (Lude.NonEmpty UserStackAssociation))
dusarsUserStackAssociations = Lens.lens (userStackAssociations :: DescribeUserStackAssociationsResponse -> Lude.Maybe (Lude.NonEmpty UserStackAssociation)) (\s a -> s {userStackAssociations = a} :: DescribeUserStackAssociationsResponse)
{-# DEPRECATED dusarsUserStackAssociations "Use generic-lens or generic-optics with 'userStackAssociations' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusarsNextToken :: Lens.Lens' DescribeUserStackAssociationsResponse (Lude.Maybe Lude.Text)
dusarsNextToken = Lens.lens (nextToken :: DescribeUserStackAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeUserStackAssociationsResponse)
{-# DEPRECATED dusarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dusarsResponseStatus :: Lens.Lens' DescribeUserStackAssociationsResponse Lude.Int
dusarsResponseStatus = Lens.lens (responseStatus :: DescribeUserStackAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserStackAssociationsResponse)
{-# DEPRECATED dusarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
