{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the permissions for shared AWS account IDs on a private image that you own.
module Network.AWS.AppStream.DescribeImagePermissions
  ( -- * Creating a request
    DescribeImagePermissions (..),
    mkDescribeImagePermissions,

    -- ** Request lenses
    dipsNextToken,
    dipsSharedAWSAccountIds,
    dipsMaxResults,
    dipsName,

    -- * Destructuring the response
    DescribeImagePermissionsResponse (..),
    mkDescribeImagePermissionsResponse,

    -- ** Response lenses
    dipsrsSharedImagePermissionsList,
    dipsrsNextToken,
    dipsrsName,
    dipsrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeImagePermissions' smart constructor.
data DescribeImagePermissions = DescribeImagePermissions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    sharedAWSAccountIds ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    maxResults :: Lude.Maybe Lude.Natural,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImagePermissions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum size of each page of results.
-- * 'name' - The name of the private image for which to describe permissions. The image must be one that you own.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'sharedAWSAccountIds' - The 12-digit identifier of one or more AWS accounts with which the image is shared.
mkDescribeImagePermissions ::
  -- | 'name'
  Lude.Text ->
  DescribeImagePermissions
mkDescribeImagePermissions pName_ =
  DescribeImagePermissions'
    { nextToken = Lude.Nothing,
      sharedAWSAccountIds = Lude.Nothing,
      maxResults = Lude.Nothing,
      name = pName_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsNextToken :: Lens.Lens' DescribeImagePermissions (Lude.Maybe Lude.Text)
dipsNextToken = Lens.lens (nextToken :: DescribeImagePermissions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImagePermissions)
{-# DEPRECATED dipsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The 12-digit identifier of one or more AWS accounts with which the image is shared.
--
-- /Note:/ Consider using 'sharedAWSAccountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsSharedAWSAccountIds :: Lens.Lens' DescribeImagePermissions (Lude.Maybe (Lude.NonEmpty Lude.Text))
dipsSharedAWSAccountIds = Lens.lens (sharedAWSAccountIds :: DescribeImagePermissions -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {sharedAWSAccountIds = a} :: DescribeImagePermissions)
{-# DEPRECATED dipsSharedAWSAccountIds "Use generic-lens or generic-optics with 'sharedAWSAccountIds' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsMaxResults :: Lens.Lens' DescribeImagePermissions (Lude.Maybe Lude.Natural)
dipsMaxResults = Lens.lens (maxResults :: DescribeImagePermissions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeImagePermissions)
{-# DEPRECATED dipsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the private image for which to describe permissions. The image must be one that you own.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsName :: Lens.Lens' DescribeImagePermissions Lude.Text
dipsName = Lens.lens (name :: DescribeImagePermissions -> Lude.Text) (\s a -> s {name = a} :: DescribeImagePermissions)
{-# DEPRECATED dipsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribeImagePermissions where
  type Rs DescribeImagePermissions = DescribeImagePermissionsResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeImagePermissionsResponse'
            Lude.<$> (x Lude..?> "SharedImagePermissionsList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImagePermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.DescribeImagePermissions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeImagePermissions where
  toJSON DescribeImagePermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SharedAwsAccountIds" Lude..=) Lude.<$> sharedAWSAccountIds,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath DescribeImagePermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImagePermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeImagePermissionsResponse' smart constructor.
data DescribeImagePermissionsResponse = DescribeImagePermissionsResponse'
  { sharedImagePermissionsList ::
      Lude.Maybe
        [SharedImagePermissions],
    nextToken ::
      Lude.Maybe Lude.Text,
    name ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeImagePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the private image.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'responseStatus' - The response status code.
-- * 'sharedImagePermissionsList' - The permissions for a private image that you own.
mkDescribeImagePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImagePermissionsResponse
mkDescribeImagePermissionsResponse pResponseStatus_ =
  DescribeImagePermissionsResponse'
    { sharedImagePermissionsList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The permissions for a private image that you own.
--
-- /Note:/ Consider using 'sharedImagePermissionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrsSharedImagePermissionsList :: Lens.Lens' DescribeImagePermissionsResponse (Lude.Maybe [SharedImagePermissions])
dipsrsSharedImagePermissionsList = Lens.lens (sharedImagePermissionsList :: DescribeImagePermissionsResponse -> Lude.Maybe [SharedImagePermissions]) (\s a -> s {sharedImagePermissionsList = a} :: DescribeImagePermissionsResponse)
{-# DEPRECATED dipsrsSharedImagePermissionsList "Use generic-lens or generic-optics with 'sharedImagePermissionsList' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrsNextToken :: Lens.Lens' DescribeImagePermissionsResponse (Lude.Maybe Lude.Text)
dipsrsNextToken = Lens.lens (nextToken :: DescribeImagePermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImagePermissionsResponse)
{-# DEPRECATED dipsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the private image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrsName :: Lens.Lens' DescribeImagePermissionsResponse (Lude.Maybe Lude.Text)
dipsrsName = Lens.lens (name :: DescribeImagePermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeImagePermissionsResponse)
{-# DEPRECATED dipsrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrsResponseStatus :: Lens.Lens' DescribeImagePermissionsResponse Lude.Int
dipsrsResponseStatus = Lens.lens (responseStatus :: DescribeImagePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImagePermissionsResponse)
{-# DEPRECATED dipsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
