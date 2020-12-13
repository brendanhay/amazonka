{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions that the owner of a connection alias has granted to another AWS account for the specified connection alias. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
  ( -- * Creating a request
    DescribeConnectionAliasPermissions (..),
    mkDescribeConnectionAliasPermissions,

    -- ** Request lenses
    dcapAliasId,
    dcapNextToken,
    dcapMaxResults,

    -- * Destructuring the response
    DescribeConnectionAliasPermissionsResponse (..),
    mkDescribeConnectionAliasPermissionsResponse,

    -- ** Response lenses
    dcaprsAliasId,
    dcaprsNextToken,
    dcaprsConnectionAliasPermissions,
    dcaprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeConnectionAliasPermissions' smart constructor.
data DescribeConnectionAliasPermissions = DescribeConnectionAliasPermissions'
  { -- | The identifier of the connection alias.
    aliasId :: Lude.Text,
    -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConnectionAliasPermissions' with the minimum fields required to make a request.
--
-- * 'aliasId' - The identifier of the connection alias.
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
-- * 'maxResults' - The maximum number of results to return.
mkDescribeConnectionAliasPermissions ::
  -- | 'aliasId'
  Lude.Text ->
  DescribeConnectionAliasPermissions
mkDescribeConnectionAliasPermissions pAliasId_ =
  DescribeConnectionAliasPermissions'
    { aliasId = pAliasId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcapAliasId :: Lens.Lens' DescribeConnectionAliasPermissions Lude.Text
dcapAliasId = Lens.lens (aliasId :: DescribeConnectionAliasPermissions -> Lude.Text) (\s a -> s {aliasId = a} :: DescribeConnectionAliasPermissions)
{-# DEPRECATED dcapAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcapNextToken :: Lens.Lens' DescribeConnectionAliasPermissions (Lude.Maybe Lude.Text)
dcapNextToken = Lens.lens (nextToken :: DescribeConnectionAliasPermissions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConnectionAliasPermissions)
{-# DEPRECATED dcapNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcapMaxResults :: Lens.Lens' DescribeConnectionAliasPermissions (Lude.Maybe Lude.Natural)
dcapMaxResults = Lens.lens (maxResults :: DescribeConnectionAliasPermissions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeConnectionAliasPermissions)
{-# DEPRECATED dcapMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeConnectionAliasPermissions where
  type
    Rs DescribeConnectionAliasPermissions =
      DescribeConnectionAliasPermissionsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConnectionAliasPermissionsResponse'
            Lude.<$> (x Lude..?> "AliasId")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ConnectionAliasPermissions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConnectionAliasPermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.DescribeConnectionAliasPermissions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConnectionAliasPermissions where
  toJSON DescribeConnectionAliasPermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AliasId" Lude..= aliasId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeConnectionAliasPermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConnectionAliasPermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConnectionAliasPermissionsResponse' smart constructor.
data DescribeConnectionAliasPermissionsResponse = DescribeConnectionAliasPermissionsResponse'
  { -- | The identifier of the connection alias.
    aliasId :: Lude.Maybe Lude.Text,
    -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The permissions associated with a connection alias.
    connectionAliasPermissions :: Lude.Maybe (Lude.NonEmpty ConnectionAliasPermission),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConnectionAliasPermissionsResponse' with the minimum fields required to make a request.
--
-- * 'aliasId' - The identifier of the connection alias.
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'connectionAliasPermissions' - The permissions associated with a connection alias.
-- * 'responseStatus' - The response status code.
mkDescribeConnectionAliasPermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConnectionAliasPermissionsResponse
mkDescribeConnectionAliasPermissionsResponse pResponseStatus_ =
  DescribeConnectionAliasPermissionsResponse'
    { aliasId =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      connectionAliasPermissions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaprsAliasId :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Lude.Maybe Lude.Text)
dcaprsAliasId = Lens.lens (aliasId :: DescribeConnectionAliasPermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {aliasId = a} :: DescribeConnectionAliasPermissionsResponse)
{-# DEPRECATED dcaprsAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaprsNextToken :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Lude.Maybe Lude.Text)
dcaprsNextToken = Lens.lens (nextToken :: DescribeConnectionAliasPermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConnectionAliasPermissionsResponse)
{-# DEPRECATED dcaprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The permissions associated with a connection alias.
--
-- /Note:/ Consider using 'connectionAliasPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaprsConnectionAliasPermissions :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Lude.Maybe (Lude.NonEmpty ConnectionAliasPermission))
dcaprsConnectionAliasPermissions = Lens.lens (connectionAliasPermissions :: DescribeConnectionAliasPermissionsResponse -> Lude.Maybe (Lude.NonEmpty ConnectionAliasPermission)) (\s a -> s {connectionAliasPermissions = a} :: DescribeConnectionAliasPermissionsResponse)
{-# DEPRECATED dcaprsConnectionAliasPermissions "Use generic-lens or generic-optics with 'connectionAliasPermissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaprsResponseStatus :: Lens.Lens' DescribeConnectionAliasPermissionsResponse Lude.Int
dcaprsResponseStatus = Lens.lens (responseStatus :: DescribeConnectionAliasPermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConnectionAliasPermissionsResponse)
{-# DEPRECATED dcaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
