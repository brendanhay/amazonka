{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeploymentGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment groups for an application registered with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentGroups
  ( -- * Creating a request
    ListDeploymentGroups (..),
    mkListDeploymentGroups,

    -- ** Request lenses
    ldgNextToken,
    ldgApplicationName,

    -- * Destructuring the response
    ListDeploymentGroupsResponse (..),
    mkListDeploymentGroupsResponse,

    -- ** Response lenses
    ldgrsNextToken,
    ldgrsApplicationName,
    ldgrsDeploymentGroups,
    ldgrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'mkListDeploymentGroups' smart constructor.
data ListDeploymentGroups = ListDeploymentGroups'
  { nextToken ::
      Lude.Maybe Lude.Text,
    applicationName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeploymentGroups' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
-- * 'nextToken' - An identifier returned from the previous list deployment groups call. It can be used to return the next set of deployment groups in the list.
mkListDeploymentGroups ::
  -- | 'applicationName'
  Lude.Text ->
  ListDeploymentGroups
mkListDeploymentGroups pApplicationName_ =
  ListDeploymentGroups'
    { nextToken = Lude.Nothing,
      applicationName = pApplicationName_
    }

-- | An identifier returned from the previous list deployment groups call. It can be used to return the next set of deployment groups in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgNextToken :: Lens.Lens' ListDeploymentGroups (Lude.Maybe Lude.Text)
ldgNextToken = Lens.lens (nextToken :: ListDeploymentGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeploymentGroups)
{-# DEPRECATED ldgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgApplicationName :: Lens.Lens' ListDeploymentGroups Lude.Text
ldgApplicationName = Lens.lens (applicationName :: ListDeploymentGroups -> Lude.Text) (\s a -> s {applicationName = a} :: ListDeploymentGroups)
{-# DEPRECATED ldgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Page.AWSPager ListDeploymentGroups where
  page rq rs
    | Page.stop (rs Lens.^. ldgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldgrsDeploymentGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldgNextToken Lens..~ rs Lens.^. ldgrsNextToken

instance Lude.AWSRequest ListDeploymentGroups where
  type Rs ListDeploymentGroups = ListDeploymentGroupsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeploymentGroupsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "applicationName")
            Lude.<*> (x Lude..?> "deploymentGroups" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeploymentGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.ListDeploymentGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDeploymentGroups where
  toJSON ListDeploymentGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("applicationName" Lude..= applicationName)
          ]
      )

instance Lude.ToPath ListDeploymentGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDeploymentGroups where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'mkListDeploymentGroupsResponse' smart constructor.
data ListDeploymentGroupsResponse = ListDeploymentGroupsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    applicationName ::
      Lude.Maybe Lude.Text,
    deploymentGroups ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListDeploymentGroupsResponse' with the minimum fields required to make a request.
--
-- * 'applicationName' - The application name.
-- * 'deploymentGroups' - A list of deployment group names.
-- * 'nextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment groups call to return the next set of deployment groups in the list.
-- * 'responseStatus' - The response status code.
mkListDeploymentGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeploymentGroupsResponse
mkListDeploymentGroupsResponse pResponseStatus_ =
  ListDeploymentGroupsResponse'
    { nextToken = Lude.Nothing,
      applicationName = Lude.Nothing,
      deploymentGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment groups call to return the next set of deployment groups in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgrsNextToken :: Lens.Lens' ListDeploymentGroupsResponse (Lude.Maybe Lude.Text)
ldgrsNextToken = Lens.lens (nextToken :: ListDeploymentGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeploymentGroupsResponse)
{-# DEPRECATED ldgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgrsApplicationName :: Lens.Lens' ListDeploymentGroupsResponse (Lude.Maybe Lude.Text)
ldgrsApplicationName = Lens.lens (applicationName :: ListDeploymentGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: ListDeploymentGroupsResponse)
{-# DEPRECATED ldgrsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | A list of deployment group names.
--
-- /Note:/ Consider using 'deploymentGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgrsDeploymentGroups :: Lens.Lens' ListDeploymentGroupsResponse (Lude.Maybe [Lude.Text])
ldgrsDeploymentGroups = Lens.lens (deploymentGroups :: ListDeploymentGroupsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {deploymentGroups = a} :: ListDeploymentGroupsResponse)
{-# DEPRECATED ldgrsDeploymentGroups "Use generic-lens or generic-optics with 'deploymentGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgrsResponseStatus :: Lens.Lens' ListDeploymentGroupsResponse Lude.Int
ldgrsResponseStatus = Lens.lens (responseStatus :: ListDeploymentGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeploymentGroupsResponse)
{-# DEPRECATED ldgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
