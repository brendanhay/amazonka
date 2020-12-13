{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeploymentConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment configurations with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentConfigs
  ( -- * Creating a request
    ListDeploymentConfigs (..),
    mkListDeploymentConfigs,

    -- ** Request lenses
    ldcNextToken,

    -- * Destructuring the response
    ListDeploymentConfigsResponse (..),
    mkListDeploymentConfigsResponse,

    -- ** Response lenses
    ldcrsNextToken,
    ldcrsDeploymentConfigsList,
    ldcrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListDeploymentConfigs@ operation.
--
-- /See:/ 'mkListDeploymentConfigs' smart constructor.
newtype ListDeploymentConfigs = ListDeploymentConfigs'
  { -- | An identifier returned from the previous @ListDeploymentConfigs@ call. It can be used to return the next set of deployment configurations in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeploymentConfigs' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier returned from the previous @ListDeploymentConfigs@ call. It can be used to return the next set of deployment configurations in the list.
mkListDeploymentConfigs ::
  ListDeploymentConfigs
mkListDeploymentConfigs =
  ListDeploymentConfigs' {nextToken = Lude.Nothing}

-- | An identifier returned from the previous @ListDeploymentConfigs@ call. It can be used to return the next set of deployment configurations in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcNextToken :: Lens.Lens' ListDeploymentConfigs (Lude.Maybe Lude.Text)
ldcNextToken = Lens.lens (nextToken :: ListDeploymentConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeploymentConfigs)
{-# DEPRECATED ldcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListDeploymentConfigs where
  page rq rs
    | Page.stop (rs Lens.^. ldcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldcrsDeploymentConfigsList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldcNextToken Lens..~ rs Lens.^. ldcrsNextToken

instance Lude.AWSRequest ListDeploymentConfigs where
  type Rs ListDeploymentConfigs = ListDeploymentConfigsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeploymentConfigsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "deploymentConfigsList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeploymentConfigs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.ListDeploymentConfigs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDeploymentConfigs where
  toJSON ListDeploymentConfigs' {..} =
    Lude.object
      (Lude.catMaybes [("nextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListDeploymentConfigs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDeploymentConfigs where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListDeploymentConfigs@ operation.
--
-- /See:/ 'mkListDeploymentConfigsResponse' smart constructor.
data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse'
  { -- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment configurations call to return the next set of deployment configurations in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of deployment configurations, including built-in configurations such as @CodeDeployDefault.OneAtATime@ .
    deploymentConfigsList :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeploymentConfigsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment configurations call to return the next set of deployment configurations in the list.
-- * 'deploymentConfigsList' - A list of deployment configurations, including built-in configurations such as @CodeDeployDefault.OneAtATime@ .
-- * 'responseStatus' - The response status code.
mkListDeploymentConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeploymentConfigsResponse
mkListDeploymentConfigsResponse pResponseStatus_ =
  ListDeploymentConfigsResponse'
    { nextToken = Lude.Nothing,
      deploymentConfigsList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment configurations call to return the next set of deployment configurations in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsNextToken :: Lens.Lens' ListDeploymentConfigsResponse (Lude.Maybe Lude.Text)
ldcrsNextToken = Lens.lens (nextToken :: ListDeploymentConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeploymentConfigsResponse)
{-# DEPRECATED ldcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of deployment configurations, including built-in configurations such as @CodeDeployDefault.OneAtATime@ .
--
-- /Note:/ Consider using 'deploymentConfigsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsDeploymentConfigsList :: Lens.Lens' ListDeploymentConfigsResponse (Lude.Maybe [Lude.Text])
ldcrsDeploymentConfigsList = Lens.lens (deploymentConfigsList :: ListDeploymentConfigsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {deploymentConfigsList = a} :: ListDeploymentConfigsResponse)
{-# DEPRECATED ldcrsDeploymentConfigsList "Use generic-lens or generic-optics with 'deploymentConfigsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsResponseStatus :: Lens.Lens' ListDeploymentConfigsResponse Lude.Int
ldcrsResponseStatus = Lens.lens (responseStatus :: ListDeploymentConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeploymentConfigsResponse)
{-# DEPRECATED ldcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
