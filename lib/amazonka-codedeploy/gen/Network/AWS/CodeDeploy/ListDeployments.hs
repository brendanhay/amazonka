{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployments in a deployment group for an application registered with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeployments
  ( -- * Creating a request
    ListDeployments (..),
    mkListDeployments,

    -- ** Request lenses
    ldCreateTimeRange,
    ldNextToken,
    ldIncludeOnlyStatuses,
    ldApplicationName,
    ldExternalId,
    ldDeploymentGroupName,

    -- * Destructuring the response
    ListDeploymentsResponse (..),
    mkListDeploymentsResponse,

    -- ** Response lenses
    ldrsNextToken,
    ldrsDeployments,
    ldrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListDeployments@ operation.
--
-- /See:/ 'mkListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { createTimeRange ::
      Lude.Maybe TimeRange,
    nextToken :: Lude.Maybe Lude.Text,
    includeOnlyStatuses :: Lude.Maybe [DeploymentStatus],
    applicationName :: Lude.Maybe Lude.Text,
    externalId :: Lude.Maybe Lude.Text,
    deploymentGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeployments' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
-- * 'createTimeRange' - A time range (start and end) for returning a subset of the list of deployments.
-- * 'deploymentGroupName' - The name of a deployment group for the specified application.
-- * 'externalId' - The unique ID of an external resource for returning deployments linked to the external resource.
-- * 'includeOnlyStatuses' - A subset of deployments to list by status:
--
--
--     * @Created@ : Include created deployments in the resulting list.
--
--
--     * @Queued@ : Include queued deployments in the resulting list.
--
--
--     * @In Progress@ : Include in-progress deployments in the resulting list.
--
--
--     * @Succeeded@ : Include successful deployments in the resulting list.
--
--
--     * @Failed@ : Include failed deployments in the resulting list.
--
--
--     * @Stopped@ : Include stopped deployments in the resulting list.
--
--
-- * 'nextToken' - An identifier returned from the previous list deployments call. It can be used to return the next set of deployments in the list.
mkListDeployments ::
  ListDeployments
mkListDeployments =
  ListDeployments'
    { createTimeRange = Lude.Nothing,
      nextToken = Lude.Nothing,
      includeOnlyStatuses = Lude.Nothing,
      applicationName = Lude.Nothing,
      externalId = Lude.Nothing,
      deploymentGroupName = Lude.Nothing
    }

-- | A time range (start and end) for returning a subset of the list of deployments.
--
-- /Note:/ Consider using 'createTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCreateTimeRange :: Lens.Lens' ListDeployments (Lude.Maybe TimeRange)
ldCreateTimeRange = Lens.lens (createTimeRange :: ListDeployments -> Lude.Maybe TimeRange) (\s a -> s {createTimeRange = a} :: ListDeployments)
{-# DEPRECATED ldCreateTimeRange "Use generic-lens or generic-optics with 'createTimeRange' instead." #-}

-- | An identifier returned from the previous list deployments call. It can be used to return the next set of deployments in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDeployments (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDeployments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeployments)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A subset of deployments to list by status:
--
--
--     * @Created@ : Include created deployments in the resulting list.
--
--
--     * @Queued@ : Include queued deployments in the resulting list.
--
--
--     * @In Progress@ : Include in-progress deployments in the resulting list.
--
--
--     * @Succeeded@ : Include successful deployments in the resulting list.
--
--
--     * @Failed@ : Include failed deployments in the resulting list.
--
--
--     * @Stopped@ : Include stopped deployments in the resulting list.
--
--
--
-- /Note:/ Consider using 'includeOnlyStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIncludeOnlyStatuses :: Lens.Lens' ListDeployments (Lude.Maybe [DeploymentStatus])
ldIncludeOnlyStatuses = Lens.lens (includeOnlyStatuses :: ListDeployments -> Lude.Maybe [DeploymentStatus]) (\s a -> s {includeOnlyStatuses = a} :: ListDeployments)
{-# DEPRECATED ldIncludeOnlyStatuses "Use generic-lens or generic-optics with 'includeOnlyStatuses' instead." #-}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldApplicationName :: Lens.Lens' ListDeployments (Lude.Maybe Lude.Text)
ldApplicationName = Lens.lens (applicationName :: ListDeployments -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: ListDeployments)
{-# DEPRECATED ldApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The unique ID of an external resource for returning deployments linked to the external resource.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldExternalId :: Lens.Lens' ListDeployments (Lude.Maybe Lude.Text)
ldExternalId = Lens.lens (externalId :: ListDeployments -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: ListDeployments)
{-# DEPRECATED ldExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The name of a deployment group for the specified application.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDeploymentGroupName :: Lens.Lens' ListDeployments (Lude.Maybe Lude.Text)
ldDeploymentGroupName = Lens.lens (deploymentGroupName :: ListDeployments -> Lude.Maybe Lude.Text) (\s a -> s {deploymentGroupName = a} :: ListDeployments)
{-# DEPRECATED ldDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

instance Page.AWSPager ListDeployments where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDeployments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDeployments where
  type Rs ListDeployments = ListDeploymentsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeploymentsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "deployments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeployments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.ListDeployments" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDeployments where
  toJSON ListDeployments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("createTimeRange" Lude..=) Lude.<$> createTimeRange,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("includeOnlyStatuses" Lude..=) Lude.<$> includeOnlyStatuses,
            ("applicationName" Lude..=) Lude.<$> applicationName,
            ("externalId" Lude..=) Lude.<$> externalId,
            ("deploymentGroupName" Lude..=) Lude.<$> deploymentGroupName
          ]
      )

instance Lude.ToPath ListDeployments where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDeployments where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListDeployments@ operation.
--
-- /See:/ 'mkListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    deployments :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListDeploymentsResponse' with the minimum fields required to make a request.
--
-- * 'deployments' - A list of deployment IDs.
-- * 'nextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployments call to return the next set of deployments in the list.
-- * 'responseStatus' - The response status code.
mkListDeploymentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeploymentsResponse
mkListDeploymentsResponse pResponseStatus_ =
  ListDeploymentsResponse'
    { nextToken = Lude.Nothing,
      deployments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployments call to return the next set of deployments in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDeploymentsResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDeploymentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeploymentsResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of deployment IDs.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDeployments :: Lens.Lens' ListDeploymentsResponse (Lude.Maybe [Lude.Text])
ldrsDeployments = Lens.lens (deployments :: ListDeploymentsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {deployments = a} :: ListDeploymentsResponse)
{-# DEPRECATED ldrsDeployments "Use generic-lens or generic-optics with 'deployments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDeploymentsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDeploymentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeploymentsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
