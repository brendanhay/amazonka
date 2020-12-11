{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployments. The maximum number of deployments that can be returned is 25.
module Network.AWS.CodeDeploy.BatchGetDeployments
  ( -- * Creating a request
    BatchGetDeployments (..),
    mkBatchGetDeployments,

    -- ** Request lenses
    bgdDeploymentIds,

    -- * Destructuring the response
    BatchGetDeploymentsResponse (..),
    mkBatchGetDeploymentsResponse,

    -- ** Response lenses
    bgdrsDeploymentsInfo,
    bgdrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @BatchGetDeployments@ operation.
--
-- /See:/ 'mkBatchGetDeployments' smart constructor.
newtype BatchGetDeployments = BatchGetDeployments'
  { deploymentIds ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetDeployments' with the minimum fields required to make a request.
--
-- * 'deploymentIds' - A list of deployment IDs, separated by spaces. The maximum number of deployment IDs you can specify is 25.
mkBatchGetDeployments ::
  BatchGetDeployments
mkBatchGetDeployments =
  BatchGetDeployments' {deploymentIds = Lude.mempty}

-- | A list of deployment IDs, separated by spaces. The maximum number of deployment IDs you can specify is 25.
--
-- /Note:/ Consider using 'deploymentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdDeploymentIds :: Lens.Lens' BatchGetDeployments [Lude.Text]
bgdDeploymentIds = Lens.lens (deploymentIds :: BatchGetDeployments -> [Lude.Text]) (\s a -> s {deploymentIds = a} :: BatchGetDeployments)
{-# DEPRECATED bgdDeploymentIds "Use generic-lens or generic-optics with 'deploymentIds' instead." #-}

instance Lude.AWSRequest BatchGetDeployments where
  type Rs BatchGetDeployments = BatchGetDeploymentsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetDeploymentsResponse'
            Lude.<$> (x Lude..?> "deploymentsInfo" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetDeployments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.BatchGetDeployments" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetDeployments where
  toJSON BatchGetDeployments' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("deploymentIds" Lude..= deploymentIds)]
      )

instance Lude.ToPath BatchGetDeployments where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetDeployments where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @BatchGetDeployments@ operation.
--
-- /See:/ 'mkBatchGetDeploymentsResponse' smart constructor.
data BatchGetDeploymentsResponse = BatchGetDeploymentsResponse'
  { deploymentsInfo ::
      Lude.Maybe [DeploymentInfo],
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

-- | Creates a value of 'BatchGetDeploymentsResponse' with the minimum fields required to make a request.
--
-- * 'deploymentsInfo' - Information about the deployments.
-- * 'responseStatus' - The response status code.
mkBatchGetDeploymentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetDeploymentsResponse
mkBatchGetDeploymentsResponse pResponseStatus_ =
  BatchGetDeploymentsResponse'
    { deploymentsInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deployments.
--
-- /Note:/ Consider using 'deploymentsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdrsDeploymentsInfo :: Lens.Lens' BatchGetDeploymentsResponse (Lude.Maybe [DeploymentInfo])
bgdrsDeploymentsInfo = Lens.lens (deploymentsInfo :: BatchGetDeploymentsResponse -> Lude.Maybe [DeploymentInfo]) (\s a -> s {deploymentsInfo = a} :: BatchGetDeploymentsResponse)
{-# DEPRECATED bgdrsDeploymentsInfo "Use generic-lens or generic-optics with 'deploymentsInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdrsResponseStatus :: Lens.Lens' BatchGetDeploymentsResponse Lude.Int
bgdrsResponseStatus = Lens.lens (responseStatus :: BatchGetDeploymentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetDeploymentsResponse)
{-# DEPRECATED bgdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
